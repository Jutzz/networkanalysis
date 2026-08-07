#Setup----
library(tidyverse)
library(sf)
library(here)
library(tidytransit)
library(gtfstools)
library(timeDate)
library(httr2)
library(jsonlite)
library(zoo)
library(fst)
#Read helper functions
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)
#DHID/stop_id related functions: Checking if something is a valid DHID
#and creating a parent_id based on the stop_id.

is_ifopt <- function(stop_id) {
  grepl("^de:[^:]+:[^:]+", stop_id)
}

get_parent_dhid <- function(stop_id) {
  sub("^((de:[^:]+:[^:]+)).*$", "\\1", stop_id)
}
#Set route_type ranking: train, tram, bus, everything else.
route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700", "704"), 
  c("4","5","715", "1000")
)
#Create lookup as named integer.
route_rank_lookup <- setNames(
  rep(seq_along(route_type_ranks), lengths(route_type_ranks)),
  unlist(route_type_ranks)
)

get_route_rank <- function(rt) {
  unname(route_rank_lookup[as.character(rt)])
}
#Table for Bedienungsqualität based on stop_type and frequency.
#Value 7 is a placeholder for "everything outside the Steckbrief table."
quality_lookup <- tribble(
  ~stop_type, ~freq_class, ~Bedienungsqualität,
  3, 2, 6,
  3, 3, 5,
  3, 4, 4,
  3, 5, 3,
  3, 6, 2,
  2, 2, 5,
  2, 3, 4,
  2, 4, 3,
  2, 5, 2,
  2, 6, 1,
  1, 2, 4,
  1, 3, 3,
  1, 4, 2,
  1, 5, 1,
  1, 6, 1,
  0, 1, 7,
  1, 1, 7,
  2, 1, 7,
  3, 1, 7,
  4, 1, 7,
  4, 2, 7,
  4, 3, 7,
  4, 4, 7,
  4, 5, 7,
  4, 6, 7
)
#Date Input----
#Change feed date and area name to used feed version. (filename set in de_gtfs_cleaning).
#Select "weekday" or "normday".
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"
method <- "weekday"

#Read pre-filtered GTFS-Feed
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_", feed_date, "_", area_name,".zip"))
zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

timestamp <- format(Sys.Date(), "%Y-%m-%d")

zhv_lookup  <- zhv %>%
  select(Name, DHID)  %>%
  st_drop_geometry()

#Joining ZHV-DHIDs onto gtfs_stops to give Stops without a IFOPT-based stop_id
#that have an entry in the ZHV nontheless a valid IFOPT/DHID.
stops <- gtfs_feed$stops %>%
  left_join(zhv_lookup, by = c("stop_name" = "Name"))

#Create a cleaned stops table with a grouping_id which is:
#The parent_station from the GTFS, if that isnt present the first three keys
#from the stop_id (de:municipality_code:stop_code),
#if the stop_id is not in IFOPT format the DHID value from the ZHV
#and else the stop_id.
stops2 <- stops %>%
  mutate(
    grouping_id = case_when(
      !is.na(parent_station) & parent_station != "" ~ parent_station,
      is_ifopt(stop_id) ~ get_parent_dhid(stop_id),
      !is.na(DHID) ~ DHID,
      TRUE ~ stop_id
    )
  )
#Get dates of representative norm- and weekdays from checking in find_valid_dates.R
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")
nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
#Choose number of dates based on method set above.
ifelse(method == "weekday",
       date_select <- nonholiday_weekdays_fullservice,
       date_select <- nonholiday_normdays_fullservice)

#Build a full schedule (all stops on all days from gtfs_feed tables.)
filtered_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% date_select)

filtered_trips_dates <- gtfs_feed$trips %>%
  dplyr::select(trip_id, service_id, route_id) %>%
  left_join(gtfs_feed$routes %>% select(route_id, route_type)) %>%
  inner_join(filtered_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201", ""))  %>%
  dplyr::select(trip_id, date, route_type)

filtered_stop_times_dates <- gtfs_feed$stop_times %>%
  filter(pickup_type == 0) %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  filter(trip_id %in% filtered_trips_dates$trip_id) %>%
  left_join(filtered_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops2, by = "stop_id")  %>%
  mutate(route_rank = route_rank_lookup[as.character(route_type)])

rm(gtfs_feed)
rm(filtered_trips_dates)
gc()

#Count departures between 08:00 and 18:00 for all week-/normdays, calculate mean.
#Assign stop type and Bedienungsqualität based on Steckbrief table.
#Join with zhv as modified in osm_extract.R for geodata.
departure_counts_daily <- filtered_stop_times_dates %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time < hms("18:00:00")
  ) %>%
  group_by(date, grouping_id) %>%
  summarise(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    date,
    grouping_id,
    fill = list(departures = 0)
  ) %>%
  mutate(
    departures_per_hour = departures / 10,
    stop_type = ifelse(departures == 0, 0, stop_type)
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
    departures_per_hour,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(1,3,2,Name,4,5,22,23,21) %>%
  mutate(weekday = weekdays.Date(date)) %>%
  rename("stop_id" = grouping_id)

variability_daily <- st_drop_geometry(departure_counts_daily) %>%
  arrange(stop_id, date) %>%
  group_by(stop_id, Name) %>%
  mutate(
    diff = abs(departures_per_hour - lag(departures_per_hour))
  ) %>%
  summarise(
    mean_departures = mean(departures_per_hour, na.rm = TRUE),
    median_departures = median(departures_per_hour, na.rm = TRUE),
    min_departures = min(departures_per_hour, na.rm = TRUE),
    max_departures = max(departures_per_hour, na.rm = TRUE),
    stop_type_mean = max(stop_type),
    stop_type_median = floor(median(stop_type)),
    sum_abs_diff = sum(diff, na.rm = TRUE),
    mean_abs_diff = mean(diff, na.rm = TRUE),
    pct_variation = 100 * mean_abs_diff / mean_departures,
    min_quality = min(Bedienungsqualität, na.rm = TRUE),
    max_quality = max(Bedienungsqualität, na.rm = TRUE),
    quality_range = max_quality - min_quality,
    n_changes = sum(
      Bedienungsqualität != lag(Bedienungsqualität),
      na.rm = TRUE
    ),
    days_observed = n(),
    modal_quality = as.numeric(
      names(which.max(table(Bedienungsqualität)))
    ),
    pct_days_modal_quality =
      100 * max(table(Bedienungsqualität)) / n(),
    .groups = "drop"
  ) %>%  mutate(freq_class_mean = findInterval(
    mean_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  ) %>%  mutate(freq_class_median = findInterval(
    median_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  )%>%
  left_join(quality_lookup, by = join_by("stop_type_mean" == "stop_type", "freq_class_mean" == "freq_class")) %>%
    rename("bq_mean" = Bedienungsqualität) %>%
    left_join(quality_lookup, by = join_by("stop_type_median" == "stop_type", "freq_class_median" == "freq_class")) %>%
    rename("bq_median" = Bedienungsqualität) %>%
  left_join(zhv %>% select(DHID, Name, MunicipalityCode, Municipality, geom), by = join_by("stop_id" == "DHID" , Name)) %>%
  relocate(Name,
          stop_id,
          Municipality,
          bq_mean,
          bq_median,
          mean_departures,
          freq_class_mean,
          median_departures,
          freq_class_median,
          min_departures,
          max_departures,
          stop_type_mean,
          stop_type_median,
          sum_abs_diff,
          mean_abs_diff,
          pct_variation,
          min_quality,
          max_quality,
          quality_range,
          n_changes,
          days_observed,
          modal_quality,
          pct_days_modal_quality,
          MunicipalityCode)

#Write to geopackage. 
st_write(st_as_sf(departure_counts_daily), "geodata/Bedienungsqualität.gpkg", paste("daily", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)

st_write(variability_daily %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste("var_daily", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)

rm(departure_counts_daily, variability_daily)
gc()


#Also calculate hourly counts for additional analysis or later use.
departure_counts_hourly <- filtered_stop_times_dates %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time < hms("18:00:00")
  ) %>%
  mutate(hour = hour(departure_time)) %>%
  group_by(date, hour, grouping_id) %>%
  reframe(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE)
  ) %>%
  tidyr::complete(
    date,
    grouping_id,
    hour,
    fill = list(departures = 0)
  ) %>%
  mutate(
    departures_per_hour = departures
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
    departures_per_hour,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  rename("stop_id" = grouping_id)

variability_hourly <- st_drop_geometry(departure_counts_hourly) %>%
  arrange(stop_id, date, hour) %>%
  group_by(stop_id, Name) %>%
  mutate(
    diff = abs(departures_per_hour - lag(departures_per_hour))
  ) %>%
  summarise(
    mean_departures = mean(departures_per_hour, na.rm = TRUE),
    median_departures = median(departures_per_hour, na.rm = TRUE),
    min_departures = min(departures_per_hour, na.rm = TRUE),
    max_departures = max(departures_per_hour, na.rm = TRUE),
    stop_type_mean = max(stop_type),
    stop_type_median = floor(median(stop_type)),
    sum_abs_diff = sum(diff, na.rm = TRUE),
    mean_abs_diff = mean(diff, na.rm = TRUE),
    pct_variation = 100 * mean_abs_diff / mean_departures,
    min_quality = min(Bedienungsqualität, na.rm = TRUE),
    max_quality = max(Bedienungsqualität, na.rm = TRUE),
    quality_range = max_quality - min_quality,
    n_changes = sum(
      Bedienungsqualität != lag(Bedienungsqualität),
      na.rm = TRUE
    ),
    hours_observed = n(),
    modal_quality = as.numeric(
      names(which.max(table(Bedienungsqualität)))
    ),
    pct_hours_modal_quality =
      100 * max(table(Bedienungsqualität)) / n(),
    .groups = "drop"
  ) %>%  mutate(freq_class_mean = findInterval(
    mean_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  ) %>%  mutate(freq_class_median = findInterval(
    median_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  )%>%
  left_join(quality_lookup, by = join_by("stop_type_mean" == "stop_type", "freq_class_mean" == "freq_class")) %>%
  rename("bq_mean" = Bedienungsqualität) %>%
  left_join(quality_lookup, by = join_by("stop_type_median" == "stop_type", "freq_class_median" == "freq_class")) %>%
  rename("bq_median" = Bedienungsqualität) %>%
  left_join(zhv %>% select(DHID, Name, MunicipalityCode, Municipality, geom), by = join_by("stop_id" == "DHID" , Name)) %>%
  relocate(Name,
           stop_id,
           Municipality,
           bq_mean,
           bq_median,
           mean_departures,
           freq_class_mean,
           median_departures,
           freq_class_median,
           min_departures,
           max_departures,
           stop_type_mean,
           stop_type_median,
           sum_abs_diff,
           mean_abs_diff,
           pct_variation,
           min_quality,
           max_quality,
           quality_range,
           n_changes,
           hours_observed,
           modal_quality,
           pct_hours_modal_quality,
           MunicipalityCode)  

departure_counts_hourly <- departure_counts_hourly %>%
  select(Name, stop_id, Municipality, date, hour, Bedienungsqualität, departures_per_hour, stop_type, MunicipalityCode, geom)

st_write(st_as_sf(departure_counts_hourly), "geodata/Bedienungsqualität.gpkg", paste("hourly", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)
#Write to geopackage. 
st_write(variability_hourly %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste("var_hourly", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)
#Creating a minimal version to write as fst for usage in 10_i2_analysis.
stops_fst <- departure_counts_hourly %>%
  st_drop_geometry() %>%
  select(date, hour, stop_id, departures_per_hour, Bedienungsqualität) 


write_fst(stops_fst, paste("output/hourly", feed_date, method, min(date_select), max(date_select), ".fst", sep = "_"))

rm(departure_counts_hourly)
gc()
