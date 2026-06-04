options(java.parameters = "-Xmx20G")
library(r5r)
library(tidytransit)
library(gtfstools)
library(tidyverse)
library(timeDate)
library(here)
library(sf)
library(httr2)
library(jsonlite)
library(zoo)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

#Set route_type ranking: train, tram, bus, everything else.
route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700", "704"), 
  c("4","5","715")
)

get_route_rank <- function(rt) {
  which(vapply(route_type_ranks, function(grp) rt %in% grp, logical(1)))
}

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
  1, 6, 1
)

#Change feed date and area name to used feed version (filename set in de_gtfs_cleaning).
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"

#Read pre-filtered GTFS-Feed
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_", feed_date, "_", area_name,".zip"))
zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

timestamp <- format(Sys.Date(), "%Y-%m-%d")

#set up separate tables of GTFS components for easier joining
stops <- gtfs_feed$stops

stop_times <- gtfs_feed$stop_times

trips <- gtfs_feed$trips

routes <- gtfs_feed$routes

#Get dates of representative norm- and weekdays from checking earlier.
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")

nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")

method <- "normday"

ifelse(method == "weekday",
         date_select <- nonholiday_weekdays_fullservice,
         date_select <- nonholiday_normdays_fullservice)


filtered_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% date_select)

filtered_trips_dates <- gtfs_feed$trips %>%
  select(trip_id, service_id, route_id) %>%
  left_join(routes %>% select(route_id, route_type)) %>%
  inner_join(filtered_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201"))

filtered_stop_times_dates <- gtfs_feed$stop_times %>%
  filter(trip_id %in% filtered_trips_dates$trip_id) %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  left_join(filtered_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops, by = "stop_id") %>%
  mutate(NVBW_HST_DHID = str_extract(stop_id, "^[^:]*:[^:]*:[^:]*"))

# filter_921 <- filtered_stop_times_dates %>%
#   filter(grepl("de:vrs:921", trip_id))

departure_counts <- filtered_stop_times_dates %>%
  filter(departure_time >= hms("08:00:00")&arrival_time <= hms("18:00:00")) %>%
  group_by(NVBW_HST_DHID) %>%
  reframe(
    departures = n(),
    # Determine highest-ranking route type
    highest_rank_route_type = {
      all_types <- unique(route_type)
      ranks <- sapply(all_types, get_route_rank)
      # pick the route type with the *lowest* numerical rank
      all_types[which.min(ranks)]
    }
  ) %>%
  mutate(
    departures_per_day  = departures / length(date_select),
    departures_per_hour = departures_per_day / 10
  ) %>%
  left_join(zhv, by = join_by(NVBW_HST_DHID == DHID)) %>%
  mutate(
    stop_type = sapply(highest_rank_route_type, get_route_rank),
    freq_class = findInterval(
      departures_per_hour,
      vec = c(0,2, 4, 6, 12, 24),
      rightmost.closed = FALSE
    )
    ) %>%
  select(1:5, 9:11, 22, 23, 21) %>%
  left_join(quality_lookup, by = join_by("stop_type", "freq_class"))

st_write(departure_counts %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste(feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)


departures_hour <- function(filter_hour, stop_times){
  weekday_stop_times_dates %>%
    mutate(hour = hour(departure_time)) %>%
    filter(hour == filter_hour)
}





departure_counts_hourly <- list()

for (i in 0:27) {
  departure_count <- departures_hour(i, weekday_stop_times_dates) %>%
    group_by(NVBW_HST_DHID) %>%
    summarise(
      departures = n(),
      # Determine highest-ranking route type
      highest_rank_route_type = {
        all_types <- unique(route_type)
        ranks <- sapply(all_types, get_route_rank)
        # pick the route type with the *lowest* numerical rank
        all_types[which.min(ranks)]
      },
      .groups = "drop"
    ) %>%
    mutate(
      departures_per_hour  = departures / length(nonholiday_weekdays)
    ) %>%
    left_join(zhv, by = join_by(NVBW_HST_DHID == DHID)) %>%
    mutate(
      stop_type = sapply(highest_rank_route_type, get_route_rank)
    ) %>%
    select(1:5, 9:11, 21, 20) %>%
    mutate(Bedienungsqualität = case_when(
      stop_type == 3 & departures_per_hour < 2  ~ NA_real_,
      stop_type == 3 & departures_per_hour < 4  ~ 6,
      stop_type == 3 & departures_per_hour < 6  ~ 5,
      stop_type == 3 & departures_per_hour < 12 ~ 4,
      stop_type == 3 & departures_per_hour < 24 ~ 3,
      stop_type == 3 & departures_per_hour >= 24 ~ 2,
      
      stop_type == 2 & departures_per_hour < 2  ~ NA_real_,
      stop_type == 2 & departures_per_hour < 4  ~ 5,
      stop_type == 2 & departures_per_hour < 6  ~ 4,
      stop_type == 2 & departures_per_hour < 12 ~ 3,
      stop_type == 2 & departures_per_hour < 24 ~ 2,
      stop_type == 2 & departures_per_hour >= 24 ~ 1,
      
      stop_type == 1 & departures_per_hour < 2  ~ NA_real_,
      stop_type == 1 & departures_per_hour < 4  ~ 4,
      stop_type == 1 & departures_per_hour < 6  ~ 3,
      stop_type == 1 & departures_per_hour < 12 ~ 2,
      stop_type == 1 & departures_per_hour < 24 ~ 1,
      stop_type == 1 & departures_per_hour >= 24 ~ 1,
      TRUE ~ NA_real_
    )
    ) %>%
    mutate(hour = i)
  
  departure_counts_hourly[[i+1]] <- departure_count
}

departure_counts_hourly <- rbind(departure_counts_hourly)

departure_counts_indiviual <- weekday_stop_times_dates %>%
  group_by(stop_id) %>%
  summarise(
    departures = n(),
    # Determine highest-ranking route type
    highest_rank_route_type = {
      all_types <- unique(route_type)
      ranks <- sapply(all_types, get_route_rank)
      # pick the route type with the *lowest* numerical rank
      all_types[which.min(ranks)]
    },
    .groups = "drop"
  ) %>%
  mutate(
    departures_per_day  = departures / length(nonholiday_weekdays),
    departures_per_hour = departures_per_day / 10
  ) %>%
  left_join(zhv, by = join_by(stop_id == DHID)) %>%
  mutate(
    stop_type = sapply(highest_rank_route_type, get_route_rank)
  ) %>%
  select(1:5, 9:11, 22, 21, "Parent") %>%
  mutate(NVBW_HST_DHID = str_extract(stop_id, "^[^:]*:[^:]*:[^:]*")) %>%
  mutate(Bedienungsqualität = case_when(
    stop_type == 3 & departures_per_hour < 2  ~ NA_real_,
    stop_type == 3 & departures_per_hour < 4  ~ 6,
    stop_type == 3 & departures_per_hour < 6  ~ 5,
    stop_type == 3 & departures_per_hour < 12 ~ 4,
    stop_type == 3 & departures_per_hour < 24 ~ 3,
    stop_type == 3 & departures_per_hour >= 24 ~ 2,
    
    stop_type == 2 & departures_per_hour < 2  ~ NA_real_,
    stop_type == 2 & departures_per_hour < 4  ~ 5,
    stop_type == 2 & departures_per_hour < 6  ~ 4,
    stop_type == 2 & departures_per_hour < 12 ~ 3,
    stop_type == 2 & departures_per_hour < 24 ~ 2,
    stop_type == 2 & departures_per_hour >= 24 ~ 1,
    
    stop_type == 1 & departures_per_hour < 2  ~ NA_real_,
    stop_type == 1 & departures_per_hour < 4  ~ 4,
    stop_type == 1 & departures_per_hour < 6  ~ 3,
    stop_type == 1 & departures_per_hour < 12 ~ 2,
    stop_type == 1 & departures_per_hour < 24 ~ 1,
    stop_type == 1 & departures_per_hour >= 24 ~ 1,
    TRUE ~ NA_real_
  ))

st_write(departure_counts_indiviual %>%
           filter(!is.na(geom)), here("geodata/poi.gpkg"), paste0(timestamp, "_stop_frequencies_de_gtfs_noholidays_individual"), append = FALSE)


ggplot(departure_counts, aes(x = departures_per_hour)) +
  geom_histogram(binwidth = 1)
