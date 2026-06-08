#Setup----
library(tidytransit)
library(gtfstools)
library(tidyverse)
library(timeDate)
library(here)
library(sf)
library(httr2)
library(jsonlite)
library(zoo)
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
  4, 1, 7
)
#Date Input----
#Change feed date and area name to used feed version. (filename set in de_gtfs_cleaning).
#Select "weekday" or "normday".
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"
method <- "normday"

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
  select(trip_id, service_id, route_id) %>%
  left_join(gtfs_feed$routes %>% select(route_id, route_type)) %>%
  inner_join(filtered_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201", ""))  %>%
  select(trip_id, date, route_type)

filtered_stop_times_dates <- gtfs_feed$stop_times %>%
  filter(pickup_type == 0) %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  filter(trip_id %in% filtered_trips_dates$trip_id) %>%
  left_join(filtered_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops2, by = "stop_id")  %>%
  mutate(route_rank = route_rank_lookup[as.character(route_type)])

#Count departures between 08:00 and 18:00 for all week-/normdays, calculate mean.
#Assign stop type and Bedienungsqualität based on Steckbrief table.
#Join with zhv as modified in osm_extract.R for geodata.
departure_counts <- filtered_stop_times_dates %>%
  filter(departure_time >= hms("08:00:00")&departure_time <= hms("18:00:00")) %>%
  group_by(grouping_id) %>%
  summarise(
    departures = n(),
    # Determine highest-ranking route type
    stop_type = min(route_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    departures_per_day  = departures / length(date_select),
    departures_per_hour = departures_per_day / 10
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
    departures_per_hour,
    vec = c(0,2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by("stop_type", "freq_class")) %>%
  rename("stop_id" = grouping_id) %>%
  select(1,9,10,11,3,2,4,5,10,22,23,21)

#Write to geopackage. 
st_write(departure_counts %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste(feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)
