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
get_parent_dhid <- function(stop_id) {
  sub("^((de:[^:]+:[^:]+)).*$", "\\1", stop_id)
}

is_ifopt <- function(stop_id) {
  grepl("^de:[^:]+:[^:]+", stop_id)
}

route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700", "704"), 
  c("4","5","715")
)

route_rank_lookup <- setNames(
  rep(seq_along(route_type_ranks), lengths(route_type_ranks)),
  unlist(route_type_ranks)
)

get_route_rank <- function(rt) {
  unname(route_rank_lookup[as.character(rt)])
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

zhv_lookup  <- zhv %>%
  select(Name, DHID)  %>%
  st_drop_geometry()

stops <- gtfs_feed$stops %>%
  left_join(zhv_lookup, by = c("stop_name" = "Name"))

stops2 <- stops %>%
  mutate(
    grouping_id = case_when(
      !is.na(parent_station) & parent_station != "" ~ parent_station,
      is_ifopt(stop_id) ~ get_parent_dhid(stop_id),
      !is.na(DHID) ~ DHID,
      TRUE ~ stop_id
    )
  )
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
  left_join(gtfs_feed$routes %>% select(route_id, route_type)) %>%
  inner_join(filtered_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201", ""))  %>%
  select(trip_id, date, route_type)

filtered_stop_times_dates <- gtfs_feed$stop_times %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  filter(trip_id %in% filtered_trips_dates$trip_id) %>%
  left_join(filtered_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops2, by = "stop_id")  %>%
  mutate(route_rank = route_rank_lookup[as.character(route_type)])

# date_stoptimems <- filtered_stop_times_dates  %>%
#   filter(date == as.Date("2026-05-06"))

  

# filter_921 <- filtered_stop_times_dates %>%
#   filter(grepl("de:vrs:921", trip_id))

departure_counts <- filtered_stop_times_dates %>%
  filter(departure_time >= hms("08:00:00")&departure_time <= hms("18:00:00")) %>%
  group_by(grouping_id) %>%
  reframe(
    departures = n(),
    # Determine highest-ranking route type
    stop_type = min(route_rank, na.rm = TRUE)
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
  select(1,3,2,4,5,22,23,21)

st_write(departure_counts %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste(feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)

departure_counts_daily <- filtered_stop_times_dates %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  group_by(date, grouping_id) %>%
  reframe(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE)
  ) %>%
  mutate(
    departures_per_hour = departures / 10
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
          departures_per_hour,
          vec = c(0, 2, 4, 6, 12, 24),
          rightmost.closed = FALSE
    )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(1,3,2,4,5,22,23,21)

departure_counts_daily <- departure_counts_daily %>%
  mutate(weekday = weekdays.Date(date))

st_write(departure_counts_daily %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste("daily", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)



ggplot(departure_counts_daily, aes(x = date, y = departures_per_hour, colour = as.factor(Bedienungsqualität))) +
  geom_point()

bq_weekday <- st_read("geodata/Bedienungsqualität.gpkg", "20260518_weekday_2026-05-04_2026-06-26")

bq_normday <- st_read("geodata/Bedienungsqualität.gpkg", "20260518_normday_2026-05-05_2026-06-25")

bq_diff <- bq_weekday %>%
  left_join(st_drop_geometry(bq_normday), by = "grouping_id", suffix = c(".w", ".n")) %>%
  mutate(diff = departures_per_hour_w-departures_per_hour_n,
         bq_diff = Bedienungsqualität.w-Bedienungsqualität.n)

ggplot(st_drop_geometry(bq_normday) %>% filter(Bedienungsqualität == 5), aes(x = departures_per_hour, y = as.factor(Bedienungsqualität)))  +
  geom_point()

ggplot(bq_weekday, aes(x = departures_per_hour)) +
  geom_density()

bq_weekday %>%
  filter(departures_per_hour<100)%>%
  arrange(departures_per_hour) %>%
  mutate(idx = row_number()) %>%
  ggplot(aes(y = departures_per_hour, x = idx, color = as.factor(stop_type))) +
  geom_point(size = 1, alpha = 0.4) +
  labs(
    y = "Departures per hour",
    x = "Ordered stops",
    title = "Sorted stop frequency distribution"
  ) +
  theme_minimal() +
  facet_wrap(~ Bedienungsqualität)

departures_hour <- function(filter_hour, stop_times){
  filtered_stop_times_dates %>%
    mutate(hour = hour(departure_time)) %>%
    filter(hour == filter_hour)
}

variability <- bq_weekday %>%
  arrange(stop_id, date) %>%
  group_by(stop_id) %>%
  summarise(
    total_variation = sum(abs(diff(departures_per_hour)), na.rm = TRUE),
    .groups = "drop"
  )

one <- departures_hour(1)

departure_counts_hourly <- filtered_stop_times_dates %>%
  filter(str_detect(grouping_id, "^de:05315:")) %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(hour = hour(departure_time)) %>%
  group_by(date, hour, grouping_id) %>%
  reframe(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE)
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
  select(1,3,2,4,5,22,23,21)

departure_counts_hourlyy <- departure_counts_hourly %>%
  mutate(timestamp = as.POSIXct(
       paste(date, sprintf("%02d:00:00", hour)),
       format = "%Y-%m-%d %H:%M:%S",
       tz = "Europe/Berlin"))

dc_hbf <- departure_counts_hourlyy %>% filter(grouping_id == "de:05315:11201")

deps_hbf <- filtered_stop_times_dates %>% filter(grouping_id == "de:05315:11201") %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(hour = hour(departure_time)) %>%
  group_by(date, hour, grouping_id)


ggplot(departure_counts_hourlyy %>% filter(grouping_id == "de:05315:11212", hour < 18), aes(x = hour, y = departures)) +
  geom_point()
