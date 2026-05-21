options(java.parameters = "-Xmx20G")
library(r5r)
library(tidytransit)
library(gtfstools)
library(dplyr)
library(lubridate)
library(timeDate)
library(stringr)
library(here)
library(sf)
library(ggplot2)

source("all_clear.R")
route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700"), 
  c("4","5","715")
)

holidays_nrw <- function(year) {
  year <- as.integer(year)
  
  c(
    # Fixed nationwide
    as.Date(NewYearsDay(year)),
    as.Date(LaborDay(year)),
    as.Date(DEGermanUnity(year)),
    as.Date(ChristmasDay(year)),
    as.Date(BoxingDay(year)),
    as.Date(AllSaints(year)),  # All Saints (NRW)
    as.Date(GoodFriday(year)),
    as.Date(EasterMonday(year)),
    as.Date(Ascension(year)),
    as.Date(PentecostMonday(year)),
    as.Date(CorpusChristi(year))  # NRW-specific
  )
}

school_holidays_nrw_2026 <- c(
  seq.Date(as.Date("2026-03-30"), as.Date("2026-04-11"), by = "day"),
  seq.Date(as.Date("2026-07-20"), as.Date("2026-09-01"), by = "day"),
  seq.Date(as.Date("2026-10-17"), as.Date("2026-10-31"), by = "day"),
  seq.Date(as.Date("2026-12-23"), as.Date("2027-01-06"), by = "day"))

get_route_rank <- function(rt) {
  which(vapply(route_type_ranks, function(grp) rt %in% grp, logical(1)))
}

#gtfs handling in de_gtfs_cleaning

gtfs_feed <- tidytransit::read_gtfs(here("feeds/de_gtfs_05-26_nrw.zip"))

timestamp <- format(Sys.Date(), "%m-%y")

stops <- gtfs_feed$stops

stop_times <- gtfs_feed$stop_times

trips <- gtfs_feed$trips

routes <- gtfs_feed$routes

holidays <- holidays_nrw(2026)

#Use all weekdays present in feed
weekdays <- {
  d <- unique(gtfs_feed$.$dates_services$date)
  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
}

#Manually choose a selection of days present in the feed
weekdays <- {
  d <- seq.Date(as.Date("2026-04-20"), as.Date("2026-04-24"), by = "day")
  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
}


weekday_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% weekdays) %>%
  filter(!date %in% holidays) %>%
  filter(!date %in% school_holidays_nrw_2026)

weekday_trips_dates <- trips %>%
  select(trip_id, service_id, route_id) %>%
  left_join(routes %>% select(route_id, route_type)) %>%
  inner_join(weekday_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201"))

weekday_stop_times_dates <- stop_times %>%
  filter(trip_id %in% weekday_trips_dates$trip_id) %>%
  filter(departure_time >= hms("08:00:00")&arrival_time <= hms("18:00:00")) %>%
  select(1:4) %>%
  left_join(weekday_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops, by = "stop_id") %>%
  mutate(NVBW_HST_DHID = str_extract(stop_id, "^[^:]*:[^:]*:[^:]*"))

# filter_921 <- weekday_stop_times_dates %>%
#   filter(grepl("de:vrs:921", trip_id))

zhv <- st_read(here("geodata/poi.gpkg"), "zhv_20260427")

departure_counts <- weekday_stop_times_dates %>%
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
    departures_per_day  = departures / length(weekdays),
    departures_per_hour = departures_per_day / 10
  ) %>%
  left_join(zhv, by = join_by(NVBW_HST_DHID == DHID)) %>%
  mutate(
    stop_type = sapply(highest_rank_route_type, get_route_rank)
  ) %>%
  select(1:5, 9:11, 22, 21) %>%
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
    departures_per_day  = departures / length(weekdays),
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


st_write(departure_counts %>%
           filter(!is.na(geom)), here("geodata/poi.gpkg"), paste0(timestamp, "_stop_frequencies_de_gtfs_noholidays"), append = FALSE)

st_write(departure_counts_indiviual %>%
           filter(!is.na(geom)), here("geodata/poi.gpkg"), paste0(timestamp, "_stop_frequencies_de_gtfs_noholidays_individual"), append = FALSE)


ggplot(departure_counts, aes(x = departures_per_hour)) +
  geom_histogram(binwidth = 1)
