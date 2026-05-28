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

#Change feed date and area name to used feed version (filename set in de_gtfs_cleaning).
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"

get_school_holidays <- function(country, subdivision, start_date, end_date, lang = "DE") {
  resp <- httr2::request("https://openholidaysapi.org/SchoolHolidays") |>
    httr2::req_url_query(
      countryIsoCode = country,
      validFrom = start_date,
      validTo = end_date,
      languageIsoCode = lang,
      subdivisionCode = subdivision
    ) |>
    httr2::req_perform()
  
  school_holidays <- fromJSON(rawToChar(resp$body))
}

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

#If openholidays api breaks, use this to set school holidays manually.
#school_holidays <- c(
#  seq.Date(as.Date("2026-03-30"), as.Date("2026-04-11"), by = "day"),
#  seq.Date(as.Date("2026-07-20"), as.Date("2026-09-01"), by = "day"),
#  seq.Date(as.Date("2026-10-17"), as.Date("2026-10-31"), by = "day"),
#  seq.Date(as.Date("2026-12-23"), as.Date("2027-01-06"), by = "day"))

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

holidays <- holidays_nrw(2026)

#Set up date lists of school holidays and public holidays for filtering stop time data.
school_holidays<- get_school_holidays(country = "DE",
                                            subdivision = "DE-NW",
                                            start_date = "2026-01-01",
                                            end_date = "2028-12-31") %>%
  select(2,3,5) %>%
  mutate(
    name = map_chr(name, ~ {
      x <- .x
      x$text[x$language == "DE"][1]
    })
  ) %>%
  mutate(
    startDate = as.Date(startDate),
    endDate = as.Date(endDate),
    date = map2(startDate, endDate, ~ seq(.x, .y, by = "day"))
  ) %>%
  unnest(date) %>%
  select(name, date)


#Read pre-filtered GTFS-Feed
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_", feed_date, "_", area_name,".zip"))

timestamp <- format(Sys.Date(), "%Y-%m-%d")

#Use all normalwerktage present in feed
normdays <- {
  d <- unique(gtfs_feed$.$dates_services$date)
  wd <- wday(d, week_start = 1)
  mo <- month(d)
  
  d[wd %in% c(2, 3, 4) & mo %in% c(4, 5, 6, 9)]
}
#Use all weekdays present in feed
weekdays <- {
  d <- unique(gtfs_feed$.$dates_services$date)
  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
}

#Manually choose a selection of days present in the feed
weekdays <- {
  d <- seq.Date(as.Date("2026-05-20"), as.Date("2026-05-21"), by = "day")
  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
}

nonholiday_weekdays <- weekdays[!weekdays %in% c(holidays,school_holidays$date)]

nonholiday_normdays <- normdays[!normdays %in% c(holidays,school_holidays$date)]


#set up separate tables of GTFS components for easier joining
stops <- gtfs_feed$stops

stop_times <- gtfs_feed$stop_times

trips <- gtfs_feed$trips

routes <- gtfs_feed$routes

#Analysis of transit availability to check for feed inconsistencies, representative stretches.
##Find stats and/or function to find representative dates, to check for large jumps in availability (holidays, partial feeds ending) and for variability across hours.

trip_calendar <- gtfs_feed$.$dates_services %>%
  inner_join(gtfs_feed$trips %>%
               select(service_id, trip_id, route_id),
             by = "service_id") %>%
  inner_join(gtfs_feed$routes %>%
               select(route_id, route_type, agency_id, route_short_name),
             by = "route_id") %>%
  filter(!route_type %in% c(102,101,201))

trips_per_day <- trip_calendar %>%
  group_by(date) %>%
  summarise(
    trips = n()
  ) %>%
  filter(date %in% nonholiday_normdays) %>%
  filter(date < as.Date("2026-06-29")) %>%
  arrange(date) %>%
  mutate(week_delta = trips-lag(trips, 5)) %>%
  mutate(rolling_avg = rollmean(trips, 5, na.pad = TRUE)) %>%
  mutate(rollingavg_delta = rolling_avg-lag(rolling_avg, 5)) %>%
  mutate(absolute = abs(rollingavg_delta))
  
x_min <- min(trips_per_day$date, na.rm = TRUE)
x_max <- max(trips_per_day$date, na.rm = TRUE)

plotholidays <- get_school_holidays(country = "DE",
                                    subdivision = "DE-NW",
                                    start_date = "2026-01-01",
                                    end_date = "2028-12-31") %>%
  select(2,3,5) %>%
  mutate(
    name = map_chr(name, ~ {
      x <- .x
      x$text[x$language == "DE"][1]
    })
  ) %>%
  mutate(
    startDate = as.Date(startDate),
    endDate = as.Date(endDate)) %>%
  filter(endDate >= x_min,
         startDate <= x_max) %>%
  mutate(
    startDate = pmax(startDate, x_min),
    endDate   = pmin(endDate, x_max) + 1
  )

ggplot(trips_per_day, aes(x = date, y = trips)) +
  geom_rect(
    data = plotholidays,
    inherit.aes = FALSE,
    aes(
      xmin = startDate,
      xmax = endDate,
      ymin = -Inf,
      ymax = Inf,
      fill = name
    ),
    alpha = 0.2
  ) +
  geom_point() +
  #geom_line(aes(y=rolling_avg, color = "gleitender Mittelwert (7 Tage)"), linewidth = 2) +
  labs(title = "Fahrten pro Tag im Jahresverlauf", subtitle = "auf Grundlage des DELFI-GTFS vom 18.05.2026 (nur Wochentage)", color = element_blank(), fill = NULL) +
  xlab("Datum") +
  ylab("Anzahl Fahrten") +
  scale_color_manual(values = c("gleitender Mittelwert (7 Tage)" = "red")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  theme(legend.position = "bottom")

#R5 Setup
#TODO: Not really needed here, as checking services is pretty unreliable. Clean separation between exploratory show of work (figures) and processing steps here.
#What happens reproducibly: Does an agency_id feed part stop at some point? If so, what proportion of daily trips does it have?
#Can a threshold be set for that or is it necessarily a manual decision?
r5_network <- build_network(here("r5core_2026-05-18"), verbose = FALSE, overwrite = FALSE)
#dates = nonholiday_weekdays#

availability <- check_transit_availability(r5_network, dates = unique(gtfs_feed$.$dates_services$date)) %>%
  mutate(weekday = weekdays(as.Date(date))) %>%
  mutate(weekday_n =  format(as.Date(date),"%w")) %>%
  filter(active_services > 0) %>%
  arrange(date) %>%
  mutate(week_delta = active_services-lag(active_services, 7)) %>%
  mutate(rolling_avg = rollmean(pct_active, 7, na.pad = TRUE)) %>%
  mutate(rollingavg_delta = rolling_avg-lag(rolling_avg, 7)) %>%
  mutate(absolute = abs(rollingavg_delta))

jumps <- availability %>%
  mutate(absolute = abs(rollingavg_delta)) %>%
  filter(absolute > 2*(sd(rollingavg_delta, na.rm = TRUE)))

availability_week <- availability %>%
  group_by(weekday) %>%
  mutate(avg_pct = mean(pct_active))%>%
  mutate(avg_services = mean(active_services))%>%
  mutate(sd_services = sd(active_services)) %>%
  mutate(variance_services = var(active_services)) %>%
  select(5, 11:14) %>%
  distinct()

ggplot(availability, aes(x = date, y = pct_active)) +
  geom_line() +
  geom_line(aes(y=rolling_avg, color = "gleitender Mittelwert (7 Tage)"), linewidth = 2) +
  labs(title = "Aktive services im Jahresverlauf", subtitle = "auf Grundlage des DELFI-GTFS vom 18.05.2026", color = element_blank(), fill = NULL) +
  xlab("Datum") +
  ylab("Anteil aktiver services") +
  scale_color_manual(values = c("gleitender Mittelwert (7 Tage)" = "red")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  theme(legend.position = "bottom")

ggplot(availability, aes(x = active_services)) +
  geom_dotplot(aes(fill = as.factor(month(date))), stackgroups = TRUE, binpositions = "all")

weekday_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% nonholiday_weekdays)

weekday_trips_dates <- trips %>%
  select(trip_id, service_id, route_id) %>%
  left_join(routes %>% select(route_id, route_type)) %>%
  inner_join(weekday_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201"))

weekday_stop_times_dates <- stop_times %>%
  filter(trip_id %in% weekday_trips_dates$trip_id) %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  left_join(weekday_trips_dates %>%
              select(trip_id, date, route_type), by = "trip_id") %>%
  left_join(stops, by = "stop_id") %>%
  mutate(NVBW_HST_DHID = str_extract(stop_id, "^[^:]*:[^:]*:[^:]*"))

# filter_921 <- weekday_stop_times_dates %>%
#   filter(grepl("de:vrs:921", trip_id))

zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

departure_counts <- weekday_stop_times_dates %>%
  filter(departure_time >= hms("08:00:00")&arrival_time <= hms("18:00:00")) %>%
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
    departures_per_day  = departures / length(nonholiday_weekdays),
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


st_write(departure_counts %>%
           filter(!is.na(geom)), here("geodata/poi.gpkg"), paste0(timestamp, "_stop_frequencies_de_gtfs_noholidays"), append = FALSE)

st_write(departure_counts_indiviual %>%
           filter(!is.na(geom)), here("geodata/poi.gpkg"), paste0(timestamp, "_stop_frequencies_de_gtfs_noholidays_individual"), append = FALSE)


ggplot(departure_counts, aes(x = departures_per_hour)) +
  geom_histogram(binwidth = 1)
