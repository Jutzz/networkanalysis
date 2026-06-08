options(java.parameters = "-Xmx20G")
library(tidyverse)
library(sf)
library(r5r)
library(here)
library(lubridate)
library(zoo)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

feed_date <- "20260518"

method <- "weekday"
method_bq <- "median"

#Get dates of representative norm- and weekdays from checking in find_valid_dates.R
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")
nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
#Choose number of dates based on method set above.
ifelse(method == "weekday",
       date_select <- nonholiday_weekdays_fullservice,
       date_select <- nonholiday_normdays_fullservice)

#TODO: Make usable for any area: import full zensus and stops data, filter by generic area (limited stops and dests) 
mapping_matrix <- read_csv2(here("code/erschließung_mat_long.csv"))

stops_table <- st_read(here("geodata/Bedienungsqualität.gpkg"), paste(feed_date, method, min(date_select), max(date_select), sep = "_")) %>%
  mutate(stop_type = case_when(
    stop_type == 1 ~ "train",
    stop_type == 2 ~ "tram",
    stop_type == 3 ~ "bus",
    stop_type == 4 ~ "other",
    TRUE ~ NA_character_ # Handles any other values
  )) %>%
  filter(!is.na(Bedienungsqualität))

stops_id <- st_drop_geometry(stops_table) %>%
  mutate(id = as.character(NVBW_HST_DHID))

#----stops data----
#Einlesen von Gitter und POI
zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)
#mutate(id = GITTER_ID_100m)

#Erstellen von r5-lesbaren Start- und Zielorten. Nur bewohnte Gitterzellen als Startorte.
area <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln")
area_name <- "regbez"

poi_type <- "stops"

polygrid100 <- zensus_grid %>%
  filter(Einwohner > 0) %>%
  st_filter(st_buffer(st_transform(area, crs = st_crs(zensus_grid)), 10000), .predicate = st_intersects)

zensus_grid_df <- pointgrid_fun(polygrid100, id_col = "id") %>%
  filter(!is.na(id))

stops <- st_as_sf(stops_table, coords = c("Längengrad", "Breitengrad"), crs = 4326) %>%
  st_filter(st_buffer(st_transform(area, crs = st_crs(stops_table)), 10000), .predicate = st_intersects)

pois_df <- pois_fun(pois = get(poi_type), id_col = "NVBW_HST_DHID") %>%
  filter(!is.nan(lat))

#----Setup and Functions----
#Erreichbarkeitsanalyse ohne Filtern/Schreiben für die Verwendung mit i2_mapping (Auswahl des besten Erschließungswertes, nicht kürzeste oder beste Haltestelle)
Erreichbarkeit <- function(origins, destinations) {
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    walk_speed = 4,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 60,  # Time window in minutes for departures
    percentiles = 1,
    progress = FALSE
  )
}

#Mapping travel times and Bedienungsqualität to respective erschließung values
erschließung_map <- function(grid){
  grid %>%
    mutate(
      travel_time_cut = case_when(
        travel_time_p01 <= 5 ~ 5,
        travel_time_p01 <= 8 ~ 8,
        travel_time_p01 <= 11 ~ 11,
        travel_time_p01 <= 15 ~ 15,
        travel_time_p01 <= 19 ~ 19,
        TRUE ~ NA_real_
      )
    ) %>%
    left_join(mapping_matrix, by = c("Bedienungsqualität" = "Bedienungsqualität", "travel_time_cut" = "minutes")) %>%
    rename(Erschließungsqualität = indicator)
}

#Join mit Stops, errechnen der Erschließungsqualität, Auswahl der Station mit der besten Erschließungsqualität je Zelle, schreiben
##TODO: Clean up, develop strategy for filenames/metadata, make faster!
i2_mapping <- function(ttm, matrix, departure){
  
  mapping <- matrix  
  
  ttm_with_quality <- ttm %>%
    left_join(stops_id %>% select(id, Bedienungsqualität), by = c("from_id" = "id")) %>%
    mutate(Bedienungsqualität = ifelse(is.na(Bedienungsqualität), Inf, Bedienungsqualität))
  
  eq <- erschließung_map(ttm_with_quality)
  
  best_connections <- eq %>%
    group_by(to_id) %>%
    arrange(Erschließungsqualität) %>%
    slice(1) %>%
    ungroup()
  
  grid_with_times <<- polygrid100 %>%
    left_join(best_connections %>%
                select(from_id, to_id, travel_time_p01, Erschließungsqualität), by = c("id" = "to_id")) %>%
    mutate(start_time = departure) %>%
    mutate(end_time = start_time + minutes(travel_time_p01)) %>%
    left_join(stops_id, by = c("from_id" = "id"))
  
  travel_times <<- grid_with_times %>%
    select(id, from_id, travel_time_p01, Einwohner, NVBW_HST_DHID, geom)
}

#Find median row to determine representative day for routing
get_median_row <- function(data, column) {
  median_val <- median(data[[column]], na.rm = TRUE)
  
  exact_match <- data %>% filter(.data[[column]] == median_val)
  
  if (nrow(exact_match) > 0) {
    return(exact_match)
  } else {
    return(data %>% slice(which.min(abs(.data[[column]] - median_val))))
  }
}

#----Analysis Start----

#R5 Setup
r5_network <- build_network(here("r5core_current"), verbose = FALSE, overwrite = FALSE)

#Analysis of transit availability to pick representative (week-)day
##TODO: Is this necessary here? All days should be the same for WALK routing.
##Find stats and/or function to find representative dates, to check for large jumps in availability (holidays, partial feeds ending) and for variability across hours.
availability <- check_transit_availability(r5_network, start_date = as.Date("2026-05-02"), end_date = as.Date("2026-07-19")) %>%
  mutate(weekday = weekdays(as.Date(date))) %>%
  mutate(weekday_n =  format(as.Date(date),"%w"))


availability_week <- availability %>%
  group_by(weekday) %>%
  mutate(avg_pct = mean(pct_active))%>%
  mutate(avg_services = mean(active_services))%>%
  #select(5:7) %>%
  distinct()

ggplot(availability, aes(x = date, y = pct_active)) +
  geom_line() +
  geom_line(aes(y=rollmean(pct_active, 5, na.pad=TRUE), color = "#ff0000"))

median_date <- get_median_row(availability %>%
                                filter(!weekday_n%in%c(0,6)), "pct_active")
departure_date <- median_date$date[1]

#Parameter für Erreichbarkeitsanalyse
modes <- c("WALK")
modes_filename <- paste(modes, collapse = "")
max_walk_time <- 20
departure <- as.POSIXct(paste0(departure_date, " 09:00:00"))
max_trip_duration <- 20

#Travel Time Analysis
##TODO: This technically only has to be done once; Walk Times to stops wont change over time (except for single cases where walking infrastructure changes, so the osm data used for the core should be updated and this be rerun if needed/at a set interval).
ttm <- Erreichbarkeit(origins = pois_df, destinations = zensus_grid_df)

write.csv2(ttm, file = "output/walk_20min_zensus_stops.csv")
#This is the part that actually changes Indikator 2 with differing strategies for stop frequency calculation. 
i2_mapping(ttm, mapping_matrix, departure)

#Write out a travel times table and an accessibility table to geopackages.
st_write(travel_times, here("output/indikator_02.gpkg"), layer = paste0(freq_date, "_DEgtfs_zensus_", poi_type, "_walktime"), append = FALSE)
st_write(grid_with_times, here("results/indikator_02.gpkg"), layer = paste0(freq_date, "_i2_zensus_erschließungswerte_best_", area_name), append = FALSE)



#----Additional ttms for testing----
#stops <- st_as_sf(gtfs_feed$stops, coords = c("stop_lon", "stop_lat"), crs = st_crs(4326))
tts <- gtfs_feed %>% filter_feed_by_date("2026-04-07") %>%
  travel_times(stop_name = "Köln Hbf",
               time_range = c("14:00:00", "14:10:00"), stop_dist_check=FALSE, max_transfers = 3)

o <- pois_fun(stops %>%
                filter(NVBW_HST_DHID == "de:05515:46845"))
d <- pois_fun(stops) %>%
  filter(lat != "NaN")
grid <- st_read("geodata/grids.gpkg", layer = )
r5tts <- travel_time_matrix(r5_network, origins = pois_df[18078,], destinations = zensus_grid_df, mode = c("TRANSIT", "WALK"), max_trip_duration = 60, max_rides = 5, departure_datetime = departure, verbose = TRUE)

tts2 <- tts %>%
  left_join(gtfs_feed$stops, by = join_by("to_stop_id" == "stop_id")) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = st_crs(4326)) %>%
  st_filter(st_transform(regbez, crs = st_crs(4326)), .predicate = st_intersects) %>%
  mutate(travel_time = travel_time/60) %>%
  filter(travel_time < 300)

r5tts2 <- r5tts %>%
  left_join(d, by = join_by("to_id" == "id"))

r5tts2 <- st_as_sf(r5tts2, coords = c("lon", "lat"), crs = st_crs(4326)) %>%
  st_filter(st_transform(regbez, crs = st_crs(4326)), .predicate = st_intersects)

st_write(r5tts2, "expandedttm_r5r2.geojson")
st_write(tts2, "ttm_tidytransit.geojson")

write_csv(r5tts, "../expanded_ttm_cgn.csv")

tr <- r5tts %>%
  mutate(trajectory = paste(from_id, to_id, sep = "_"))

summary_stats <- tr %>%
  group_by(to_id) %>%
  summarise(
    mean_tt = mean(total_time),
    sd_tt = sd(total_time),
    min_tt = min(total_time),
    max_tt = max(total_time),
    p10 = quantile(total_time, 0.1),
    p50 = quantile(total_time, 0.5),
    p90 = quantile(total_time, 0.9),
    range_tt = max_tt - min_tt,
    cv = sd_tt / mean_tt
  )

r5tts2 <- summary_stats %>%
  left_join(d, by = join_by("to_id" == "id"))


iso <- isochrone(r5_network, origins = o, mode = "CAR", cutoffs = c(0,5,15,30,60,90,120), departure_datetime = as.POSIXct(paste0("2026-04-27", " 17:00:00")), polygon_output = TRUE,)

plot(iso)

st_write(iso, "iso_car.geojson")

r5tts2 <- zensus_grid %>%
  left_join(r5tts, by = join_by("id" == "to_id"))

st_write(r5tts2, "gridttm_r5r2.geojson")

