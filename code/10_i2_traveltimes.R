options(java.parameters = "-Xmx20G")
library(tidyverse)
library(sf)
library(r5r)
library(here)
library(lubridate)
library(zoo)
library(arrow)
library(plotly)
library(fst)

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
#----Routing Data----
#Einlesen von Gitter und POI
zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)
#mutate(id = GITTER_ID_100m)

stops_table <- st_read(dsn = "output/Bedienungsqualität.gpkg", paste("variability_hourly", feed_date, method, min(date_select), max(date_select), sep = "_"))

#Erstellen von r5-lesbaren Start- und Zielorten. Nur bewohnte Gitterzellen als Startorte.
area <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln")
area_name <- "regbez"

poi_type <- "stops"

polygrid100 <- zensus_grid %>%
  filter(Einwohner > 0) %>%
  st_filter(st_buffer(st_transform(area, crs = st_crs(zensus_grid)), 1000), .predicate = st_intersects)

zensus_grid_df <- pointgrid_fun(polygrid100, id_col = "id") %>%
  filter(!is.na(id))

stops <- st_as_sf(stops_table, coords = c("Längengrad", "Breitengrad"), crs = 4326) %>%
  st_filter(st_buffer(st_transform(area, crs = st_crs(stops_table)), 1000), .predicate = st_intersects)

pois_df <- pois_fun(pois = get(poi_type), id_col = "stop_id") %>%
  filter(!is.nan(lat))

#----Setup and Functions----
#Reisezeitmatrix
Erreichbarkeit <- function(origins, destinations) {
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    walk_speed = 3,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 1L,  # Time window in minutes for departures
    percentiles = 1L,
    
    progress = FALSE
  )
}

#----Analysis Start----

#R5 Setup
r5_network <- build_network(here("r5core_2026-05-18/"), verbose = FALSE, overwrite = FALSE)

departure_date <- date_select[1]
#Parameter für Erreichbarkeitsanalyse
modes <- c("WALK")
modes_filename <- paste(modes, collapse = "")
max_walk_time <- 26
departure <- as.POSIXct(paste0(departure_date, " 09:00:00"))
max_trip_duration <- 26

#Travel Time Analysis
#This only has to be done once; Walk Times to stops wont change over time
#(except for single cases where walking infrastructure changes, so the osm data
#used for the core should be updated and this be rerun if needed/at a set
#interval).
ttm <- Erreichbarkeit(origins = pois_df, destinations = zensus_grid_df)

#Write for usage in i2_analysis
write.csv2(ttm, file = "output/walk_20min_zensus_stops.csv")
stop_r5() 
gc()
# ttm <- read_csv2("output/walk_20min_zensus_stops.csv")