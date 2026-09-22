options(java.parameters = "-Xmx20G")
library(r5r)
library(fst)
library(tidyverse)
library(sf)
library(gpkg)
library(raster)
library(tidytransit)
library(here)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

#Erreichbarkeitsanalyse, Ergebnis gefiltert nach kürztester Reisezeit, hinzufügen von Start/Endzeit und Start/Zielortname, schreiben als gpkg-Layer
Erreichbarkeit <- function(origins, destinations, departure) {
  timestamp <- format(Sys.Date(), "%m-%y") 
  
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    max_rides = max_rides,
    walk_speed = 4,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 60,  # Time window in minutes for departures
    percentiles = c(1L, 99L),
    draws_per_minute = 1L,
    progress = TRUE
  )
  
  write_csv(ttm, paste0("output/i3_ttm_hourly/de_gtfs_zensus_", poi_type, "_", modes_filename, "_", max_trip_duration,"min_",date(departure), "_", hour(departure), "h.csv"))
  
  # shortest_travel_times <- ttm %>%
  #   group_by(to_id) %>%
  #   slice_min(travel_time_p01, with_ties = FALSE) %>%
  #   ungroup()
  # 
  # grid_with_times <- polygrid100 %>%
  #   filter(if ("Einwohner" %in% names(.)) Einwohner != 0 else TRUE) %>%
  #   left_join(shortest_travel_times %>%
  #               dplyr::select(from_id, to_id, travel_time_p01), by = c("id" = "to_id")) %>%
  #   mutate(start_time = as.POSIXct("2026-05-12 09:00:00")) %>%
  #   mutate(end_time = start_time + minutes(travel_time_p01)) %>%
  #   left_join(zentraleOrteid, by = c("from_id" = "id")) %>%
  #   #dplyr::select(!c("ART", "STAND", "KN7stellig")) %>%
  #   rename("GN_ziel" = GN, "core_ziel" = area_id)
  # 
  # st_write(grid_with_times, here("output/indikator_03.gpkg"), layer = paste0("de_gtfs_polygrid_100m_shortest_", poi_type, "_", modes_filename, "_", max_trip_duration,"min_",date(departure), "_", hour(departure), "h"), append = FALSE)
}

#Wie oben, ohne filtern nach kürzester
Erreichbarkeit_nofiltering <- function(origins, destinations, departure) {
  
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    max_rides = max_rides,
    walk_speed = 4,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 60,  # Time window in minutes for departures
    percentiles = c(1L,99L ),
    draws_per_minute = 1L,
    progress = TRUE
  )
  
  # grid_with_times <- polygrid100 %>%
  #   filter(if ("Einwohner" %in% names(.)) Einwohner != 0 else TRUE) %>%
  #   left_join(ttm %>%
  #               dplyr::select(from_id, to_id, travel_time_p01), by = c("id" = "to_id")) %>%
  #   mutate(start_time = as.POSIXct("2026-05-12 09:00:00")) %>%
  #   mutate(end_time = start_time + minutes(travel_time_p01)) %>%
  #   left_join(zentraleOrteid, by = c("from_id" = "id")) %>%
  #   #dplyr::select(!c("ART", "STAND", "KN7stellig")) %>%
  #   rename("GN_ziel" = GN, "core_ziel" = area_id)
  # 
  # st_write(grid_with_times, here("output/indikator_03.gpkg"), layer = paste0("de_gtfs_polygrid_100m_all_destinations_subset", "_", poi_type, "_", modes_filename, "_", max_trip_duration,"min"), append = FALSE)
  # return(ttm)
}

#Parameter für Indikator 03
modes <- c("WALK", "TRANSIT")
modes_filename <- paste(modes, collapse = "")
max_walk_time <- 19
max_rides = 3
max_trip_duration <- 61
#Einlesen von Gitter, Gemeinden und POI
zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  dplyr::select(id, ags, Einwohner)
gemeinden <- st_read(dsn = here("geodata/dvg1nw.gpkg"), layer = "gemeinden_regbez_vg250")
zentraleOrte_nrw <- st_transform(st_read(here("geodata/poi.gpkg"), "zentrale_orte_oz_manual"), crs = st_crs(zensus_grid))  %>%
  filter(str_detect(KN,  "^053") | add_outside) %>%
  filter(!(str_detect(KN,  "^053") & zentralitaet == "Oberzentrum" & oz_manual == FALSE)) %>%
  dplyr::select(area_id, GN, zentralitaet)

zentraleOrte_rlp <- st_transform(st_read(here("geodata/poi.gpkg"), "zentrale_orte_rlp"), crs = st_crs(zensus_grid))  %>%
  filter(add_outside) %>%
  mutate(area_id = paste0(GN)) %>%
  dplyr::select(area_id, GN, zentralitaet)

zentraleOrte <- rbind(zentraleOrte_nrw, zentraleOrte_rlp)

st_write(zentraleOrte, "geodata/poi.gpkg", "zentraleOrte_routingdestinations_zentrenkonzept", append = FALSE)

zentraleOrteid <- st_drop_geometry(zentraleOrte) %>%
  mutate(id = area_id) %>%
  dplyr::select(id, area_id, GN)

#R5 Setup
r5_network <- build_network(here("r5core_2026-05-18_large/"), verbose = FALSE, overwrite = FALSE)

#Erstellen von r5-lesbaren Start- und Zielorten. Nur bewohnte Gitterzellen als Startorte.
area_name <- "regbez_kln_zensus"
poi_type <- "zentraleOrte"
polygrid100 <- zensus_grid %>%
  filter(Einwohner > 0,
         !is.na(ags)) 

zensus_grid_df <- pointgrid_fun(polygrid100, "id")

pois_df <- pois_fun(pois = get(poi_type), id_col = "area_id")

#Erreichbarkeitsanalyse
#ttm <- Erreichbarkeit_nofiltering(origins = pois_df, destinations = zensus_grid_df, departure = as.POSIXct("2026-05-12 12:00:00"))

normdays <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
weekdays <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")

days <- weekdays
#hours <- c(8,10,11,15)


for(d in days){
  for (h in 8:17) {
    departure_dt <- as_datetime(d) + hours(h)
    
    cat(paste(departure_dt,"\n"))
    
    ttm <- Erreichbarkeit_nofiltering(origins = pois_df, destinations = zensus_grid_df, departure = departure_dt) %>%
      mutate(departure = departure_dt)
    
    fst::write_fst(ttm, file.path("output/i3_ttm_hourly/", paste0("i3_ttm_", d, "_", h, ".fst")))
  }
}


