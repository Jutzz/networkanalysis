options(java.parameters = "-Xmx20G")
library(r5r)
library(tidyverse)
library(sf)
library(gpkg)
library(raster)
library(tidytransit)
library(here)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

#Erreichbarkeitsanalyse, Ergebnis gefiltert nach kürztester Reisezeit, hinzufügen von Start/Endzeit und Start/Zielortname, schreiben als gpkg-Layer
Erreichbarkeit <- function(origins, destinations) {
  timestamp <- format(Sys.Date(), "%m-%y") 
  
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    max_rides = max_rides,
    walk_speed = 4.2,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 60,  # Time window in minutes for departures
    percentiles = 1,
    progress = FALSE
  )
  
  
  shortest_travel_times <- ttm %>%
    group_by(to_id) %>%
    slice_min(travel_time_p01, with_ties = FALSE) %>%
    ungroup()
  
  grid_with_times <- polygrid100 %>%
    filter(if ("Einwohner" %in% names(.)) Einwohner != 0 else TRUE) %>%
    left_join(shortest_travel_times %>%
                dplyr::select(from_id, to_id, travel_time_p01), by = c("id" = "to_id")) %>%
    mutate(start_time = as.POSIXct("2025-02-19 08:00:00")) %>%
    mutate(end_time = start_time + minutes(travel_time_p01)) #%>%
    #left_join(zentraleOrteid, by = c("to_id" = "id")) %>%
    #dplyr::select(!c("ART", "STAND", "KN7stellig")) %>%
    #rename("GN_ziel" = GN, "core_ziel" = core_id)
  
  st_write(grid_with_times, here("output/indikator_03.gpkg"), layer = paste0("de_gtfs_polygrid_100m_shortest_", poi_type, "_", modes_filename, "_", max_trip_duration,"min"), append = FALSE)
}

#Wie oben, ohne filtern nach kürzester
Erreichbarkeit_nofiltering <- function(origins, destinations) {
  
  ttm <- travel_time_matrix(
    r5r_network = r5_network,
    origins = origins,
    destinations = destinations,
    mode = modes,
    max_rides = max_rides,
    walk_speed = 4.2,
    mode_egress = "WALK",
    max_walk_time = max_walk_time,
    departure_datetime = departure,
    max_trip_duration = max_trip_duration,
    time_window = 60,  # Time window in minutes for departures
    percentiles = 1,
    progress = TRUE
  )
  
  grid_with_times <- polygrid100 %>%
    filter(if ("Einwohner" %in% names(.)) Einwohner != 0 else TRUE) %>%
    left_join(ttm %>%
                dplyr::select(from_id, to_id, travel_time_p01), by = c("id" = "to_id")) %>%
    mutate(start_time = as.POSIXct("2025-02-19 08:00:00")) %>%
    mutate(end_time = start_time + minutes(travel_time_p01)) %>%
    left_join(zentraleOrteid, by = c("from_id" = "id")) %>%
    #dplyr::select(!c("ART", "STAND", "KN7stellig")) %>%
    rename("GN_ziel" = GN, "core_ziel" = core_id)
  
  st_write(grid_with_times, here("output/indikator_03.gpkg"), layer = paste0("nrw_gtfs_polygrid_100m_all_destinations_subset", "_", poi_type, "_", modes_filename, "_", max_trip_duration,"min"), append = FALSE)
  return(ttm)
}

catchment_fun <- function(grid, overlay){
  #Separate catched and uncatched cells
  grid_catched <- grid %>%
    dplyr::filter(!is.na(travel_time_p01))
  
  grid_uncatched <- grid %>%
    dplyr::filter(is.na(travel_time_p01))
  
  print("Catchment Separation done")
  
  total_pop <- st_join(overlay, grid, join = st_contains) %>%
    group_by(GN, KN) %>%  
    summarize(
      count = n(),  # Count of intersecting features
      bevölkerung_sum = sum(Einwohner, na.rm = TRUE
      )
    )
  
  print("Total Population Aggregation done")
  
  catched_pop <- st_join(overlay, grid_catched %>%
                           filter(!is.na(travel_time_p01)), join = st_contains) %>%
    group_by(GN, KN) %>%  
    summarize(
      count = n(),  # Count of intersecting features
      bevölkerung_catched_sum = sum(Einwohner, na.rm = TRUE)
    )
  print("Catched Population Aggregation done")
  
  uncatched_pop <- st_join(overlay, grid_uncatched %>%
                             filter(is.na(travel_time_p01)), join = st_contains) %>%
    group_by(GN, KN) %>%  
    summarize(
      count = n(),  # Count of intersecting features
      bevölkerung_uncatched_sum = sum(Einwohner, na.rm = TRUE)
    )
  
  print("Uncatched Population Aggregation done")
  
  catchment <- st_join(total_pop, catched_pop, join = st_equals, left = TRUE)
  catchment <- st_join(catchment, uncatched_pop, join = st_equals) %>%
    dplyr::select(GN, KN, bevölkerung_sum, bevölkerung_catched_sum, bevölkerung_uncatched_sum) %>%
    mutate(catched_percentage = round((bevölkerung_catched_sum/bevölkerung_sum)*100, 2),
           uncatched_percentage = round((bevölkerung_uncatched_sum/bevölkerung_sum)*100, 2))
  
  print("Stats done")
  
  return(catchment)
}

#Parameter für Erreichbarkeitsanalyse
modes <- c("WALK", "TRANSIT")
modes_filename <- paste(modes, collapse = "")
max_walk_time <- 19
max_rides = 3
departure <- as.POSIXct("2026-05-12 09:00:00")
max_trip_duration <- 60
#Einlesen von Gitter, Gemeinden und POI
zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  dplyr::select(id, ags, Einwohner)
gemeinden <- st_read(dsn = here("geodata/dvg1nw.gpkg"), layer = "gemeinden_regbez_kln")
zentraleOrte <- st_transform(st_read(here("geodata/poi.gpkg"), "zo_pd3_cut0.17"), crs = crs(zensus_grid))
zentraleOrteid <- st_drop_geometry(zentraleOrte) %>%
  mutate(id = core_id) %>%
  dplyr::select(id, core_id, GN)

#R5 Setup
r5_network <- build_network(here("r5core_current"), verbose = FALSE, overwrite = FALSE)

#Erstellen von r5-lesbaren Start- und Zielorten. Nur bewohnte Gitterzellen als Startorte.
area_name <- "regbez_kln_zensus"
poi_type <- "zentraleOrte"
polygrid100 <- zensus_grid %>%
  filter(Einwohner > 0)

zensus_grid_df <- pointgrid_fun(polygrid100, "id")

pois_df <- pois_fun(pois = get(poi_type), id_col = "core_id")

#Erreichbarkeitsanalyse
ttm <- Erreichbarkeit_nofiltering(origins = pois_df, destinations = zensus_grid_df)

all_clear()


i3full <- st_read(here("output/indikator_03.gpkg"), "nrw_gtfs_polygrid_100m_all_destinations_subset_zentraleOrte_WALKTRANSIT_60min")


cent <- st_centroid(i3full) %>%
  st_join(st_transform(gemeinden, crs = st_crs(i3full)), st_intersects)

cent_s <- st_drop_geometry(cent) %>%
  dplyr::select(id, core_ziel, GN, KN) %>%
  rename("origin_KN" = KN,
         "origin_GN" = GN)

i3full_coded <- i3full %>%
  left_join(cent_s)

zensusstats <- i3full_coded %>%
  st_drop_geometry() %>%
  group_by(origin_KN,GN_ziel) %>%
  mutate(pop_sum = sum(Einwohner)) %>%
  dplyr::select(origin_GN, origin_KN, pop_sum) %>%
  distinct()

i3fullstats <- i3full %>%
  st_drop_geometry() %>%
  filter(ziel_is_origin == FALSE) %>%
group_by(origin_KN) %>%
  mutate("sum_orignotdest" = sum(Einwohner)) %>%
  dplyr::select(9:12) %>%
  distinct() %>%
  left_join(zensusstats, by = c("origin_GN", "origin_KN")) %>%
  mutate("perc_orignotdest" = (sum_orignotdest/pop_sum)*100)

i3_diffdest <- gemeinden %>%
  left_join(i3fullstats, by = c("KN" = "origin_KN", "GN" = "origin_GN")) %>%
  dplyr::select(!c("ziel_is_origin"))

st_write(i3_diffdest, here("results/indikator_03.gpkg"), "destination_different_from_origin")


i3all <- st_read(here("output/indikator_03.gpkg"), "nrw_gtfs_polygrid_100m_all_destinations_zentraleOrte_WALKTRANSIT_60min_origin_coded")

i3all_noaccess <- i3all %>%
  filter(is.na(travel_time_p01))

i3all_noaccess_stats <- i3all_noaccess %>%
  st_drop_geometry() %>%
  group_by(origin_KN) %>%
  mutate(uncatched = sum(Einwohner)) %>%
  dplyr::select(origin_GN, origin_KN, uncatched) %>%
  distinct()

diffdest <- st_read("indikator3.gpkg", "destination_different_from_origin")
diffdest <- diffdest %>%
  left_join(i3all_noaccess_stats, by = c("KN" = "origin_KN", "GN" = "origin_GN")) %>%
  mutate(perc_uncatched = (uncatched/pop_sum)*100)

zensus_populated <- st_read("grids.gpkg", "zensusgrid_populated")
i3_cgn <- i3all %>%
  filter(GN_ziel == "Köln" | is.na(travel_time_p01))

catchment_fun(grid = i3full_coded, overlay = st_transform(gemeinden, crs = st_crs(i3full_coded)))

              