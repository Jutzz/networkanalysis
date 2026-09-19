library(tidytransit)
library(gtfstools)
library(dplyr)
library(readr)
library(sf)
library(here)

#Change feed date to used feed version in dataenv.R (filename).
source("code/dataenv.R")
buffer_size  <- 25
area_name <- paste0("regbez",buffer_size,"kmbuffer")

#TODO: generalize for any spatial filter
#Uncomment to process new fahrplaene_gesamtdeutschland. Downloaded feed into raw.
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/raw/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs.zip"))
unzip(paste0("feeds/raw/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs.zip"), files = c("routes.txt", "stops.txt"), exdir = paste0("feeds/extract/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs"))
area <- st_read(here("geodata/dvg1nw.gpkg"), area_name)
#area <- st_read(here("geodata/base_data/dvg1_EPSG25832_Shape/dvg1bld_nw.shp"))
de_gtfs_area <- filter_feed_by_area(gtfs_feed, area)
tidytransit::write_gtfs(de_gtfs_area, paste0("feeds/filtered/de_gtfs_",feed_date,"_",area_name,".zip"))

#DE: Der deutschlandweite Feed nutzt die optionale Tabelle transfers.txt, um
#Kuppelungen, Flügelungen und in-seat-transfers abzubilden. Außerdem verwendet
#er das optionale Feld parent_station in der stops.txt, wodurch einzelne
#Einstiegspunkte eines größeren Haltes zusammengefasst werden können.
#filter_feed_by_area() berücksichtigt diese optionalen Werte nicht,  Trips, die
#zum Teil außerhalb des gewählten Bereiches liegen, werden mit ihren stops
#beibehalten, nicht aber die zugehörige parent_station (d.h. beispielsweise ein
#Gleis eines Bahnhofs ist im gefilterten Feed enthalten, nicht aber der Bahnhof
#selbst. Ebenso verhält es sich mit Routen, die als transfer einer Route im
#Bereich angegeben sind, aber außerhalb des Bereichs liegen.

#filter_feed_by_area doesnt keep parent_stops and routes in transfers, so this is handled manually
de_gtfs_stops <- read_csv(paste0("feeds/extract/", feed_date,"_fahrplaene_gesamtdeutschland_gtfs/stops.txt"))
de_gtfs_routes <- read_csv(paste0("feeds/extract/", feed_date,"_fahrplaene_gesamtdeutschland_gtfs/routes.txt"))
area_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_",feed_date,"_",area_name,".zip"))

de_gtfs_parent_stops <- de_gtfs_stops %>%
  filter(stop_id %in% area_feed$stops$parent_station) %>%
  filter(!stop_id %in% area_feed$stops$stop_id) %>%
  mutate(level_id = as.character(level_id))

de_gtfs_transfer_routes <- de_gtfs_routes %>%
  filter(route_id %in% area_feed$transfers$from_route_id | route_id %in% area_feed$transfers$to_route_id) %>%
  filter(!route_id %in% area_feed$routes$route_id) %>%
  mutate(agency_id = as.character(agency_id))

area_feed$stops <- area_feed$stops %>%
  bind_rows(de_gtfs_parent_stops) 
  
area_feed$routes <- area_feed$routes %>%
  bind_rows(de_gtfs_transfer_routes) %>%
  mutate(route_type = as.integer(route_type))

area_feed <- area_feed %>%
  filter_by_route_type(route_type = 1501, keep = FALSE) %>%
  frequencies_to_stop_times() %>%
  as_tidygtfs()
  

#Writing into /feeds, copy manually into r5core_current for processing with r5r.
tidytransit::write_gtfs(area_feed, paste0("feeds/filtered/nofreq_de_gtfs_",feed_date, "_",area_name,".zip"))

stops <- unique(area_feed$stops$stop_id)

pfaedle_feed <- area_feed

pfaedle_feed$pathways <- pfaedle_feed$pathways %>%
  filter(from_stop_id %in% stops, to_stop_id %in% stops)

tidytransit::write_gtfs(pfaedle_feed, paste0("feeds/filtered/pfaedle_de_gtfs_",feed_date, "_",area_name,".zip"))

