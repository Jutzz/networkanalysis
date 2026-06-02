library(tidytransit)
library(dplyr)
library(readr)
library(sf)
library(here)

#Change feed date to used feed version (filename).
feed_date <- "20260525"
area_name <- "regbez"
s
#TODO: generalize for any spatial filter
#Uncomment to process new fahrplaene_gesamtdeutschland. Downloaded feed into raw.
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/raw/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs.zip"))
unzip(paste0("feeds/raw/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs.zip"), files = c("routes.txt", "stops.txt"), exdir = paste0("feeds/extract/", feed_date, "_fahrplaene_gesamtdeutschland_gtfs"))
area <- st_read(here("geodata/dvg1nw.gpkg"), "regbez10kmbuffer")
#area <- st_read(here("geodata/base_data/dvg1_EPSG25832_Shape/dvg1bld_nw.shp"))
de_gtfs_area <- filter_feed_by_area(gtfs_feed, area)
tidytransit::write_gtfs(de_gtfs_area, paste0("feeds/filtered/de_gtfs_",feed_date,"_",area_name,".zip"))

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
  bind_rows(de_gtfs_transfer_routes) 

#Writing into /feeds, copy manually into r5core_current for processing with r5r.
tidytransit::write_gtfs(area_feed, paste0("feeds/filtered/de_gtfs_",feed_date, "_",area_name,".zip"))
