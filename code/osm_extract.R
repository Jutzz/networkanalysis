library(dplyr)
library(rosmium)
library(sf)
library(readr)
library(httr2)
library(here)
library(lubridate)
options(timeout = 1000)

dldate <- format(Sys.Date(), format = "%y%m%d")
dldate_zhv <- format(Sys.Date(), format = "%Y-%m-%d")
#Download latest OSM-extract for NRW.
osm_req <- request("https://download.geofabrik.de/europe/germany/nordrhein-westfalen-latest.osm.pbf")
osmresp <- req_perform(osm_req, path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"))

regbez10km <- st_read("geodata/regbez10kmbuffer.geojson")
#Extract Regierungsbezirk plus 10km as buffer. Osmium needs to be locally available.
rosmium::extract(input_path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"), extent = regbez10km, output_path = paste0("osmdata/regbez10km-", dldate, ".osm.pbf"), overwrite = TRUE)

zhv_req <- request("https://www.opendata-oepnv.de/fileadmin/datasets/delfi/20260521_zHV_gesamt.zip")
zhvresp <- req_perform(zhv_req, path = paste0("geodata/zhv/", dldate, "_zHV_gesamt.zip"))

unzip(zipfile = "geodata/zhv/zHV_aktuell_csv.2026-05-21.zip",exdir = paste0("geodata/zhv/", dldate, "_zHV_gesamt"))

zhv <- read_csv2(paste0("geodata/zhv/",dldate,"_zHV_gesamt/zHV_aktuell_csv.",dldate_zhv,".csv"))

zhv_geo <- st_as_sf(zhv, coords = c("Longitude", "Latitude"), crs = st_crs(4326))

st_write(zhv_geo, here("geodata/poi.gpkg"), paste0("zhv_",dldate), append = FALSE)
