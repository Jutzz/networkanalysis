#This script creates an INSPIRE-based 100m-edged geogrid in EPSG:3035 with INSPIRE-compliant grid ids. It is recommended to use the provided grids from the BKG at
#https://gdz.bkg.bund.de/index.php/default/geographische-gitter-fur-deutschland-in-lambert-projektion-geogitter-inspire.html and cut them down in QGIS. This is essentially a proof of concept or to be used for larger-edged grids.
library(sf)
library(tidyverse)
library(here)
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

area <- st_transform(st_read("geodata/dvg1nw.gpkg", "regbez25kmbuffer"), crs = st_crs(3035))

gem <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_25km")

csize <- 100

grid <- st_make_grid(area, cellsize = csize, offset = c(4020000,3022000))

grid_sf <- st_sf(
  geometry = grid
)

area_intersects <- st_intersects(grid_sf, area, sparse = FALSE)

grid_clipped <- grid_sf[area_intersects,]

coords <- st_coordinates(st_centroid(grid_clipped))

grid_clipped$id <- paste0(
  csize,"mN",
  floor(coords[,2] / 100),
  "E",
  floor(coords[,1] / 100)
)

centroids <- st_centroid(grid_clipped) %>%
  st_join(st_transform(gem, crs = st_crs(3035))) %>%
  select(id,KN) %>%
  rename("ags" = KN)

grid_with_ids <- st_as_sf(grid_clipped) %>%
  left_join(st_drop_geometry(centroids))

st_write(grid_with_ids, "geodata/grids.gpkg", paste0(csize, "mregbez25kmbuffer"), append = FALSE)
  