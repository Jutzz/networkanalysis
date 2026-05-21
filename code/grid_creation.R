library(sf)
library(tidyverse)
library(here)
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

area <- st_transform(st_read("geodata/dvg1nw.gpkg", "regbez10kmbuffer"), crs = st_crs(3035))

grid <- st_make_grid(area, cellsize = 100, offset = c(4020000,3022000))

grid_sf <- st_sf(
  geometry = grid
)

grid_clipped <- st_intersection(grid_sf, area)

grid_with_ids <- grid %>%

st_write(grid, "geodata/grids.gpkg", "100mregbez10kmbuffer", append = FALSE)
  

#laea_grid <- st_read("geodata/base_data/DE_Grid_ETRS89-LAEA_100m.gpkg/DE_Grid_ETRS89-LAEA_100m/geogitter/DE_Grid_ETRS89-LAEA_100m.gpkg")
 
grid_area <- st_par(grid, st_filter(area), n_cores = 12)
