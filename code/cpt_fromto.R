options(java.parameters = "-Xmx20G")
library(tidyverse)
library(sf)
library(r5r)
library(here)
library(lubridate)
library(zoo)
library(osmextract)

filter_na <- function(tbl, expr){
  tbl %>% filter({{expr}} %>% replace_na(T))
}


osmextract::oe_vectortranslate("osmdata/zentraler_ort_pois.pbf", layer = "points", extra_tags = c("amenity", "shop", "craft", "office", "brand", "restaurant",  "place"), never_skip_vectortranslate = TRUE)

grid <- st_read("geodata/grids.gpkg", "100mregbez10kmbuffer")

poi <- st_transform(st_read("osmdata/zentraler_ort_pois.gpkg"), crs = st_crs(grid)) %>%
  replace_na(list(shop =  "no", amenity = "no")) %>%
  filter(shop != "vacant",
         amenity != "fast_food",
         amenity != "restaurant")

grid$poi_count <- lengths(st_intersects(grid, poi))

grid_i <- grid %>%
  group_by(ags) %>%
  top_n(1, poi_count)

st_write(grid_i, "geodata/poi.gpkg", "zentralorte_gridcells_100", append = FALSE)

grid_poifull <- grid %>%
  filter(poi_count>0)

st_write(grid_poifull, "geodata/poi.gpkg", "zentraleorte_gridcells_FULL_100", append = FALSE)

cpt <- st_read("geodata/poi.gpkg", "centralplaces_WIP")

r5_network <- setup_r5("r5core_2026-05-18/", overwrite = FALSE)

poi <- pois_fun(cpt, id_col = "ags") %>%
  filter(!is.na(id))

ttm <- travel_time_matrix(r5r_network = r5_network, origins = poi, destinations = poi, mode = c("WALK", "TRANSIT"), max_trip_duration = 300)

# all unique pairs of rows
idx <- t(combn(seq_len(nrow(cpt)), 2))

lines_sf <- st_sf(
  from = cpt$ags[idx[, 1]],
  to   = cpt$ags[idx[, 2]],
  geometry = st_sfc(
    lapply(seq_len(nrow(idx)), function(i) {
      st_linestring(
        rbind(
          st_coordinates(cpt[idx[i, 1], ]),
          st_coordinates(cpt[idx[i, 2], ])
        )
      )
    }),
    crs = st_crs(cpt)
  )
)

lines_tt <- lines_sf %>% left_join(ttm, by = join_by("from" == "from_id", "to" == "to_id"))

st_write(lines_tt, "output/lines_cpt_tt.gpkg")
