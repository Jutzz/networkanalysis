#!/usr/bin/env Rscript
# Determine one central place per municipal unit (gem)
# Method: use 250m grid cells to count POIs and pick the densest cell per gem.

library(sf)
library(dplyr)
library(tidyr)
library(here)

gpkg_grids <- "geodata/grids.gpkg"
gpkg_poi <- "osmdata/zentraler_ort_pois.gpkg"
gpkg_gem  <- "geodata/dvg1nw.gpkg"
out_gpkg  <- "output/central_places.gpkg"

stopifnot(file.exists(gpkg_grids), file.exists(gpkg_poi), file.exists(gpkg_gem))

message("Reading municipal units (gem) ...")
gem <- st_read(gpkg_gem, "gemeinden_regbez_kln", quiet = TRUE)

id_col <- if ("ags" %in% names(gem)) "ags" else {
  gem$gem_id <- as.character(seq_len(nrow(gem)))
  "gem_id"
}

message("Reading POIs ...")
poi <- st_read(gpkg_poi, quiet = TRUE) %>% st_transform(st_crs(gem))

# filter out obviously irrelevant POI types (same as other scripts)
poi <- poi %>% mutate(shop = replace_na(.data$shop, "no"), amenity = replace_na(.data$amenity, "no")) %>%
  filter(shop != "vacant", amenity != "fast_food", amenity != "restaurant")

message("Detecting 250m grid layer in geodata/grids.gpkg ...")
layers <- sf::st_layers(gpkg_grids)$name
grid_layer <- layers[grepl("250", layers)][1]
if (is.na(grid_layer) || is.null(grid_layer)) stop("No 250m grid layer found in geodata/grids.gpkg")

message("Reading grid layer: ", grid_layer)
grid250 <- st_read(gpkg_grids, grid_layer, quiet = TRUE) %>% st_transform(st_crs(gem))

message("Counting POIs per grid cell ...")
grid250$poi_count <- lengths(st_intersects(grid250, poi))


# Compute neighborhood-summed POI density to handle split dense areas across cell edges.
message("Computing neighborhood-summed POI density for grid cells ...")
# use centroids for distance-based neighborhood
grid_cent <- st_centroid(grid250)
# consider neighbors within ~400 m to include adjacent and diagonal cells
nb_mat <- st_is_within_distance(grid_cent, grid_cent, dist = 400)
neigh_sum <- sapply(nb_mat, function(idxs) sum(grid250$poi_count[idxs], na.rm = TRUE))
grid250$neigh_poi_sum <- neigh_sum

# Precompute grid cells intersecting each gem
message("Computing grid cells intersecting each gem (vectorised) ...")
gem_to_grid <- st_intersects(gem, grid250)

central_pts <- vector("list", length = nrow(gem))

for (i in seq_len(nrow(gem))) {
  gids <- gem_to_grid[[i]]

  chosen_pt <- NULL
  chosen_method <- NA_character_

    if (length(gids) > 0) {
      subgrid <- grid250[gids, , drop = FALSE]
      # choose cell with maximum neighborhood-summed poi count
      maxn <- max(subgrid$neigh_poi_sum, na.rm = TRUE)
      if (maxn > 0) {
        top_cells <- subgrid %>% filter(neigh_poi_sum == maxn)
        if (nrow(top_cells) > 1) {
          # tie-breaker: pick centroid closest to gem centroid
          gem_cent <- st_centroid(gem[i, ])
          dists <- st_distance(st_centroid(top_cells), gem_cent)
          chosen <- which.min(as.numeric(dists))
          chosen_pt <- st_centroid(top_cells[chosen, ])
          chosen_method <- "grid_neigh"
        } else {
          chosen_pt <- st_centroid(top_cells)
          chosen_method <- "grid_neigh"
        }
      }
    }

  # fallback: if no grid cell with POIs, but there are POIs inside the gem polygon,
  # use centroid of POIs inside gem
  if (is.null(chosen_pt) || nrow(chosen_pt) == 0) {
    poi_in_gem_idx <- which(as.logical(st_within(poi, gem[i, ], sparse = FALSE)))
    if (length(poi_in_gem_idx) > 0) {
      chosen_pt <- st_centroid(st_union(poi[poi_in_gem_idx, ]))
      chosen_method <- "poi_union"
    }
  }

  # final fallback: polygon centroid
  if (is.null(chosen_pt) || nrow(chosen_pt) == 0) {
    chosen_pt <- st_centroid(gem[i, ])
    chosen_method <- "poly_centroid"
  }

  # ensure same CRS and keep id
  geom_chosen <- st_geometry(chosen_pt)
  chosen_pt <- st_sf(data.frame(id = gem[[id_col]][i], method = chosen_method), geometry = geom_chosen, crs = st_crs(gem))
  names(chosen_pt)[1] <- id_col
  central_pts[[i]] <- chosen_pt
}

central_sf <- do.call(rbind, central_pts)
central_sf$method <- as.character(central_sf$method)

# diagnostics: show method column presence and counts
message("Method column present. Counts:")
print(table(central_sf$method, useNA = "ifany"))

st_write(central_sf, out_gpkg, "central_place_gem_250", append = FALSE, quiet = TRUE)

message("Done. Output layer: central_place_gem_250 in ", out_gpkg)
