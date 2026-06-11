#!/usr/bin/env Rscript
# KDE-based central place per gem: compute a smoothed kernel density from POIs
# and pick the density maximum inside each gem. Fallback to POI centroid or polygon centroid.

library(sf)
library(dplyr)
library(tidyr)
library(MASS)

gpkg_poi <- "osmdata/zentraler_ort_pois.gpkg"
 gpkg_gem <- "geodata/dvg1nw.gpkg"
 out_gpkg <- "output/central_places_kde.gpkg"

stopifnot(file.exists(gpkg_poi), file.exists(gpkg_gem))

message("Reading data...")
poi <- st_read(gpkg_poi, quiet = TRUE)
gem <- st_read(gpkg_gem, "gemeinden_regbez_kln", quiet = TRUE)

# ensure projected CRS in meters
if (st_is_longlat(gem)) stop("Please use projected CRS for accurate distances")
poi <- st_transform(poi, st_crs(gem))

# basic filters similar to other scripts
poi <- poi %>% mutate(shop = ifelse(is.na(shop), "no", shop), amenity = ifelse(is.na(amenity), "no", amenity)) %>%
  filter(shop != "vacant", amenity != "fast_food", amenity != "restaurant")

central_list <- vector("list", nrow(gem))

for (i in seq_len(nrow(gem))) {
  g <- gem[i, ]
  # POIs inside gem
  pts_in <- poi[st_within(poi, g, sparse = FALSE)[,1], ]
  method <- NA_character_

  if (nrow(pts_in) >= 3) {
    # prepare grid for KDE based on gem bbox
    bbox <- st_bbox(g)
    expand <- 0.1
    xrange <- c(bbox$xmin - expand * (bbox$xmax - bbox$xmin), bbox$xmax + expand * (bbox$xmax - bbox$xmin))
    yrange <- c(bbox$ymin - expand * (bbox$ymax - bbox$ymin), bbox$ymax + expand * (bbox$ymax - bbox$ymin))

    xy <- st_coordinates(st_transform(pts_in, st_crs(g)))
    # choose grid size relative to gem size (max 200x200)
    nx <- ny <- 100
    kd <- MASS::kde2d(xy[,1], xy[,2], n = c(nx, ny), lims = c(xrange, yrange))
    # find max density cell
    ind <- which(kd$z == max(kd$z), arr.ind = TRUE)[1, ]
    x_max <- kd$x[ind[1]]
    y_max <- kd$y[ind[2]]
    pt <- st_sfc(st_point(c(x_max, y_max)), crs = st_crs(g))
    # ensure point lies inside gem; if not, snap to nearest POI
    if (!st_within(pt, g, sparse = FALSE)[1,1]) {
      # nearest POI
      d <- st_distance(st_sfc(pt), st_geometry(pts_in))
      nearest <- which.min(as.numeric(d))
      pt <- st_geometry(pts_in[nearest, ])
      method <- "kde_snapped"
    } else {
      method <- "kde"
    }

  } else if (nrow(pts_in) > 0) {
    # too few points for KDE: use centroid of POIs
    pt <- st_centroid(st_union(pts_in))
    method <- "poi_centroid"
  } else {
    # no POIs inside gem: use polygon centroid
    pt <- st_centroid(g)
    method <- "poly_centroid"
  }

  df <- data.frame(id = ifelse("ags" %in% names(g), as.character(g$ags), as.character(i)), method = method, stringsAsFactors = FALSE)
  names(df)[1] <- ifelse("ags" %in% names(g), "ags", "gem_id")
  central_list[[i]] <- st_sf(df, geometry = st_geometry(pt), crs = st_crs(g))
}

central_sf <- do.call(rbind, central_list)

message("Writing result to ", out_gpkg)
if (file.exists(out_gpkg)) file.remove(out_gpkg)
st_write(central_sf, out_gpkg, "central_place_gem_kde", append = FALSE, quiet = TRUE)
message("Done")
