#!/usr/bin/env Rscript
library(sf)
f <- "output/central_places.gpkg"
layer <- "central_place_gem_250"
if (!file.exists(f)) stop("Output geopackage not found: ", f)
cp <- st_read(f, layer, quiet = TRUE)
cat("Columns and first rows:\n")
print(names(cp))
print(utils::head(cp))
cat("\nSelection method counts:\n")
print(table(cp$method, useNA = "ifany"))
invisible()
