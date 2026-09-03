library(future.apply)
library(progressr)

options(progressr.enable = TRUE)
handlers("rstudio")

# Use multisession for cross-platform compatibility (including Windows)
workers <- max(2)
plan(multisession, workers = workers)

unique_kn <- unique(poi_flex_full$KN)
total <- length(unique_kn)

process_municipality <- function(N) {
  
  poi_f <- poi_flex_full %>%
    filter(KN == N) %>%
    st_transform(st_crs(3035))
  
  if (nrow(poi_f) == 0) {
    return(NULL)
  }
  
  muni <- gem %>%
    filter(KN == N)
  
  # KDE
  d <- sf.kde(
    poi_f,
    bw = 1000,
    res = as.numeric(st_area(muni) / 15000000),
    standardize = TRUE,
    ref = muni
  )
  
  breaks <- seq(0.1, 1, by = 0.1)
  
  d_class <- classify(
    d,
    cbind(
      breaks[-length(breaks)],
      breaks[-1],
      seq_along(breaks[-1])
    )
  )
  
  bands <- as.polygons(d_class, dissolve = TRUE) %>%
    st_as_sf(crs = st_crs(3035)) %>%
    dplyr::rename(density = lyr.1) %>%
    filter(density >= 1) %>%
    mutate(KN = N) %>%
    smooth(method = "ksmooth", smoothness = 2) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON", do_split = TRUE)
  
  # Safe because every municipality gets its own TIFF file
  writeRaster(
    d,
    paste("output/kderasters/kde", N, "flex.tif", sep = "_"),
    overwrite = TRUE
  )
  
  message(N, " done.")
  
  bands
}

# Run municipalities in parallel
with_progress({
  
  p <- progressor(steps = length(unique_kn))
  
  bands_list <- future_lapply(
    unique_kn,
    function(N) {
      result <- process_municipality(N)
      
      p(message = paste(N, "completed"))
      result
    },
    future.seed = TRUE
  )
  
})
plan(sequential)
# Remove municipalities without results
bands_list <- Filter(Negate(is.null), bands_list)

# Combine results
bands_all <- do.call(rbind, bands_list)

# Write once, sequentially, to avoid concurrent GeoPackage writes
st_write(
  bands_all,
  "geodata/zentrale_orte_bands.gpkg",
  layer = "bands_flex",
  append = FALSE
)

