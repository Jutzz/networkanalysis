##### Function to make a grid of routing origins/destinations.
polygrid_fun <- function(area, cellsize) {
  st_make_grid(
    x = st_transform(x = area, crs = st_crs(3035)),
    cellsize = cellsize, what = "polygons"
  ) %>%
    st_as_sf() %>%
    mutate(id = as.character(row_number()))
}