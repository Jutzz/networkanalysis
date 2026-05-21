#### Function to make a r5r-conforming dataframe of routing origins/destinations from a polygon grid.
pointgrid_fun <- function(polygrid, id_col = id) {
  pointgrid <- polygrid %>%
    st_centroid()
  
  grid <- st_transform(pointgrid, crs = 4326) #%>%
  #mutate(id = as.character(row_number()))
  
  grid_df <- grid %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(lat = Y, lon = X) %>% 
    dplyr::mutate(id = polygrid[[id_col]])
  
  return(grid_df)
}