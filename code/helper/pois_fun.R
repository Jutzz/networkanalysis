#Function to make r5r-compliant poi-dataframe (only x, y, id columns) from POINT sf Object

pois_fun <- function(pois, id_col = NULL) {
  pois <- st_transform(pois, crs = 4326) %>%
    mutate(tmp_id = as.character(row_number()))
  
  coords <- pois %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(lat = Y, lon = X)
  
  if (is.null(id_col)) {
    coords$id <- pois$tmp_id
  } else {
    coords$id <- as.character(pois[[id_col]])
  }
  
  return(coords)
}