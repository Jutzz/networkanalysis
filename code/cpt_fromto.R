options(java.parameters = "-Xmx20G")
library(plyr)
library(tidyverse)
library(sf)
library(r5r)
library(here)
library(lubridate)
library(zoo)
library(osmextract)
library(terra)
library(spatialEco)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

filter_na <- function(tbl, expr){
  tbl %>% filter({{expr}} %>% replace_na(T))
}

osmextract::oe_vectortranslate("osmdata/zentraler_ort_pois.pbf", layer = "points", never_skip_vectortranslate = TRUE, osmconf_ini = "code/osmconf_cpt.txt")

osmextract::oe_vectortranslate("osmdata/zentraler_ort_pois.pbf", layer = "multipolygons", never_skip_vectortranslate = TRUE, osmconf_ini = "code/osmconf_cpt.txt")


gem <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln")


grid <- st_read("geodata/grids.gpkg", "100mregbez10kmbuffer")

poi_pt <- st_transform(st_read("osmdata/zentraler_ort_pois.gpkg", "points"), crs = st_crs(3857))
poi_poly <- st_centroid(st_transform(st_read("osmdata/zentraler_ort_pois.gpkg", "multipolygons"), crs = st_crs(3857)))


poi <- bind_rows(poi_pt, poi_poly) %>%
  mutate(
    category = case_when(
      
      # FOOD
      amenity %in% c(
        "restaurant","cafe","fast_food",
        "bar","pub","biergarten"
      ) ~ "Gastro",
      
      shop %in% c(
        "bakery","butcher","convenience",
        "supermarket","greengrocer"
      ) ~ "Food",
      
      # SHOPPING
      !is.na(shop) ~ "Shopping",
      
      # CULTURE
      amenity %in% c(
        "theatre","cinema","arts_centre",
        "library","archive","events_venue"
      ) ~ "Culture",
      
      tourism %in% c(
        "museum","gallery"
      ) ~ "Culture",
      
      # SOCIAL
      
      amenity %in% c(
        "social_facility","community_centre","youth_room","youth_welfare_office"
      ) ~ "Social",
      
      # ADMINISTRATION
      amenity %in% c(
        "townhall","courthouse"
      ) ~ "Administration",
      
      office == "government" ~ "Administration",
      
      # INFORMATION
      amenity %in% c(
        "bank","post_office","atm"
      ) ~ "Information",
      
      # CHILD + ELDERY
      amenity %in% c(
        "kindergarten", "childcare" ,"nursing_home"
      ) ~ "Care",
      
      # EDUCATION
      amenity %in% c(
        "school","college","university","prep_school"
      ) ~ "Education",
      
      # HEALTH
      amenity %in% c(
        "hospital","clinic","doctors",
        "pharmacy","dentist"
      ) ~ "Health",
      
      # LEISURE
      leisure %in% c("playground","dance","horse_riding","tanning_salon","fitness_centre","hackerspace","sports","sports_centre","pitch","fitness_station","sports_hall","spa","track","dog_park"
                     ) ~ "Leisure",
      amenity %in% c("dancing_school") ~ "Leisure",
      
      !is.na(public_transport) ~ "Mobility",
      
      TRUE ~ "Other"
    )
  ) %>%
  replace_na(list(shop =  "no", amenity = "no", place = "no", boundary = "no", historic = "no", type = "no")) %>%
  filter(shop != "vacant",
         !amenity %in% c("recycling" ,"vending_machine", "parking_entrance", "parking_space", "parking","waste_basket", "waste_disposal", "fast_food", "restaurant", "hitching_post", "hunting_stand","grit_bin","game_feeding","fountain","charging_station","bicycle_parking","bicycle_rental","bench"),
         type != "boundary",
         !place %in% c("locality", "farm", "village", "hamlet"),
         historic == "no",
         is.na(natural),
         is.na(highway))# %>%
  f <- poi %>% filter(category == "Other")
  d <- f %>% count(amenity)

st_write(poi_poly, "osmdata/zentraler_ort_pois_poly.gpkg", "POIs_3035_filtered_poly", append = FALSE)
st_write(poi %>% filter(category != "Other"), "osmdata/zentraler_ort_pois.gpkg", "POIs_3857_filtered", append = FALSE)

poi <- st_transform(poi, crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))

N <- "05315000"

sp::

for (N in unique(poi$KN)){
  poi_f <- poi %>%
    filter(KN == N) %>%
    st_transform(st_crs(3035))
  
  if(nrow(poi_f) == 0) 
    next
  else
  
  d <- sf.kde(poi_f, bw = 1000, res = 100, standardize = TRUE)
}

sta kde_poi

grid$poi_count <- lengths(st_intersects(grid, poi))

grid_i <- grid %>%
  group_by(ags) %>%
  top_n(1, poi_count)

st_write(grid_i, "geodata/poi.gpkg", "zentralorte_gridcells_100", append = FALSE)

grid_poifull <- grid %>%
  filter(poi_count>0)

st_write(grid_poifull, "geodata/poi.gpkg", "zentraleorte_gridcells_FULL_100", append = FALSE)

cpt <- st_read("geodata/poi.gpkg", "centralplaces_WIP") %>%
  filter(!is.na(ags))

r5_network <- setup_r5("r5core_2026-05-18/", overwrite = FALSE)

poi <- pois_fun(cpt, id_col = "ags") %>%
  filter(!is.na(id))

