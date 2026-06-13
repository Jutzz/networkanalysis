library(plyr)
library(tidyverse)
library(sf)
library(here)
library(osmextract)
library(terra)
library(spatialEco)
library(smoothr)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

filter_na <- function(tbl, expr){
  tbl %>% filter({{expr}} %>% replace_na(T))
}

osmdate <- "260521"

gem <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln")

#Turn osm points and polygons into geopackage according to osmconf_cpt.txt
osmextract::oe_vectortranslate("osmdata/zentraler_ort_pois.pbf", layer = "points", never_skip_vectortranslate = TRUE, osmconf_ini = "code/osmconf_cpt.txt")

osmextract::oe_vectortranslate("osmdata/zentraler_ort_pois.pbf", layer = "multipolygons", never_skip_vectortranslate = TRUE, osmconf_ini = "code/osmconf_cpt.txt")

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
         is.na(highway))

poi_flex <- bind_rows(poi_pt, poi_poly) %>%
  filter(amenity == "pharmacy" | grepl("apotheke", name, ignore.case = TRUE) |
         office == "government" | grepl("Bürgerbüro", name, ignore.case = TRUE) |
         amenity == "library" | grepl("bibliothek|bücherei", name, ignore.case = TRUE) |
         community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE) |
         isced_level == 1 | grepl("Grundschule", name, ignore.case = TRUE) | school == "primary" |
         isced_level == 2 | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) | school == "secondary" |
         amenity == "doctors" | healthcare == "doctor" | grepl("Hausarzt", name, ignore.case = TRUE) |
         amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE) |
         amenity %in% c("post_office") |
         leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE) |
         amenity == "nursing_home" | social_facility == "nursing_home" | grepl("Seniorenheim|Altenheim|betreutes wohnen", name, ignore.case = TRUE) |
         shop %in% c("supermarket", "convenience") | building == "supermarket" |
         amenity == "dentist" | healthcare == "dentist" | grepl("Zahnarzt|odonto", name, ignore.case = TRUE) |
         amenity == "bank" |
         amenity == "atm") %>%
  filter(!amenity %in% c("parking", "bicycle_parking", "parking_space", "charging_station", "police", "trailer_parking")) %>%
  mutate(category = case_when(
    amenity == "pharmacy"        | grepl("apotheke", name, ignore.case = TRUE)                                                              ~ "Pharmacy",
    office == "government"       | grepl("Bürgerbüro", name, ignore.case = TRUE)                                                            ~ "Government Office",
    amenity == "library"         | grepl("bibliothek|bücherei", name, ignore.case = TRUE)                                                   ~ "Library",
    community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE)                                                   ~ "Youth Centre",
    isced_level == 1  | school == "primary"    | grepl("Grundschule", name, ignore.case = TRUE)                                             ~ "Primary School",
    isced_level == 2  | school == "secondary"  | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) ~ "Secondary School",
    amenity == "doctors"         | healthcare == "doctor"   | grepl("Hausarzt", name, ignore.case = TRUE)                                   ~ "General Practitioner",
    amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE)               ~ "Kindergarten / Childcare",
    amenity == "post_office"                                                                                                                 ~ "Post Office",
    leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE)                     ~ "Sports Facility",
    amenity == "nursing_home"    | social_facility == "nursing_home" | grepl("Seniorenheim|Altenheim|betreutes wohnen", name, ignore.case = TRUE) ~ "Nursing Home",
    shop %in% c("supermarket", "convenience") | building == "supermarket"                                                                   ~ "Supermarket",
    amenity == "dentist"         | healthcare == "dentist"  | grepl("Zahnarzt|odonto", name, ignore.case = TRUE)                            ~ "Dentist",
    amenity == "bank"                                                                                                                        ~ "Bank",
    amenity == "atm"                                                                                                                         ~ "ATM",
    .default = NA_character_
  )) %>%  select(1,22,3,4,5,25,geometry)
         
st_write(poi %>% filter(category != "Other"), "geodata/pois.gpkg", paste0("zo_POI_large_", osmdate), append = FALSE)
st_write(poi_flex, "geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate), append = FALSE)

gem <- st_transform(gem, crs = st_crs(3035))

poi <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_large_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))

poi_flex <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))


N <- "05315000"

unique_kn <- unique(poi$KN)
total <- length(unique_kn)

for (N in unique(poi$KN)){
  if(!N %in% gem$KN)
    next
  else
  
  poi_f <- poi %>%
    filter(KN == N) %>%
    st_transform(st_crs(3035))
  
  if(nrow(poi_f) == 0) 
    next
  else
    
  d <- sf.kde(poi_f, bw = 1000, res = as.numeric(st_area(gem[gem$KN == N, ])/15000000), standardize = TRUE, ref = gem[gem$KN == N, ])
  
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
    rename("density" = lyr.1) %>%
    filter(density >= 1) %>%
    mutate(KN = N) %>%
    smooth(method = "ksmooth", smoothness = 2) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON")
  
  plot(bands)
  writeRaster(d, paste("output/kderasters/kde", N, "large.tif", sep = "_"), overwrite = TRUE)
  st_write(bands, "geodata/zentrale_orte_bands.gpkg", layer = "bands_large", append = TRUE)
  
  message(N, " done. ", match(N, unique_kn), "/", total)
}

for (N in unique(poi_flex$KN)){
  poi_f <- poi_flex %>%
    filter(KN == N) %>%
    st_transform(st_crs(3035))
  
  if(nrow(poi_f) == 0) 
    next
  else
    
  d <- sf.kde(poi_f, bw = 1000, res = as.numeric(st_area(gem[gem$KN == N, ])/15000000), standardize = TRUE, ref = gem[gem$KN == N, ])
  
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
    rename("density" = lyr.1) %>%
    filter(density >= 1) %>%
    mutate(KN = N) %>%
    smooth(method = "ksmooth", smoothness = 2) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON")
  
  plot(bands)
  writeRaster(d, paste("output/kderasters/kde", N, "flex.tif", sep = "_"), overwrite = TRUE)
  st_write(bands, "geodata/zentrale_orte_bands.gpkg", layer = "bands_flex", append = TRUE)
  message(N, " done. ", match(N, unique_kn), "/", total)
}

plot(top10perc) %>%
  contours()

step <- as.polygons(top10perc, round = TRUE, digits = 1)


step <- st_as_sf(step)

min_value <- as_tibble(d, na.rm = TRUE) %>%
  slice_max(order_by = lyr.1, prop = .01) %>%
  min()

top10perc <- d %>% filter(lyr.1 > min_value)

plot(top10perc)

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

