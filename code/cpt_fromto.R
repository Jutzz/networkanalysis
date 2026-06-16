library(plyr)
library(tidyverse)
library(sf)
library(here)
library(osmextract)
library(terra)
library(spatialEco)
library(smoothr)
library(nngeo)

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
  filter(amenity == "pharmacy" | grepl("apotheke", name, ignore.case = TRUE)|
           office == "government" | grepl("Bürgerbüro", name, ignore.case = TRUE) |
           amenity == "library" | grepl("bibliothek|bücherei", name, ignore.case = TRUE) |
           community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE) |
           isced_level == 1 | grepl("Grundschule", name, ignore.case = TRUE) | school == "primary" |
           isced_level == 2 | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) | school == "secondary"| 
           amenity == "doctors" | healthcare == "doctor" | grepl("Hausarzt", name, ignore.case = TRUE) |
           amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE) |
           amenity %in% c("post_office") |
           leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE) |
           amenity == "nursing_home" | social_facility == "nursing_home" | grepl("Seniorenheim|Altenheim|betreutes wohnen", name, ignore.case = TRUE) |
           shop %in% c("supermarket", "convenience") | building == "supermarket" |
           amenity == "dentist" | healthcare == "dentist" | grepl("Zahnarzt|odonto", name, ignore.case = TRUE)|
           amenity == "bank" | amenity == "atm") %>%
  filter(!amenity %in% c("parking", "bicycle_parking", "parking_space",
                         "charging_station", "police", "trailer_parking")) %>% 
  mutate(category = case_when( amenity == "pharmacy" | grepl("apotheke", name, ignore.case = TRUE) ~ "Pharmacy",
                               office == "government" | grepl("Bürgerbüro", name, ignore.case = TRUE) ~ "Government Office",
                               amenity == "library" | grepl("bibliothek|bücherei", name, ignore.case = TRUE) ~ "Library",
                               community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE) ~ "Youth Centre",
                               isced_level == 1 | school == "primary" | grepl("Grundschule", name, ignore.case = TRUE) ~ "Primary School",
                               isced_level == 2 | school == "secondary" | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) ~ "Secondary School",
                               amenity == "doctors" | healthcare == "doctor" | grepl("Hausarzt", name, ignore.case = TRUE) ~ "General Practitioner",
                               amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE) ~ "Kindergarten / Childcare",
                               amenity == "post_office" ~ "Post Office",
                               leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE) ~ "Sports Facility",
                               amenity == "nursing_home" | social_facility == "nursing_home" | grepl("Seniorenheim|Altenheim|betreutes wohnen", name, ignore.case = TRUE) ~ "Nursing Home",
                               shop %in% c("supermarket", "convenience") | building == "supermarket" ~ "Supermarket",
                               amenity == "dentist" | healthcare == "dentist" | grepl("Zahnarzt|odonto", name, ignore.case = TRUE) ~ "Dentist",
                               amenity == "bank" ~ "Bank",
                               amenity == "atm" ~ "ATM",
                               .default = NA_character_
    )
  )

poi_flex_optional <- bind_rows(poi_pt, poi_poly) %>%
  filter(amenity == "lawyer" | office == "lawyer" | grepl("Anwalt|Rechtsanwalt|Kanzlei", name, ignore.case = TRUE) |
           amenity == "car_repair" | shop == "car_repair" |
           shop == "bakery" |
           shop == "optician" |
           tourism == "travel_agency" |grepl("Reisebüro", name, ignore.case = TRUE) |
           shop == "shoes" |
           shop == "chemist" | shop == "drugstore" | grepl("Drogerie", name, ignore.case = TRUE) |
           leisure == "fitness_centre" |
           shop == "plumber" | craft == "plumber" |
           office == "accountant" | grepl("Steuerberat", name, ignore.case = TRUE) |
           railway %in% c("halt", "station") | public_transport %in% c("stop_position", "platform") | grepl("Haltepunkt|Bahnhof", name, ignore.case = TRUE) ) %>%
  filter( !amenity %in% c( "parking", "bicycle_parking", "parking_space",
                           "charging_station", "police", "trailer_parking" ) ) %>%
  filter(is.na(highway)) %>%
  mutate( category = case_when(
    amenity == "lawyer" | office == "lawyer" | grepl("Anwalt|Rechtsanwalt|Kanzlei", name, ignore.case = TRUE) ~ "Lawyer",
    amenity == "car_repair" | shop == "car_repair" ~ "Car Repair Workshop",
    shop == "bakery" ~ "Bakery",
    shop == "optician" ~ "Optician",
    tourism == "travel_agency" | grepl("Reisebüro", name, ignore.case = TRUE) ~ "Travel Agency",
    shop == "plumber" | craft == "plumber" ~ "Plumbing Services",
    shop == "shoes" ~ "Shoe Store",
    office == "accountant" | grepl("Steuerberat", name, ignore.case = TRUE) ~ "Tax Advisor",
    shop == "chemist" | shop == "drugstore" | grepl("Drogerie", name, ignore.case = TRUE) ~ "Drugstore",
    leisure == "fitness_centre" ~ "Fitness Centre",
    railway %in% c("halt", "station") | public_transport %in% c("stop_position", "platform") | grepl("Haltepunkt|Bahnhof", name, ignore.case = TRUE) ~ "Public Transport Stop",
    .default = NA_character_ )
  ) %>%
  select(1,2,category,geometry)

st_write(poi %>% filter(category != "Other"), "geodata/pois.gpkg", paste0("zo_POI_large_", osmdate), append = FALSE)
st_write(poi_flex, "geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate), append = FALSE)
st_write(poi_flex_optional, "geodata/pois.gpkg", paste0("zo_POI_flex_opt", osmdate), append = FALSE)

gem <- st_transform(gem, crs = st_crs(3035))

poi <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_large_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))

poi_flex <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))

poi_flex_optional <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_opt", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))


unique_kn <- unique(poi$KN)
total <- length(unique_kn)

#KDE for every muni, normalized and written as poly bands
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
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON", do_split = TRUE)
  
  plot(bands)
  writeRaster(d, paste("output/kderasters/kde", N, "large.tif", sep = "_"), overwrite = TRUE)
  st_write(bands, "geodata/zentrale_orte_bands.gpkg", layer = "bands_large", append = TRUE)
  
  message(N, " done. ", match(N, unique_kn), "/", total)
}

for (N in unique(poi_flex$KN)){
  poi_f <- poi_flex %>%
    filter(optional == FALSE) %>%
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
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON", do_split = TRUE)
  
  plot(bands)
  writeRaster(d, paste("output/kderasters/kde", N, "flex.tif", sep = "_"), overwrite = TRUE)
  st_write(bands, "geodata/zentrale_orte_bands.gpkg", layer = "bands_flex", append = TRUE)
  message(N, " done. ", match(N, unique_kn), "/", total)
}
#Turning bands into closed areas of equal minimal density
bands_flex <- st_read("geodata/zentrale_orte_bands.gpkg", "bands_flex")

areas_flex <- st_remove_holes(bands_flex) %>%
  group_by(KN, density) %>%
  mutate(
    area_id = paste0(KN, "_", density, "_", row_number())
  ) %>%
  ungroup()

#st_write(areas_flex, "geodata/zentrale_orte_areas.gpkg", "areas_flex_optional", append = FALSE)

areas_flex <- st_read("geodata/zentrale_orte_areas.gpkg", "areas_flex")

#What POI are in which areas? Base and optional
# --- BASE POIs ---
poi_base_join <- st_join(
  poi_flex %>% select(-KN),
  areas_flex,
  join = st_within
) %>%
  filter(!is.na(area_id))

# --- OPTIONAL POIs ---
poi_opt_join <- st_join(
  poi_flex_optional %>% select(-KN),
  areas_flex,
  join = st_within
) %>%
  filter(!is.na(area_id))

#Which categories are present in which area?
presence_base <- poi_base_join %>%
  st_drop_geometry() %>%
  distinct(area_id, category) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = category,
    values_from = present,
    values_fill = 0
  )

presence_opt <- poi_opt_join %>%
  st_drop_geometry() %>%
  distinct(area_id, category) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = category,
    values_from = present,
    values_fill = 0
  )

areas_flex_p <- areas_flex %>%
  left_join(presence_base, by = "area_id", suffix = c("", "_base")) %>%
  left_join(presence_opt, by = "area_id", suffix = c("", "_opt"))

categories_base <- unique(poi_flex$category)
categories_optional <- unique(poi_flex_optional$category)

# ensure only existing columns are used
base_cols <- intersect(categories_base, names(areas_flex_p))
opt_cols  <- intersect(categories_optional, names(areas_flex_p))

#How many categories are in each area?
areas_flex_p <- areas_flex_p %>%
  mutate(
    n_categories_base = rowSums(across(all_of(base_cols))),
    n_categories_opt  = rowSums(across(all_of(opt_cols)))
  )

#How many POI in the area?
poi_counts_base <- poi_base_join %>%
  st_drop_geometry() %>%
  count(area_id, name = "n_poi_base")

poi_counts_opt <- poi_opt_join %>%
  st_drop_geometry() %>%
  count(area_id, name = "n_poi_opt")

areas_flex_p <- areas_flex_p %>%
  left_join(poi_counts_base, by = "area_id") %>%
  left_join(poi_counts_opt, by = "area_id") %>%
  mutate(
    n_poi_base = replace_na(n_poi_base, 0),
    n_poi_opt  = replace_na(n_poi_opt, 0),
    n_categories_base = replace_na(n_categories_base, 0),
    n_categories_opt = replace_na(n_categories_opt, 0)
  )

areas_flex_p <- areas_flex_p %>%
  mutate(
    n_poi_total = n_poi_base + n_poi_opt,
    n_categories_total = n_categories_base + n_categories_opt
  )

st_write(
  areas_flex_p,
  "geodata/zentrale_orte_areas.gpkg",
  "areas_flex_catcount",
  append = FALSE
)

#Densest areas
core <- areas_flex_p %>%
  group_by(KN) %>%
  slice_max(density, n = 5, with_ties = TRUE) %>%
  ungroup()

#What areas are contained in what other areas?
idx <- st_within(core, areas_flex_p)

parent_tbl <- purrr::map2_dfr(
  seq_along(idx),
  idx,
  ~{
    areas_flex_p[.y, ] %>%
      st_drop_geometry() %>%
      mutate(core_id = core$area_id[.x])
  }
) %>%
  select(2,3,1,30:33,35,36) %>%
  rename_with(~ paste0("parent_", .x)) %>%
  rename("core_id" = parent_core_id)

#How does the density = 2 area compare to the core?
core_parent <- parent_tbl %>%
  left_join(core %>%
             select(1,3,30:33,36) %>%
              rename_with(~ paste0("core_", .x)), by = join_by("core_id" == "core_area_id")) %>%
  relocate(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>%
  filter(parent_density == 2)

#Select the core with the highest density that is not dominated by a core that has a parent density larger than its parent density
best <- core_parent %>%
  group_by(parent_KN) %>%
  arrange(
    desc(parent_n_categories_base),
    desc(core_density),
    desc(core_n_categories_base)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf()

#Write to disk
st_write(best, "geodata/zentrale_orte_areas.gpkg", "highestdexceptforparentn", append = FALSE)


##Everything from here: Testing, document briefly how we got here.
poi_present_flex <- st_join(poi_flex %>% select(!KN), areas_flex, join = st_within) %>%
  filter(!is.na(area_id))

presence_flex <- poi_present_flex %>%
  st_drop_geometry() %>%
  distinct(area_id, category) %>%
  mutate(present = 1) %>%
  tidyr::pivot_wider(
    names_from = category,
    values_from = present,
    values_fill = 0
  )

areas_flex_p <- left_join(areas_flex, presence_flex, by = "area_id") 

categories_base <- unique(poi_flex$category)

categories_optional <- unique(poi_flex_optional$category)


areas_flex_p$n_categories <- rowSums(
  st_drop_geometry(areas_flex_p)[, categories_base]
)

st_write(areas_flex_p, "geodata/zentrale_orte_areas.gpkg", "areas_flex_catcount")

pareto_flex <- areas_flex_p %>%
  filter(density >= 1) %>%
  group_by(KN) %>%
  group_modify(~{
    x <- .x
    
    dominated <- sapply(seq_len(nrow(x)), function(i) {
      any(
        (x$density >= x$density[i] &
           x$n_categories >= x$n_categories[i]) &
          (x$density > x$density[i] |
             x$n_categories > x$n_categories[i])
      )
    })
    
    x[!dominated, ]
  }) %>%
  ungroup()

st_write(pareto_flex, "geodata/zentrale_orte_areas.gpkg", "pareto_flex_opt")

bands_large <- st_read("geodata/zentrale_orte_bands.gpkg", "bands_large")
 
areas_large <- st_remove_holes(bands_large) %>%
    group_by(KN, density) %>%
    mutate(
      area_id = paste0(KN, "_", density, "_", row_number())
    ) %>%
    ungroup()


st_write(areas_large, "geodata/zentrale_orte_areas.gpkg", "areas_large", append = FALSE)

poi_present_large <- st_join(poi %>% select(!KN), areas_large, join = st_within)

presence_large <- poi_present_large %>%
   st_drop_geometry() %>%
   distinct(area_id, category) %>%
   mutate(present = 1) %>%
   tidyr::pivot_wider(
     names_from = category,
     values_from = present,
     values_fill = 0
   )

areas_large_p <- left_join(areas_large, presence_large, by = "area_id") 

category_names <- unique(poi$category)

areas_large_p$n_categories <- rowSums(
  st_drop_geometry(areas_large_p)[, category_names]
)

st_write(areas_large_p, "geodata/zentrale_orte_areas.gpkg", "areas_large_catcount")

pareto_large <- areas_large_p %>%
  group_by(KN) %>%
  group_modify(~{
    x <- .x
    
    dominated <- sapply(seq_len(nrow(x)), function(i) {
      any(
        (x$density >= x$density[i] &
           x$n_categories >= x$n_categories[i]) &
          (x$density > x$density[i] |
             x$n_categories > x$n_categories[i])
      )
    })
    
    x[!dominated, ]
  }) %>%
  ungroup()

st_write(pareto_large, "geodata/zentrale_orte_areas.gpkg", "pareto_large")
