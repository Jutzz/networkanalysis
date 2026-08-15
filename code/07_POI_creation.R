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
poi_poly <- st_centroid(st_transform(st_read("osmdata/zentraler_ort_pois.gpkg", "multipolygons"), crs = st_crs(3857))) %>%
  mutate(osm_id = ifelse(
    is.na(osm_id),
    osm_way_id,
    osm_id))

# Selbstentworfener POI-Satz mit größerer Auswahl, allerdings keine große Veränderung zu POI nach Flex et al. 2016.
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
           government %in% c("einwohnermeldeamt")| amenity == "townhall" | grepl("Bürger|quartiersbüro|stadtteilbüro", name, ignore.case = TRUE) |
           amenity == "library" | grepl("bibliothek|bücherei", name, ignore.case = TRUE) |
           community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE) |
           isced_level == 1 | grepl("Grundschule", name, ignore.case = TRUE) | school == "primary" |
           isced_level == 2 | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) | school == "secondary"| 
           amenity == "doctors" | healthcare == "doctor" | grepl("Hausarzt", name, ignore.case = TRUE) |
           amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE) |
           amenity %in% c("post_office") |
           leisure == "fitness_centre" |
           amenity == "nursing_home" | social_facility == "nursing_home" | social_facility_for == "senior" | social_facility == "assisted_living" | grepl("Seniorenheim|Altenheim|betreutes wohnen|Seniorenresidenz", name, ignore.case = TRUE) |
           shop %in% c("supermarket", "convenience") | building == "supermarket" |
           amenity == "dentist" | healthcare == "dentist" | grepl("Zahnarzt|odonto", name, ignore.case = TRUE)|
           amenity == "bank" | amenity == "atm") %>%
  filter(!amenity %in% c("parking", "bicycle_parking", "parking_space",
                         "charging_station", "police", "trailer_parking")) %>%
  filter(str_detect(healthcare_speciality, "general|internal") | str_detect(name, "Hausarzt") | is.na(healthcare_speciality)) %>%
  mutate(category = case_when( amenity == "pharmacy" | grepl("apotheke", name, ignore.case = TRUE) ~ "Pharmacy",
                               government %in% c("einwohnermeldeamt")| amenity == "townhall" | grepl("Bürger|quartiersbüro|stadtteilbüro", name, ignore.case = TRUE) ~ "Government Office",
                               amenity == "library" | grepl("bibliothek|bücherei", name, ignore.case = TRUE) ~ "Library",
                               community_centre == "youth_centre" | grepl("Jugendzentrum", name, ignore.case = TRUE) ~ "Youth Centre",
                               isced_level == 1 | school == "primary" | grepl("Grundschule", name, ignore.case = TRUE) ~ "Primary School",
                               isced_level == 2 | school == "secondary" | grepl("Sekundarschule|Hauptschule|Realschule|Gesamtschule|Gymnasium", name, ignore.case = TRUE) ~ "Secondary School",
                               amenity == "doctors" | healthcare == "doctor" | grepl("Hausarzt", name, ignore.case = TRUE) ~ "General Practitioner",
                               amenity %in% c("kindergarten", "childcare") | grepl("kita|kindergarten|kindertagesstätte|hort", name, ignore.case = TRUE) ~ "Kindergarten / Childcare",
                               amenity == "post_office" ~ "Post Office",
                               leisure == "fitness_centre" ~ "Fitness Centre",
                               amenity == "nursing_home" | social_facility == "nursing_home" | social_facility_for == "senior" | social_facility == "assisted_living" | grepl("Seniorenheim|Altenheim|betreutes wohnen", name, ignore.case = TRUE) ~ "Nursing Home",
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
           leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE) |
           shop == "plumber" | craft == "plumber" |
           office == "accountant" | grepl("Steuerberat", name, ignore.case = TRUE) |
           railway %in% c("halt", "station") | public_transport %in% c("stop_position", "platform") | grepl("Haltepunkt|Bahnhof", name, ignore.case = TRUE) ) %>%
  filter( !amenity %in% c( "parking", "bicycle_parking", "parking_space",
                           "charging_station", "police", "trailer_parking" ) ) %>%
  filter(is.na(highway)) %>%
  filter(sport %in% c("soccer", "basketball", "running", "multi", "beachvolleyball") | is.na(sport)) %>%
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
    leisure %in% c("pitch", "track") | grepl("Sportplatz|Spielfeld|Fussballfeld|Fußballfeld", name, ignore.case = TRUE) ~ "Sports Facility",
    railway %in% c("halt", "station") | public_transport %in% c("stop_position", "platform") | grepl("Haltepunkt|Bahnhof", name, ignore.case = TRUE) ~ "Public Transport Stop",
    .default = NA_character_ )
  ) %>%
  select(1,2,category,geometry)

#Sportplätze die weniger als 100 m auseinanderliegen werden zu einem POI
#zusammengefasst, um zu verhindern, das eine einzige zentralörtliche Funktion
#die Dichte zu stark beeinflusst. So sorgen sechs Beachvolleyballplätze in
#nächster Nähe zueinander zu einer sehr hohen Dichte, sind aber in der
#zentralörtlichen Funktion nicht so bedeutend wie sechs POI unterschiedlicher
#Kategorien.

#Buffer
pitch <- poi_flex_optional %>%
  filter(category == "Sports Facility") %>%
  st_buffer(100)

#Identify overlaps
adj <- st_intersects(pitch)

#Group overlapping Buffers
g <- graph_from_adj_list(adj, mode = "all")
pitch$group <- components(g)$membership

#Merge groups
merged <- pitch %>%
  group_by(group) %>%
  summarise(
    osm_id = paste(unique(osm_id), collapse = ";"),
    do_union = TRUE,
    category = "Sports Facility"
  )

#Get Centroids
centers <- st_centroid(merged)

poi_flex_optional <- poi_flex_optional %>%
  filter(!category == "Sports Facility") %>%
  bind_rows(centers) %>%
  select(!group)

st_write(poi %>% filter(category != "Other"), "geodata/pois.gpkg", paste0("zo_POI_large_", osmdate), append = FALSE)
st_write(poi_flex, "geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate), append = FALSE)
st_write(poi_flex_optional, "geodata/pois.gpkg", paste0("zo_POI_flex_opt", osmdate), append = FALSE)