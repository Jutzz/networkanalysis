library(tidyverse)
library(tidytransit)

feed <- read_gtfs("feeds/filtered/de_gtfs_20260518_regbez.zip")

gem <- st_transform(st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln"), crs = st_crs(4326))

neigh <- st_touches(gem, gem)

stops <- feed$stops
stops_geo <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = st_crs(4326))

stops_gem <- stops_geo %>%
  st_join(gem %>%
            select(GN, KN), join = st_intersects) %>%
  mutate(area_id = ifelse(is.na(KN), "out", KN))

areas_txt <- tibble(area_id = stops_gem$area_id, area_name = stops_gem$GN) %>%
  replace_na(replace = list("area_name" = "Ausserhalb Tarif"))

stop_areas_txt <- tibble(area_id = stops_gem$area_id, stop_id = stops_gem$stop_id)

fare_products_txt <- tribble(
  ~fare_product_id, ~fare_product_name, ~amount, ~currency,
  "short", "K", 2.9, "EUR",
  "inzone", "1a", 3.5, "EUR",
  "inzone_metro", "1b", 4, "EUR",
  "adjacent", "2", 5.5, "EUR",
  "regional", "3", 13.9, "EUR"
)

gem_KN <- gem$KN

metro_areas <- c("05315000", "05314000", "05334002")  # your three metro area_ids

fare_rules_txt <- tidyr::crossing(
  from = seq_len(nrow(gem)),
  to   = seq_len(nrow(gem))
) %>%
  mutate(
    from_area_id = gem$KN[from],
    to_area_id   = gem$KN[to],
    fare_product_id = case_when(
      from == to & from_area_id %in% metro_areas ~ "inzone_metro",
      from == to                                 ~ "inzone",
      map2_lgl(from, to, ~ .y %in% neigh[[.x]]) ~ "adjacent",
      TRUE                                       ~ "regional"
    )
  ) %>%
  select(from_area_id, to_area_id, fare_product_id)

stop_dist <- st_distance(s, by_element = FALSE)

st_write(stops_gem, "code/temp/stops_fareas.gpkg", append = FALSE)
plot(stops_geo["THID"])
          
f <- st_contains(stops_geo, gem)
