library(tidyverse)
library(tidytransit)
library(sf)
library(r5r)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

feed <- read_gtfs("feeds/filtered/nofreq_de_gtfs_20260518_regbez.zip")

gem <- st_transform(st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln"), crs = st_crs(4326))

neigh <- st_touches(gem, gem)
#For V2, maybe works later with r5r but not now.
# stops <- feed$stops
# stops_geo <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = st_crs(4326))
# 
# stops_gem <- stops_geo %>%
#   st_join(gem %>%
#             dplyr::select(GN, KN), join = st_intersects) %>%
#   mutate(area_id = ifelse(is.na(KN), "out", KN))
# 
# areas_txt <- tibble(area_id = stops_gem$area_id, area_name = stops_gem$GN) %>%
#   replace_na(replace = list("area_name" = "Ausserhalb Tarif"))
# 
# stop_areas_txt <- tibble(area_id = stops_gem$area_id, stop_id = stops_gem$stop_id)
# 
# fare_products_txt <- tribble(
#   ~fare_product_id, ~fare_product_name, ~amount, ~currency,
#   "short", "K", 2.9, "EUR",
#   "inzone", "1a", 3.5, "EUR",
#   "inzone_metro", "1b", 4, "EUR",
#   "adjacent", "2", 5.5, "EUR",
#   "regional", "3", 13.9, "EUR",
#   "out", "NRW", 25.8, "EUR"
# )
# 
# gem_KN <- gem$KN
# 
# metro_areas <- c("05315000", "05314000", "05334002")  # your three metro area_ids
# 
# fare_rules <- tidyr::crossing(
#   from = seq_len(nrow(gem)),
#   to   = seq_len(nrow(gem))
# ) %>%
#   mutate(
#     from_area_id = gem$KN[from],
#     to_area_id   = gem$KN[to],
#     fare_product_id = case_when(
#       from == to & from_area_id %in% metro_areas ~ "inzone_metro",
#       from == to                                 ~ "inzone",
#       map2_lgl(from, to, ~ .y %in% neigh[[.x]]) ~ "adjacent",
#       TRUE                                       ~ "regional"
#     )
#   ) %>%
#   dplyr::select(from_area_id, to_area_id, fare_product_id)
# 
# out_rules <- tibble(
#   from_area_id = unique(areas_txt$area_id),
#   to_area_id = "out",
#   fare_product_id = "out"
# )
# 
# out_rules_rev <- tibble(
#   from_area_id = "out",
#   to_area_id = unique(areas_txt$area_id),
#   fare_product_id = "regional"
# )
# 
# 
# fare_rules_txt <- bind_rows(
#   fare_rules,
#   out_rules,
#   out_rules_rev
# )
# 
# feed$areas <- areas_txt
# feed$stop_aras <- stop_areas_txt
# feed$fare_rules <- fare_rules_txt


stops <- feed$stops

stops_geo <- st_as_sf(
  stops,
  coords = c("stop_lon", "stop_lat"),
  crs = 4326, remove = FALSE
)

stops <- stops_geo %>%
  st_join(
    gem %>% dplyr::select(GN, KN),
    join = st_intersects
  ) %>%
  mutate(zone_id = if_else(is.na(KN), "out", KN)) %>%
  st_drop_geometry()

feed$stops <- stops

fare_rules <- crossing(
  from = seq_len(nrow(gem)),
  to   = seq_len(nrow(gem))
) %>%
  mutate(
    origin_id      = gem$KN[from],
    destination_id = gem$KN[to],
    fare_id = case_when(
      from == to & origin_id %in% metro_areas ~ "inzone_metro",
      from == to                              ~ "inzone",
      map2_lgl(from, to, ~ .y %in% neigh[[.x]]) ~ "adjacent",
      TRUE                                    ~ "regional"
    )
  ) %>%
  dplyr::select(fare_id, origin_id, destination_id)

out_rules <- tibble(
  fare_id = "out",
  origin_id = unique(stops$zone_id),
  destination_id = "out"
)

out_rules_rev <- tibble(
  fare_id = "regional",
  origin_id = "out",
  destination_id = unique(stops$zone_id)
)

fare_rules <- bind_rows(
  fare_rules,
  out_rules,
  out_rules_rev
)

fare_attributes <- tribble(
  ~fare_id, ~price, ~currency_type, ~payment_method, ~transfers,
  "short", 2.9, "EUR", 0, NA,
  "inzone", 3.5, "EUR", 0, NA,
  "inzone_metro", 4.0, "EUR", 0, NA,
  "adjacent", 5.5, "EUR", 0, NA,
  "regional", 13.9, "EUR", 0, NA,
  "out", 25.8, "EUR", 0, NA
)

feed$fare_attributes <- fare_attributes
feed$fare_rules <- fare_rules


write_gtfs(feed, "feeds/filtered/nofreq_de_gtfs_20260518_regbez_fares.zip")

st_write(stops_gem, "code/temp/stops_fareas.gpkg", append = FALSE)
plot(stops_geo["THID"])
          
f <- st_contains(stops_geo, gem)

r5r::get_gtfs_errors(r5r_network = r5_network)
r5_network <- build_network("r5core_fares/", overwrite = FALSE, verbose = TRUE)

stops_df  <- pois_fun(stops_geo, id_col = "stop_id")

ttm <- travel_time_matrix(r5r_network = r5_network, origins = sample_n(stops_df, 5000), destinations = stops_df, departure_datetime = as.POSIXct("2026-05-12 12:00:00"),
                   mode = c("WALK", "TRANSIT"),
                   max_trip_duration = 120L,
                   max_rides = 3L,
                   time_window = 60L,
                   percentiles = c(1L, 25L, 50L, 75L, 99L ),
                   draws_per_minute = 1L,
                   progress = TRUE)

from_idx <- match(ttm$from_id, stops_geo$stop_id)
to_idx   <- match(ttm$to_id, stops_geo$stop_id)

ttm$distance <- st_distance(
  stops_geo[from_idx, ],
  stops_geo[to_idx, ],
  by_element = TRUE
)

ttm_km <- ttm %>%
  mutate(distance_km = as.numeric(distance)/1000) %>%
  mutate(price_eezy =  1.77+(distance_km*0.23)) %>%
  group_by(from_id) %>%
  summarise(price_mean = mean(price_eezy),
            tt_mean = mean(travel_time_p01),
            reached = n())

ttm_geo <- ttm_km %>%
  left_join(stops_geo, by = join_by("from_id" == "stop_id")) %>%
  rename("stop_id" = from_id)

st_write(ttm_geo, "code/temp/stops_fareas.gpkg", "stops_s1000_fare")

 ggplot(ttm_geo, aes(x = reached, y =price_mean)) +
   geom_point()
