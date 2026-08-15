library(sf)
library(tidyverse)
library(tidygeocoder)

gem <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_25km")
laea_grid <- st_read("geodata/grids.gpkg", "100mregbez25kmbuffer")
zensus_csv <- read_csv2("geodata/base_data/Zensus2022_Bevoelkerungszahl_100m-Gitter.csv")

zensus_cleaned <- zensus_csv %>%
  mutate(
    E_origin = x_mp_100m - 50,
    N_origin = y_mp_100m - 50,
    E_code = E_origin / 100,
    N_code = N_origin / 100,
    gitterid = sprintf(
      "100mN%dE%d",
      as.integer(N_code),
      as.integer(E_code)
    )
  ) %>%
  select(gitterid, Einwohner)

zensus_grid <- left_join(laea_grid, zensus_cleaned, by = join_by("id" == "gitterid"))
zensus_grid_populated <- zensus_grid %>%
  filter(Einwohner > 0)

st_write(zensus_grid, "geodata/grids.gpkg", "zensus_grid_100m_regbez_25km", append = FALSE)
st_write(zensus_grid_populated, "geodata/zensus.gpkg", "regbez_zensus_populated_25km", append = FALSE)

#Finding contiguous dwellings in census
adj <- st_intersects(zensus_grid_populated, zensus_grid_populated, sparse = TRUE)
adj <- lapply(seq_along(adj), function(i) setdiff(adj[[i]], i))

g   <- graph_from_adj_list(adj, mode = "all")
comp <- components(g)

zensus_grid_populated$cluster_id <- comp$membership

dwellings <- zensus_grid_populated %>%
  group_by(cluster_id) %>%
  summarise(
    total_pop = sum(Einwohner, na.rm = TRUE),
    n_cells   = n(),
    .groups   = "drop"
  ) %>%
  st_as_sf()

tocatch_gem <- st_drop_geometry(zensus_grid_populated) %>%
  left_join(st_drop_geometry(dwellings)) %>%
  rename("cluster_pop" = total_pop) %>%
  mutate(tocatch = ifelse(cluster_pop >= 200, TRUE, FALSE)) %>%
  group_by(ags) %>%
  summarise(total_pop = sum(Einwohner),
            nocatch_pop = sum(Einwohner[!tocatch]),
            nocatch_pct = nocatch_pop/total_pop,
            .groups = "drop") %>%
  filter(str_detect(ags, "^053")) %>%
  left_join(gem, by = join_by("ags" == "KN")) %>%
  select(GN, total_pop, nocatch_pct, geom) %>%
  st_as_sf()

st_write(tocatch_gem, "geodata/dwellings.gpkg", "percent_nocatch_200")

dwellings_union <- zensus_grid_populated %>%
  group_by(cluster_id) %>%
  summarise(
    total_pop = sum(Einwohner, na.rm = TRUE),
    n_cells   = n(),
    geom  = st_union(geom),   # merge cells into one polygon per cluster
    .groups   = "drop"
  ) %>%
  st_as_sf() %>%
  st_cast("MULTIPOLYGON")

dwell_centroid <- st_centroid(dwellings %>% filter(total_pop>200)) %>%
  st_transform(crs = st_crs(4326))

coords <- st_coordinates(dwell_centroid)

dwell_centroid$long <- coords[,1]
dwell_centroid$lat <- coords[,2]

coded <- reverse_geocode(dwell_centroid, lat = lat, long = long, api_url = "http://localhost:8081/reverse",min_time = 0.001, full_results = TRUE)

coded <- coded %>%
  select(cluster_id, total_pop, n_cells, geom, address, name, village, county, quarter, hamlet, city, town, city_district, neighbourhood, isolated_dwelling, municipality) %>%
  filter(address != "Deutschland")

coded$cluster_name = if (!is.na(coded$village)) {coded$village} else if (!is.na(coded$city_district)) {coded$city_district}

st_write(st_as_sf(dwellings), "geodata/dwellings.gpkg", layer = "dwellings", append = FALSE)

#reverse_geocode(dwell_centroid[1,], lat = lat, long = long, method = "osm", api_url = "http://localhost:8081/reverse")

