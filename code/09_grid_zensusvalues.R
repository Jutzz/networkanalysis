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

#Finding contiguous dwellings in census----
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

#Calculating centrality/density index
grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated")
curve <- grid %>%
  st_drop_geometry() %>%
  group_by(ags) %>%
  arrange(desc(Einwohner), .by_group = TRUE) %>%
  mutate(
    n_cells = n(),
    total_pop = sum(Einwohner, na.rm = TRUE),
    
    # Kumulierte Bevölkerung
    cum_pop = cumsum(Einwohner),
    
    # Anteile
    area_share = row_number() / n_cells,
    pop_share = cum_pop / total_pop
  ) %>%
  ungroup() %>%
  left_join(gemeinden, join_by("ags" == "KN"))

plotly::ggplotly(ggplot(
curve %>% filter(RegioStaR7 %in% c(74,75)) ,  aes(x = area_share, y = pop_share, colour = GN, group = GN)
) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Kumulierter Flächenanteil",
    y = "Kumulierter Bevölkerungsanteil"
  ))

concentration <- curve %>%
  group_by(RegioStaR7) %>%
  arrange(area_share, .by_group = TRUE) %>%
  summarise(
    auc = sum(
      diff(area_share) *
        (head(pop_share, -1) + tail(pop_share, -1)) / 2
    ),
    concentration_index = 2 * auc - 1,
    .groups = "drop"
  )

r50_binary <- function(x, tolerance = 25, cut = 0.5, id_col = "id") {
  
  pts <- st_centroid(x)
  pop <- x$Einwohner
  target <- cut * sum(pop, na.rm = TRUE)
  
  message(paste0("Target: ", target))
  
  # Find the circle with the largest population for a given radius
  check_radius <- function(r) {
    message(paste0("Checking pop in Radius ", round(r, digits = 2)))
    
    neighbors <- st_is_within_distance(
      pts, pts, dist = r
    )
    
    populations <- vapply(
      neighbors,
      function(idx) sum(pop[idx], na.rm = TRUE),
      numeric(1)
    )
    
    winner <- which.max(populations)
    max_pop <- populations[winner]
    
    message(paste0(max_pop, " in circle."))
    
    list(
      reaches_target = max_pop >= target,
      winner = winner,
      population = max_pop,
      populations = populations
    )
  }
  
  lower <- 0
  
  # Safe upper bound: maximum distance between centroids
  bb <- st_bbox(pts)
  upper <- sqrt(
    (bb["xmax"] - bb["xmin"])^2 +
      (bb["ymax"] - bb["ymin"])^2
  )
  
  # Binary search for the minimum radius
  while ((upper - lower) > tolerance) {
    mid <- (lower + upper) / 2
    result <- check_radius(mid)
    
    if (result$reaches_target) {
      upper <- mid
    } else {
      lower <- mid
    }
  }
  
  # Recalculate at the final radius to identify the winning cell
  final_result <- check_radius(upper)
  winner <- final_result$winner
  
  # Create the circle around the winning centroid
  circle_geom <- st_buffer(pts[winner, ], dist = upper)
  
  # Determine the cell ID
  cell_id <- if (is.null(id_col)) {
    winner
  } else {
    x[[id_col]][winner]
  }
  
  list(
    radius = upper,
    cell_id = cell_id,
    population = final_result$population,
    target = target,
    center = pts[winner, ],
    circle = circle_geom
  )
}

result <- r50_binary(grid %>% filter(ags == "05366016"), cut = 0.5)
