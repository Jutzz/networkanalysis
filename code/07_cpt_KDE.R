options(java.parameters = "-Xmx20G")
library(plyr)
library(tidyverse)
library(sf)
library(r5r)
library(here)
library(osmextract)
library(terra)
library(spatialEco)
library(smoothr)
library(nngeo)
library(units)
library(vegan)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

osmdate <- "260521"

gem <- st_transform(st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln"), crs = st_crs(3035))

# poi <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_large_", osmdate)), crs = st_crs(gem)) %>%
#   st_join(gem %>% select(KN, geom))

poi_flex <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom)) %>%
  arrange(KN) %>%
  select(osm_id, name, category, KN, amenity, atm)

poi_flex_optional <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_opt", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom)) %>%
  mutate(amenity = NA, atm = NA)


unique_kn <- unique(poi_flex$KN)
total <- length(unique_kn)

poi_flex_full <- rbind(poi_flex, poi_flex_optional)

#KDE for every muni, normalized and written as poly bands
#Flex POI set ----
for (N in unique(poi_flex_full$KN)){
  poi_f <- poi_flex_full %>%
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

st_write(areas_flex, "geodata/zentrale_orte_areas.gpkg", "areas_flex", append = FALSE)

areas_flex <- st_read("geodata/zentrale_orte_areas.gpkg", "areas_flex")

#Separating ATMs from Banks for category counting
atm <- poi_flex %>%
  filter(amenity == "bank" & atm == "yes") %>%
  mutate(amenity = "atm") %>%
  mutate(category = "ATM")

poi_flex <- rbind(poi_flex, atm)

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
  count(area_id, category, name = "n_poi") %>%
  pivot_wider(
    names_from = category,
    values_from = n_poi,
    values_fill = 0
  )

presence_opt <- poi_opt_join %>%
  st_drop_geometry() %>%
  count(area_id, category, name = "n_poi") %>%
  pivot_wider(
    names_from = category,
    values_from = n_poi,
    values_fill = 0
  )

areas_flex_p <- areas_flex %>%
  left_join(presence_base, by = "area_id", suffix = c("", "_base")) %>%
  left_join(presence_opt, by = "area_id", suffix = c("", "_opt")) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

categories_base <- unique(poi_flex$category)
categories_optional <- unique(poi_flex_optional$category)

# ensure only existing columns are used
base_cols <- intersect(categories_base, names(areas_flex_p))
opt_cols  <- intersect(categories_optional, names(areas_flex_p))

#How many categories are in each area?
areas_flex_p <- areas_flex_p %>%
  mutate(
    n_categories_base = rowSums(across(all_of(base_cols), ~ .x > 0)),
    n_categories_opt  = rowSums(across(all_of(opt_cols),  ~ .x > 0))
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

category_cols = c(base_cols, opt_cols)

#Write category count per area for visual control of later results and visualization.
st_write(
  areas_flex_p,
  "geodata/zentrale_orte_areas.gpkg",
  "areas_flex_catcount",
  append = FALSE
)

#A core is a local maximum, an area that doesnt contain another one of higher density.
contains <- st_contains(areas_flex_p)

areas_flex_p$has_child <- purrr::map_lgl(
  seq_along(contains),
  function(i) {
    idx <- setdiff(contains[[i]], i)
    
    any(
      areas_flex_p$density[idx] >
        areas_flex_p$density[i]
    )
  }
)

core <- areas_flex_p %>%
  filter(!has_child) %>%
  select(density, KN, area_id)

st_write(core %>% st_centroid(), "geodata/zentrale_orte_areas.gpkg", "cores_nochild", append = FALSE)

#Create r5 Network for walking time analysis, turn cores and POI into routable df with plain coordinates.
r5_network <- setup_r5("r5core_2026-05-18/", overwrite = FALSE)

poi_base_df <- pois_fun(poi_flex, id_col = "osm_id")
poi_optional_df <- pois_fun(poi_flex_optional, id_col = "osm_id")

core_df <- pois_fun(core %>% st_centroid(), id_col = "area_id")

#Calculate traveltime matrices from cores to POI
ttm <- travel_time_matrix(r5_network, origins = core_df, destinations = poi_base_df, mode = "WALK", max_trip_duration = 15L, percentiles = 1L, walk_speed = 4L)
ttm_opt <- travel_time_matrix(r5_network, origins = core_df, destinations = poi_optional_df, mode = "WALK", max_trip_duration = 15L, percentiles = 1L, walk_speed = 4L)

ttm_all <- bind_rows(
  ttm %>% mutate(type = "base"),
  ttm_opt %>% mutate(type = "optional")
)

#Count reached categories based on generic cutoffs.
cutoffs <- c(2, 5, 10, 15)

ttm_cutoff <- ttm_all %>%
  crossing(cutoff = cutoffs) %>%
  filter(travel_time_p01 <= cutoff)

poi_base_wide <- ttm_cutoff %>%
  filter(type == "base") %>%
  left_join(
    poi_flex %>% select(osm_id, category),
    by = c("from_id" = "osm_id"), relationship = "many-to-many"
  ) %>%
  group_by(to_id, cutoff) %>%
  summarise(
    cat = n_distinct(category),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cutoff,
    values_from = c(cat),
    names_glue = "{.value}_base_{cutoff}",
    values_fill = 0
  )

poi_opt_wide <- ttm_cutoff %>%
  filter(type == "optional") %>%
  left_join(
    poi_flex_optional %>% select(osm_id, category),
    by = c("from_id" = "osm_id")
  ) %>%
  group_by(to_id, cutoff) %>%
  summarise(
    cat = n_distinct(category),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cutoff,
    values_from = c(cat),
    names_glue = "{.value}_opt_{cutoff}",
    values_fill = 0
  )

#Add travel times to POI tables for visualizations
poi_ttm <- poi_flex %>%
  left_join(ttm, join_by("osm_id" == "from_id"))

poi_ttm_opt <- poi_flex_optional %>%
  left_join(ttm_opt, join_by("osm_id" == "from_id"))

st_write(poi_ttm, "geodata/pois.gpkg", "poi_flex_tt_15", append = FALSE)
st_write(poi_ttm_opt, "geodata/pois.gpkg", "poi_flex_opt_tt_15", append = FALSE)
  
#What categories are reached within 15 min?
core_cats <- poi_ttm %>%
  distinct(to_id, category) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = category,
    values_from = present,
    values_fill = 0
  )

core_cats_opt <- poi_ttm_opt %>%
  distinct(to_id, category) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = category,
    values_from = present,
    values_fill = 0
  )

core_cat_full <- core_cats %>%
  full_join(core_cats_opt, by = "to_id") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  filter(!is.na(to_id))

#This table contains for each core the number of categories reached in each cutoff time, a presence of the category in 15 min walk time and a score.
core_stat <- core %>%
  #number of categories by cutoff
  left_join(poi_base_wide, join_by("area_id" == "to_id")) %>%
  left_join(poi_opt_wide, join_by("area_id" == "to_id")) %>%
  mutate(
    cat_total_2 = cat_base_2 + cat_opt_2,
    cat_total_5 = cat_base_5 + cat_opt_5,
    cat_total_10 = cat_base_10 + cat_opt_10,
    cat_total_15 = cat_base_15 + cat_opt_15
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(score = (density/9 +
                    cat_total_2/26 +
                    cat_total_5/26 +
                    cat_total_10/26 +
                    cat_total_15/26)/5) %>%
  left_join(core_cat_full, join_by("area_id" == "to_id")) %>%
  group_by(KN) %>%
  mutate(
    score_max = max(score),
    score_group = abs(score-score_max) < 0.1
  ) %>%
  ungroup() %>%
  st_centroid() %>%
  left_join(gem %>% select(KN, GN, zentralitaet) %>% st_drop_geometry())

#Turn category presence into matrix for analysis of functional differenc between cores
mat <- core_cat_full %>%
  st_drop_geometry() %>%
  column_to_rownames("to_id") %>%
  as.matrix()


central_union_list <- core_stat %>%
  filter(score_group) %>%
  select(KN, area_id) %>%
  group_split(KN) %>%
  map(~{
    kn <- unique(.x$KN)
    
    ids <- .x$area_id
    
    m <- mat[ids, , drop = FALSE]
    
    tibble(
      KN = kn,
      central_union = list(colSums(m) > 0)
    )
  }) %>%
  bind_rows()

#Create lookup table for presence of all categories of primary (best scored) cores.
central_union_lookup <- setNames(
  central_union_list$central_union,
  central_union_list$KN
)

#This checks if there are any functions reachable from a core that are not given in the primary core(s) and returns the amount if so.
secondary_gain <- function(id, mat, central_union) {
  if (!(id %in% rownames(mat))) return(0)
  
  new <- mat[id, ] & !central_union
  sum(new)
}

#Calculate functional gain for cores that dont pass the 10%-Rule and keep them if they provide functional gain and dont fall below the 20%-mark.
core_stat_results <- core_stat %>%
  rowwise() %>%
  mutate(
    secondary_functional_gain = if (
      score_group
    ) {
      NA_real_
    } else {
      secondary_gain(
        area_id,
        mat,
        central_union_lookup[[as.character(KN)]]
      )
    }
  ) %>%
  ungroup() %>%
  filter(score_group | (secondary_functional_gain >= 2 & cat_base_15 >= 5 & abs(score-score_max) < 0.2)) %>%
  mutate(dist = score-score_max)


st_write(core_stat_results, "geodata/zentrale_orte_areas.gpkg", "acc_scoring_centroids_filtered", append = FALSE)
st_write(core_stat_results, "geodata/poi.gpkg", "zentrale_orte", append = FALSE)
st_write(core_stat, "geodata/zentrale_orte_areas.gpkg", "acc_scoring_centroids", append = FALSE)

# st_write(result, "geodata/zentrale_orte_areas.gpkg", "acc_scoring_greedy", append = FALSE)  
# 
# ggplot(core_stat, aes(x = zentralitaet, y = cat_total_reached)) + geom_boxplot()
# #What areas are contained in what other areas?
# idx <- st_within(core, areas_flex_p)
# 
# parent_tbl <- purrr::map2_dfr(
#   seq_along(idx),
#   idx,
#   ~{
#     areas_flex_p[.y, ] %>%
#       st_drop_geometry() %>%
#       mutate(core_id = core$area_id[.x])
#   }
# ) %>%
#   select(KN, area_id, core_id, density, shannon_norm, area, n_categories_base, n_categories_opt, n_categories_total) %>%
#   rename_with(~ paste0("parent_", .x)) %>%
#   rename("core_id" = parent_core_id) %>%
#   rename("parent_id" = parent_area_id)
# 
# density_parent <- 3
# 
# #How does the density = 3 area compare to the core?
# core_parent <- parent_tbl %>%
#   left_join(core %>%
#               select(density, shannon_norm, area_id, area, n_categories_base, n_categories_opt, n_categories_total) %>%
#               rename_with(~ paste0("core_", .x)), by = join_by("core_id" == "core_area_id")) %>%
#   relocate(parent_KN, parent_id, core_id, parent_density, core_density, parent_shannon_norm, core_shannon_norm, parent_area, core_area, parent_n_categories_base, core_n_categories_base, parent_n_categories_opt, core_n_categories_opt, parent_n_categories_total, core_n_categories_total) #%>%
#   filter(parent_density == density_parent)
# 
# st_write(core_parent, "geodata/zentrale_orte_areas.gpkg", "core_parent_nochild", append = FALSE)
# 
# 
# total_cat <- core_parent %>%
#   #filter(parent_KN == "05362004") %>%
#   group_by(parent_KN) %>%
#   mutate(
#     cat_rank = dense_rank(desc(parent_n_categories_total))
#   ) %>%
#   filter(cat_rank <= 4) %>%
#   arrange(
#     cat_rank,
#     desc(core_density),
# #    desc(core_n_categories_total)
#   ) %>%
#   group_by(parent_KN, cat_rank) %>%
#   slice(1) %>%   # break ties within each distinct category count
#   ungroup() %>%
#   st_as_sf() %>%
#   left_join(
#     st_drop_geometry(gem) %>% select(KN, GN),
#     by = join_by("parent_KN" == "KN")
#   ) %>%
#   st_centroid()
# 
# #Write to disk
# 
# 
# tcat_scores <- total_cat %>%
#   mutate(score = (core_density/9 + parent_n_categories_base/15 + core_n_categories_opt/11)/3) %>%
#   group_by(parent_KN) %>%
#   arrange(cat_rank) %>%
#   mutate(
#     score_rank1 = first(score[cat_rank == 1]),
#     distance = score - score_rank1
#   ) %>%
#   mutate(distance = replace_na(distance, 0)) %>%
#   ungroup()
# 
# #st_write(tcat_scores, "geodata/zentrale_orte_areas.gpkg", "highestdexceptforparentn_allcat_3", append = FALSE)
# 
# score_cutoff <- sd(tcat_scores$distance)
# 
# zentral <- tcat_scores %>%
#   filter(distance > -score_cutoff) %>%
#   left_join(gem %>% select(KN, zentralitaet) %>% st_drop_geometry(), join_by("parent_KN" == "KN"))
# 
# st_write(zentral, "geodata/poi.gpkg", paste0("zo_pd", density_parent, "_cut",round(score_cutoff, 2)), append = FALSE)
# 
# max_d <- core_parent %>%
#   group_by(parent_KN) %>%
#   arrange(desc(core_density), desc(parent_n_categories_total)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   st_as_sf() %>%
#   left_join(st_drop_geometry(gem) %>% select(KN,GN), by = join_by("parent_KN" == "KN")) %>%
#   st_centroid()
# 
# st_write(max_d, "geodata/zentrale_orte_areas.gpkg", "maxdmaxcat", append = FALSE)
  

#Testing with bw = 500 if there is any difference
# #Turning bands into closed areas of equal minimal density
# bands_flex <- st_read("geodata/zentrale_orte_bands.gpkg", "bands_flex_500")
# 
# areas_flex <- st_remove_holes(bands_flex) %>%
#   group_by(KN, density) %>%
#   mutate(
#     area_id = paste0(KN, "_", density, "_", row_number())
#   ) %>%
#   ungroup()
# 
# #st_write(areas_flex, "geodata/zentrale_orte_areas.gpkg", "areas_flex_500", append = FALSE)
# 
# areas_flex <- st_read("geodata/zentrale_orte_areas.gpkg", "areas_flex_500")
# 
# #What POI are in which areas? Base and optional
# # --- BASE POIs ---
# poi_base_join <- st_join(
#   poi_flex %>% select(-KN),
#   areas_flex,
#   join = st_within
# ) %>%
#   filter(!is.na(area_id))
# 
# # --- OPTIONAL POIs ---
# poi_opt_join <- st_join(
#   poi_flex_optional %>% select(-KN),
#   areas_flex,
#   join = st_within
# ) %>%
#   filter(!is.na(area_id))
# 
# #Which categories are present in which area?
# presence_base <- poi_base_join %>%
#   st_drop_geometry() %>%
#   distinct(area_id, category) %>%
#   mutate(present = 1) %>%
#   pivot_wider(
#     names_from = category,
#     values_from = present,
#     values_fill = 0
#   )
# 
# presence_opt <- poi_opt_join %>%
#   st_drop_geometry() %>%
#   distinct(area_id, category) %>%
#   mutate(present = 1) %>%
#   pivot_wider(
#     names_from = category,
#     values_from = present,
#     values_fill = 0
#   )
# 
# areas_flex_p <- areas_flex %>%
#   left_join(presence_base, by = "area_id", suffix = c("", "_base")) %>%
#   left_join(presence_opt, by = "area_id", suffix = c("", "_opt"))
# 
# categories_base <- unique(poi_flex$category)
# categories_optional <- unique(poi_flex_optional$category)
# 
# # ensure only existing columns are used
# base_cols <- intersect(categories_base, names(areas_flex_p))
# opt_cols  <- intersect(categories_optional, names(areas_flex_p))
# 
# #How many categories are in each area?
# areas_flex_p <- areas_flex_p %>%
#   mutate(
#     n_categories_base = rowSums(across(all_of(base_cols))),
#     n_categories_opt  = rowSums(across(all_of(opt_cols)))
#   )
# 
# #How many POI in the area?
# poi_counts_base <- poi_base_join %>%
#   st_drop_geometry() %>%
#   count(area_id, name = "n_poi_base")
# 
# poi_counts_opt <- poi_opt_join %>%
#   st_drop_geometry() %>%
#   count(area_id, name = "n_poi_opt")
# 
# areas_flex_p <- areas_flex_p %>%
#   left_join(poi_counts_base, by = "area_id") %>%
#   left_join(poi_counts_opt, by = "area_id") %>%
#   mutate(
#     n_poi_base = replace_na(n_poi_base, 0),
#     n_poi_opt  = replace_na(n_poi_opt, 0),
#     n_categories_base = replace_na(n_categories_base, 0),
#     n_categories_opt = replace_na(n_categories_opt, 0)
#   )
# 
# areas_flex_p <- areas_flex_p %>%
#   mutate(
#     n_poi_total = n_poi_base + n_poi_opt,
#     n_categories_total = n_categories_base + n_categories_opt
#   )
# 
# st_write(
#   areas_flex_p,
#   "geodata/zentrale_orte_areas.gpkg",
#   "areas_flex_catcount_500",
#   append = FALSE
# )
# 
# #Densest areas
# core <- areas_flex_p %>%
#   group_by(KN) %>%
#   slice_max(density, n = 5, with_ties = TRUE) %>%
#   ungroup()
# 
# #What areas are contained in what other areas?
# idx <- st_within(core, areas_flex_p)
# 
# parent_tbl <- purrr::map2_dfr(
#   seq_along(idx),
#   idx,
#   ~{
#     areas_flex_p[.y, ] %>%
#       st_drop_geometry() %>%
#       mutate(core_id = core$area_id[.x])
#   }
# ) %>%
#   select(2,3,1,30:33,35,36) %>%
#   rename_with(~ paste0("parent_", .x)) %>%
#   rename("core_id" = parent_core_id)
# 
# #How does the density = 2 area compare to the core?
# core_parent <- parent_tbl %>%
#   left_join(core %>%
#               select(1,3,30:33,36) %>%
#               rename_with(~ paste0("core_", .x)), by = join_by("core_id" == "core_area_id")) %>%
#   relocate(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>%
#   filter(parent_density == 2)
# 
# #Select the core with the highest density that is not dominated by a core that has a parent density larger than its parent density
# best <- core_parent %>%
#   group_by(parent_KN) %>%
#   arrange(
#     desc(parent_n_categories_total),
#     desc(core_density),
#     desc(core_n_categories_total)
#   ) %>%
#   slice(1) %>%
#   ungroup() %>%
#   st_as_sf()
# 
# #Write to disk
# st_write(best, "geodata/zentrale_orte_areas.gpkg", "highestdexceptforparentn_allcat_500", append = FALSE)
##Everything from here: Testing, document briefly how we got here.
# poi_present_flex <- st_join(poi_flex %>% select(!KN), areas_flex, join = st_within) %>%
#   filter(!is.na(area_id))
# 
# presence_flex <- poi_present_flex %>%
#   st_drop_geometry() %>%
#   distinct(area_id, category) %>%
#   mutate(present = 1) %>%
#   tidyr::pivot_wider(
#     names_from = category,
#     values_from = present,
#     values_fill = 0
#   )
# 
# areas_flex_p <- left_join(areas_flex, presence_flex, by = "area_id") 
# 
# categories_base <- unique(poi_flex$category)
# 
# categories_optional <- unique(poi_flex_optional$category)
# 
# 
# areas_flex_p$n_categories <- rowSums(
#   st_drop_geometry(areas_flex_p)[, categories_base]
# )
# 
# st_write(areas_flex_p, "geodata/zentrale_orte_areas.gpkg", "areas_flex_catcount")
# 
# pareto_flex <- areas_flex_p %>%
#   filter(density >= 1) %>%
#   group_by(KN) %>%
#   group_modify(~{
#     x <- .x
#     
#     dominated <- sapply(seq_len(nrow(x)), function(i) {
#       any(
#         (x$density >= x$density[i] &
#            x$n_categories >= x$n_categories[i]) &
#           (x$density > x$density[i] |
#              x$n_categories > x$n_categories[i])
#       )
#     })
#     
#     x[!dominated, ]
#   }) %>%
#   ungroup()
# 
# st_write(pareto_flex, "geodata/zentrale_orte_areas.gpkg", "pareto_flex_opt")
# 
# 
# d <- sf.kde(st_transform(x = poi_flex %>% filter(str_detect(KN, "053")), crs = st_crs(gem)),  res = 50, bw = 1000, ref = gem)
