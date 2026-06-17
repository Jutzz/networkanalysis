library(plyr)
library(tidyverse)
library(sf)
library(here)
library(osmextract)
library(terra)
library(spatialEco)
library(smoothr)
library(nngeo)

gem <- st_transform(st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln"), crs = st_crs(3035))

poi <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_large_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))

poi_flex <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom)) %>%
  arrange(KN)

poi_flex_optional <- st_transform(st_read("geodata/pois.gpkg", paste0("zo_POI_flex_opt", osmdate)), crs = st_crs(gem)) %>%
  st_join(gem %>% select(KN, geom))


unique_kn <- unique(poi_flex$KN)
total <- length(unique_kn)



#KDE for every muni, normalized and written as poly bands
#Large POI set----
# for (N in unique(poi$KN)){
#   if(!N %in% gem$KN)
#     next
#   else
#     
#     poi_f <- poi %>%
#       filter(KN == N) %>%
#       st_transform(st_crs(3035))
#   
#   if(nrow(poi_f) == 0) 
#     next
#   else
#     
#     d <- sf.kde(poi_f, bw = 1000, res = as.numeric(st_area(gem[gem$KN == N, ])/15000000), standardize = TRUE, ref = gem[gem$KN == N, ])
#   
#   breaks <- seq(0.1, 1, by = 0.1)
#   
#   d_class <- classify(
#     d,
#     cbind(
#       breaks[-length(breaks)],
#       breaks[-1],
#       seq_along(breaks[-1])
#     )
#   )
#   
#   bands <- as.polygons(d_class, dissolve = TRUE) %>%
#     st_as_sf(crs = st_crs(3035)) %>%
#     rename("density" = lyr.1) %>%
#     filter(density >= 1) %>%
#     mutate(KN = N) %>%
#     smooth(method = "ksmooth", smoothness = 2) %>%
#     st_make_valid() %>%
#     st_cast("MULTIPOLYGON") %>%
#     st_cast("POLYGON", do_split = TRUE)
#   
#   plot(bands)
#   writeRaster(d, paste("output/kderasters/kde", N, "large.tif", sep = "_"), overwrite = TRUE)
#   st_write(bands, "geodata/zentrale_orte_bands.gpkg", layer = "bands_large", append = TRUE)
#   
#   message(N, " done. ", match(N, unique_kn), "/", total)
# }
#Flex POI set ----
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
    desc(parent_n_categories_total),
    desc(core_density),
    desc(core_n_categories_total)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf()

#Write to disk
st_write(best, "geodata/zentrale_orte_areas.gpkg", "highestdexceptforparentn_allcat", append = FALSE)

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

d <- 