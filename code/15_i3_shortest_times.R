library(here)
library(tidyverse)
library(fst)
library(data.table)
library(sf)
library(dtplyr)

gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")

zentrale_orte <- st_read("geodata/poi.gpkg", "zentraleOrte_routingdestinations_zentrenkonzept") %>%
  rename("from_id" = area_id) %>%
  mutate(ags = substr(from_id, 1,8)) %>%
  st_drop_geometry()

zensus_grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated")

zgrid_ags <- st_drop_geometry(zensus_grid) %>%
  dplyr::select(id, ags, Einwohner) %>%
  filter(!is.na(ags))

grid_base <- zgrid_ags %>%
  left_join(zentrale_orte %>% select(ags, GN, zentralitaet), join_by(ags)) %>%
  rename("gem_orig" = GN,
         "centrality_orig" = zentralitaet) %>%
  mutate(cutoff_oz = ifelse(centrality_orig == "Grundzentrum", 60, 30))

fst_dir   <- "output/i3_ttm_hourly/"
fst_files <- list.files(fst_dir, pattern = "\\.fst$", full.names = TRUE)

setDT(zentrale_orte)

#Calculate shortest travel time from each cell to each centrality category at each hour.
shortest_out <- "output/i3_shortest_hourly/"
dir.create(shortest_out, showWarnings = FALSE)

categories <- c("Grundzentrum", "Mittelzentrum", "Oberzentrum")

for (i in seq_along(fst_files)) {
  
  processing_file <- fst_files[i]
  
  filename <- basename(processing_file)
  
  pdate <- as.Date(
    str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  )
  
  phour <- as.integer(
    str_extract(filename, "(?<=_)\\d{1,2}(?=\\.fst$)")
  )
  
  tt <- read_fst(
    processing_file,
    columns = c("from_id", "to_id", "travel_time_p01"),
    as.data.table = TRUE
  )
  
  tt <- zentrale_orte[
    tt,
    on = "from_id",
    nomatch = 0L
  ]
  
  min_tt <- lazy_dt(tt) %>%
    rename("centrality_dest" = zentralitaet) %>%
    filter(!is.na(travel_time_p01)) %>%
    group_by(to_id, centrality_dest) %>%
    filter(travel_time_p01 == min(travel_time_p01)) %>%
    ungroup() %>%
    as.data.frame() %>%
    rename(
      central_place = from_id,
      GN_dest = GN,
      grid_id = to_id
    ) %>%
    dplyr::select(grid_id,
                  travel_time_p01,
                  centrality_dest,
                  central_place,
                  GN_dest
                  )
  
  min_tt_wide <- min_tt %>%
    pivot_wider(
      names_from = centrality_dest,
      values_from = c(travel_time_p01, central_place, GN_dest),
      names_glue = "{.value}_{centrality_dest}",
      id_cols = grid_id,
      values_fn = list(
        travel_time_p01 = min,
        central_place = ~ first(.x),
        GN_dest = ~ first(.x)
      )
    )
  
  best_min <- min_tt_wide %>%
    mutate(
      mind_oz = travel_time_p01_Oberzentrum,
      mind_mz = pmin(
        travel_time_p01_Oberzentrum,
        travel_time_p01_Mittelzentrum,
        na.rm = TRUE
      ),
      mind_gz = pmin(
        travel_time_p01_Oberzentrum,
        travel_time_p01_Mittelzentrum,
        travel_time_p01_Grundzentrum,
        na.rm = TRUE
      )
    ) %>%
    dplyr::select(grid_id, mind_gz, mind_mz, mind_oz)
  
  grid_tt <- grid_base %>%
    left_join(best_min, join_by(id == grid_id)) %>%
    mutate(
      date = pdate,
      hour = phour
    ) %>%
    mutate(
      capt_gz = mind_gz <= 30,
      capt_mz = mind_mz <= 30,
      capt_oz = mind_oz <= cutoff_oz,
      capt_mz_sb = mind_oz <= 45,
    ) %>%
    mutate(
      across(starts_with("capt"), ~ coalesce(.x, FALSE))
    ) 
  
  outfile <- file.path(
    shortest_out,
    paste0(format(pdate, "%Y-%m-%d"), "_", phour, ".fst")
  )
  
  write_fst(
    grid_tt,
    outfile,
    compress = 50
  )
  
  message(i, "/", length(fst_files), ": ", basename(processing_file))
}

processed_files <- list.files(
  shortest_out,
  pattern = "\\.fst$",
  full.names = TRUE
)

grid_tt_full <- data.table::rbindlist(
  lapply(
    processed_files,
    function(f) {
      read_fst(f, as.data.table = TRUE)
    }
  )
) #%>%
  rename("dest_centrality" = zentralitaet)

write_fst(grid_tt_full, "output/i3_shortest_hourly/full/captured_mintt.fst")
rm(grid_tt_full)
gc()