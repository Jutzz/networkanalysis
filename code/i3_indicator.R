library(tidyverse)
library(sf)
library(DBI)
library(duckdb)

db_path <- "output/db/i3ttm.duckdb"

con <- dbConnect(duckdb(), db_path)

schema_stmts <- strsplit(paste(readLines("code/i3_threshold.sql"), collapse = "\n"), ";")[[1]]
for (stmt in schema_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

schema_stmts <- strsplit(paste(readLines("code/i3_views.sql"), collapse = "\n"), ";")[[1]]
for (stmt in schema_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

dbExecute(con, "CHECKPOINT")

# dbExecute(con, "CREATE OR REPLACE TABLE catchment_potential AS
# WITH cell_target AS (
#     -- DISTINCT collapses duplicate central places within the same
#     -- municipality, so a cell's population is counted once per target
#     -- municipality even if multiple central places serve it
#     SELECT DISTINCT
#         cp.gemeindeschluessel AS municipality_id,
#         cc.to_key,
#         cc.centrality_class AS origin_class,   -- 0=Nahbereich, 1=Grund, 2=Mittel, 3=Ober
#         cc.population
#     FROM cell_place_dist d
#     JOIN central_places cp ON cp.from_key = d.from_key
#     JOIN census_cells cc   ON cc.to_key   = d.to_key
#     WHERE d.distance_km <= 50
#       AND cp.regbez = TRUE               -- only report target municipalities in the area of interest
#       AND cc.gemeindeschluessel IS NOT NULL   -- exclude out_of_scope buffer-edge cells
#       AND (
#             cc.centrality_class < cp.centrality_class          -- lower-rank origin feeding upward
#             OR cc.gemeindeschluessel = cp.gemeindeschluessel    -- or the target's own municipality
#           )
# )
# SELECT
#     municipality_id,
#     origin_class,
#     SUM(population) AS population,
#     COUNT(*)         AS n_cells
# FROM cell_target
# GROUP BY municipality_id, origin_class;
#  
# CREATE INDEX IF NOT EXISTS idx_catchment_muni ON catchment_potential(municipality_id);
#  
# -- Wide/pivoted convenience version -- one row per municipality, one column
# -- per origin class, easier to join straight onto a geopackage layer later
# CREATE OR REPLACE VIEW v_catchment_potential_wide AS
# SELECT
#     municipality_id,
#     SUM(population) FILTER (WHERE origin_class = 0) AS pop_nahbereich,
#     SUM(population) FILTER (WHERE origin_class = 1) AS pop_grundzentrum,
#     SUM(population) FILTER (WHERE origin_class = 2) AS pop_mittelzentrum,
#     SUM(population) FILTER (WHERE origin_class = 3) AS pop_oberzentrum,
#     SUM(population) AS pop_total_50km
# FROM catchment_potential
# GROUP BY municipality_id;
# ")
# 
# potential <- dbGetQuery(con ,"SELECT * FROM catchment_potential")
# 
# dbExecute(con, "CREATE OR REPLACE TABLE muni_min_travel_time AS
# SELECT
#     cp_muni AS municipality_id,
#     day, hour,
#     MIN(tt_p50) AS min_tt_p50,
#     MIN(tt_p01) AS min_tt_p01,
#     MIN(tt_p99) AS min_tt_p99,
#     COUNT(DISTINCT to_key) AS n_cells_considered
# FROM travel_times_scoped
# GROUP BY cp_muni, day, hour;
#  
# CREATE INDEX IF NOT EXISTS idx_min_tt_muni ON muni_min_travel_time(municipality_id);
# ")
# 
# dbExecute(con, "CREATE OR REPLACE MACRO isochrone_cells(target_muni, target_day, target_hour) AS TABLE
#           SELECT
#           t.to_key,
#           MIN(t.tt_p50) AS travel_time_p50,
#           MIN(t.tt_p01) AS travel_time_p01,
#           MIN(t.tt_p25) AS travel_time_p25,
#           MIN(t.tt_p75) AS travel_time_p75,
#           MIN(t.tt_p99) AS travel_time_p99,
#           COUNT(*) AS n_central_places_considered
#           FROM travel_times t
#           JOIN central_places cp ON cp.from_key = t.from_key
#           WHERE cp.gemeindeschluessel = target_muni
#           AND t.day  = target_day
#           AND t.hour = target_hour
#           GROUP BY t.to_key;
#           
#           -- convenience version that also returns cell_id (original character id),
#           -- for joining directly onto a geometry layer without a separate lookup step
#           CREATE OR REPLACE MACRO isochrone_cells_with_id(target_muni, target_day, target_hour) AS TABLE
#           SELECT
#           cc.cell_id,
#           iso.*
#             FROM isochrone_cells(target_muni, target_day, target_hour) iso
#           JOIN census_cells cc ON cc.to_key = iso.to_key;"
# )
# 
# iso <- dbGetQuery(con, "
#   SELECT * FROM isochrone_cells_with_id(?, ?, ?)
# ", params = list("05314000", as.Date("2026-05-05"), 10L))
# 
# grid_sf <- readRDS("output/db/census_grid_sf.rds")
# 
# iso_map <- dplyr::left_join(grid_sf, iso, by = c("id" = "cell_id"))
# 
# st_write(iso_map, "code/temp/i3test.gpkg", layer = "isochrone_05315000_050510", delete_layer = TRUE)
# 
# 
# tt_min <- dbGetQuery(con, "SELECT * FROM muni_min_travel_time")
# 
# i3 <- dbGetQuery(con, "SELECT * FROM v_pct_reachable;")
# 
# i3_summary <- dbGetQuery(con, "SELECT * FROM v_pct_reachable_summary;")
# 
# to_lookup <- dbGetQuery(con, "SELECT * FROM to_id_lookup")
# 
# cells_cgn_scoped <- dbGetQuery(con, "SELECT * FROM travel_times_scoped WHERE cp_muni = '05315000' AND day = '2026-05-05' AND hour = 10")
# 
# cells_cgn_scoped_best <- dbGetQuery(con, "SELECT *
#                                       FROM (
#                                         SELECT *,
#                                         ROW_NUMBER() OVER (
#                                           PARTITION BY to_key
#                                           ORDER BY tt_p01
#                                         ) AS rn
#                                         FROM travel_times_scoped
#                                         WHERE cp_muni = '05315000'
#                                         AND day = '2026-05-05'
#                                       ) t
#                                     WHERE rn = 1;")
# 
# cells_id <- cells_cgn_scoped_best %>%
#   left_join(to_lookup)
# 
# grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated_25km")
# 
# tt_grid <- grid %>%
#   left_join(cells_id, by = join_by("id" == "to_id"))
# 
# st_write(st_as_sf(tt_grid), "code/temp/i3test.gpkg", "aac_0505_10c", append = FALSE)
# 
# tt_grid_s <- tt_grid %>%
#   filter(rule_name == "medium_to_high") %>%
#   filter(tt_p01 <  max_minutes)
# 
# sum(tt_grid_s$Einwohner)
# 
# sum(tt_grid_s$Einwohner)
# 
# dbExecute(con, "CREATE OR REPLACE TABLE pair_scope AS
# SELECT
#     d.from_key, d.to_key, d.distance_km,
#     cp.gemeindeschluessel AS cp_muni,
#     m_cp.centrality_class AS cp_class,
#     cc.gemeindeschluessel AS cell_muni,
#     m_cell.centrality_class AS cell_class,
#     cc.population,
#     thr.max_minutes,
#     thr.rule_name
# FROM cell_place_dist d
# JOIN central_places cp    ON cp.from_key = d.from_key
# JOIN municipalities m_cp  ON m_cp.gemeindeschluessel = cp.gemeindeschluessel
# JOIN census_cells cc      ON cc.to_key = d.to_key
# JOIN municipalities m_cell ON m_cell.gemeindeschluessel = cc.gemeindeschluessel
# JOIN od_threshold thr
#   ON (thr.same_muni = FALSE AND thr.from_class = m_cell.centrality_class AND thr.to_class = m_cp.centrality_class)
#   OR (thr.same_muni = TRUE  AND cp.gemeindeschluessel = cc.gemeindeschluessel)
# WHERE d.distance_km <= 50
#   AND cc.gemeindeschluessel IS NOT NULL")
# 
# 
# dbExecute(con,"CREATE OR REPLACE TABLE travel_times_scoped AS
# SELECT
# t.from_key, t.to_key, t.day, t.hour,
# t.tt_p01, t.tt_p25, t.tt_p50, t.tt_p75, t.tt_p99,
# ps.cp_muni, ps.cp_class, ps.cell_muni, ps.cell_class,
# ps.population, ps.max_minutes, ps.rule_name
# FROM travel_times t
# JOIN pair_scope ps ON t.from_key = ps.from_key AND t.to_key = ps.to_key;")
# 
# #dbGetQuery(con,"SELECT * FROM travel_times_scoped LIMIT 50")
# 
# #dbExecute(con,  "CREATE INDEX IF NOT EXISTS idx_tts_muni_rule ON travel_times_scoped(cp_muni, rule_name);")
# 
# 
# tt <- dbGetQuery(con, "SELECT *
#                                       FROM (
#                                         SELECT *,
#                                         ROW_NUMBER() OVER (
#                                           PARTITION BY to_key
#                                           ORDER BY tt_p01
#                                         ) AS rn
#                                         FROM travel_times_scoped
#                                         WHERE cp_muni = '05315000'
#                                         AND day = '2026-05-05'
#                                       ) t
#                                     WHERE rn = 1;")
# 
# to_lookup <- dbGetQuery(con, "SELECT * FROM to_id_lookup")
# 
# grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated_25km")
# 
# grid_id <- grid %>%
#   left_join(to_lookup, by = join_by("id" == "to_id"))
# 
# grid_tt <- grid_id %>%
#   left_join(tt)
# 
# st_write(st_as_sf(grid_tt), "code/temp/i3test.gpkg", "cgn_0505_wholeday", append = FALSE)
# 
# gtfs <- tidytransit::read_gtfs("r5core_2026-05-18_large/nofreq_de_gtfs_20260518_regbez25kmbuffer.zip")
# 
# sf <- tidytransit::gtfs_as_sf(gtfs)


dbExecute(con, "CREATE OR REPLACE MACRO cell_min_tt_by_class(target_day, target_hour) AS TABLE
SELECT
    t.to_key,
    MIN(t.tt_p01) FILTER (WHERE cp.centrality_class = 1) AS min_tt_grundzentrum,
    MIN(t.tt_p01) FILTER (WHERE cp.centrality_class = 2) AS min_tt_mittelzentrum,
    MIN(t.tt_p01) FILTER (WHERE cp.centrality_class = 3) AS min_tt_oberzentrum
FROM travel_times t
JOIN central_places cp ON cp.from_key = t.from_key
WHERE t.day = target_day AND t.hour = target_hour
GROUP BY t.to_key;
 
-- full version: every area-of-interest cell, LEFT JOINed so cells with no
-- matching travel_times data for this day/hour still appear (with NULL
-- travel times), rather than silently disappearing from the result
CREATE OR REPLACE MACRO cell_min_tt_to_centrality_classes(target_day, target_hour) AS TABLE
SELECT
    cc.to_key,
    cc.cell_id,
    cc.population,
    cc.gemeindeschluessel,
    cc.centrality_class AS cell_centrality_class,
    m.min_tt_grundzentrum,
    m.min_tt_mittelzentrum,
    m.min_tt_oberzentrum
FROM census_cells cc
LEFT JOIN cell_min_tt_by_class(target_day, target_hour) m ON m.to_key = cc.to_key
WHERE cc.regbez = TRUE;
")

tt <- dbGetQuery(con, "
  SELECT * FROM cell_min_tt_to_centrality_classes(?, ?)
", params = list(as.Date("2026-05-05"), 8L))

tt_opt <- tt %>%
  mutate(
    min_tt_mittelzentrum = pmin(min_tt_mittelzentrum, min_tt_oberzentrum, na.rm = TRUE),
    min_tt_grundzentrum  = pmin(min_tt_grundzentrum, min_tt_mittelzentrum, na.rm = TRUE)
  )

grid_tt <- grid_sf %>%
  left_join(tt_opt, by = join_by("id" == "cell_id"))

st_write(grid_tt, "code/temp/i3test.gpkg", "wholeareaminopt", append = FALSE)
