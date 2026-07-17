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

i3 <- dbGetQuery(con, "SELECT * FROM v_pct_reachable;")

i3_summary <- dbGetQuery(con, "SELECT * FROM v_pct_reachable_summary;")

to_lookup <- dbGetQuery(con, "SELECT * FROM to_id_lookup")

cells_cgn_scoped <- dbGetQuery(con, "SELECT * FROM travel_times_scoped WHERE cp_muni = '05315000' AND day = '2026-05-05' AND hour = 10")

cells_cgn_scoped_best <- dbGetQuery(con, "SELECT *
                                      FROM (
                                        SELECT *,
                                        ROW_NUMBER() OVER (
                                          PARTITION BY to_key
                                          ORDER BY tt_p01
                                        ) AS rn
                                        FROM travel_times_scoped
                                        WHERE cp_muni = '05315000'
                                        AND day = '2026-05-05'
                                        AND hour = 10
                                      ) t
                                    WHERE rn = 1;")

cells_id <- cells_cgn_scoped_best %>%
  left_join(to_lookup)

grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated_25km")

tt_grid <- grid %>%
  left_join(cells_id, by = join_by("id" == "to_id"))

st_write(st_as_sf(tt_grid), "code/temp/i3test.gpkg", "aac_0505_10c", append = FALSE)

tt_grid_s <- tt_grid %>%
  filter(rule_name == "medium_to_high") %>%
  filter(tt_p01 <  max_minutes)

sum(tt_grid_s$Einwohner)

sum(tt_grid_s$Einwohner)

dbExecute(con, "CREATE OR REPLACE TABLE pair_scope AS
SELECT
    d.from_key, d.to_key, d.distance_km,
    cp.gemeindeschluessel AS cp_muni,
    m_cp.centrality_class AS cp_class,
    cc.gemeindeschluessel AS cell_muni,
    m_cell.centrality_class AS cell_class,
    cc.population,
    thr.max_minutes,
    thr.rule_name
FROM cell_place_dist d
JOIN central_places cp    ON cp.from_key = d.from_key
JOIN municipalities m_cp  ON m_cp.gemeindeschluessel = cp.gemeindeschluessel
JOIN census_cells cc      ON cc.to_key = d.to_key
JOIN municipalities m_cell ON m_cell.gemeindeschluessel = cc.gemeindeschluessel
JOIN od_threshold thr
  ON (thr.same_muni = FALSE AND thr.from_class = m_cell.centrality_class AND thr.to_class = m_cp.centrality_class)
  OR (thr.same_muni = TRUE  AND cp.gemeindeschluessel = cc.gemeindeschluessel)
WHERE d.distance_km <= 50
  AND cc.gemeindeschluessel IS NOT NULL")


dbExecute(con,"CREATE OR REPLACE TABLE travel_times_scoped AS
SELECT
t.from_key, t.to_key, t.day, t.hour,
t.tt_p01, t.tt_p25, t.tt_p50, t.tt_p75, t.tt_p99,
ps.cp_muni, ps.cp_class, ps.cell_muni, ps.cell_class,
ps.population, ps.max_minutes, ps.rule_name
FROM travel_times t
JOIN pair_scope ps ON t.from_key = ps.from_key AND t.to_key = ps.to_key;")

dbGetQuery(con,"SELECT * FROM travel_times_scoped LIMIT 50")

dbExecute(con,  "CREATE INDEX IF NOT EXISTS idx_tts_muni_rule ON travel_times_scoped(cp_muni, rule_name);")

# dbExecute(con, "CREATE OR REPLACE VIEW v_pct_reachable AS
#               WITH numerator AS (
#                   SELECT
#                       ps.cp_muni AS municipality_id,
#                       t.day,
#                       t.hour,
#                       SUM(ps.population)
#                           FILTER (WHERE t.tt_p01 <= ps.max_minutes) AS pop_reachable
#                   FROM travel_times t
#                   JOIN pair_scope ps
#                     ON t.from_key = ps.from_key
#                    AND t.to_key   = ps.to_key
#                   GROUP BY ps.cp_muni, t.day, t.hour
#               ),
#               denominator AS (
#                   SELECT
#                       cp_muni AS municipality_id,
#                       SUM(population) AS pop_total
#                   FROM (
#                       SELECT DISTINCT cp_muni, to_key, population
#                       FROM pair_scope
#                   )
#                   GROUP BY cp_muni
#               )
#               SELECT
#                   n.municipality_id,
#                   n.day,
#                   n.hour,
#                   d.pop_total,
#                   n.pop_reachable,
#                   100.0 * n.pop_reachable / d.pop_total AS pct_reachable
#               FROM numerator n
#               JOIN denominator d USING (municipality_id);")


dbExecute(con,  "CREATE OR REPLACE VIEW v_pct_reachable AS
          WITH cell_best_tt AS (
            -- best (minimum) travel time per cell to ANY central place of the
            -- target municipality, per rule/day/hour
            SELECT
            cp_muni, rule_name, max_minutes, to_key, population, day, hour,
            MIN(tt_p50) AS best_tt
            FROM travel_times_scoped
            GROUP BY cp_muni, rule_name, max_minutes, to_key, population, day, hour
          ),
          numerator AS (
            SELECT
            cp_muni AS municipality_id, rule_name, day, hour,
            SUM(population) FILTER (WHERE best_tt <= max_minutes) AS pop_reachable
            FROM cell_best_tt
            GROUP BY cp_muni, rule_name, day, hour
          ),
          denominator AS (
            -- eligible population for each (municipality, rule): every in-scope cell
            -- counted once, regardless of how many central places serve it
            SELECT
            cp_muni AS municipality_id, rule_name, SUM(population) AS pop_total
            FROM (
              SELECT DISTINCT cp_muni, rule_name, to_key, population
              FROM pair_scope
            )
            GROUP BY cp_muni, rule_name
          )
          SELECT
          n.municipality_id, n.rule_name, n.day, n.hour,
          d.pop_total, n.pop_reachable,
          100.0 * n.pop_reachable / d.pop_total AS pct_reachable
          FROM numerator n
          JOIN denominator d USING (municipality_id, rule_name);")

f <- dbGetQuery(con, "SELECT * FROM v_pct_reachable_summary")

dbExecute(con, "CREATE OR REPLACE VIEW v_pct_reachable_summary AS
SELECT
    municipality_id, rule_name,
    AVG(pct_reachable)                                          AS mean_pct,
    STDDEV_SAMP(pct_reachable)                                  AS sd_pct,
    STDDEV_SAMP(pct_reachable) / NULLIF(AVG(pct_reachable), 0)  AS cv_pct,
    APPROX_QUANTILE(pct_reachable, 0.5)                         AS median_pct,
    MIN(pct_reachable) AS min_pct,
    MAX(pct_reachable) AS max_pct,
    COUNT(*) AS n_day_hours
FROM v_pct_reachable
GROUP BY municipality_id, rule_name;")
s


dbExecute(con, "DROP TABLE pair_scope;")
dbExecute(con, "DROP TABLE travel_times_scoped;")

schema_stmts <- strsplit(paste(readLines("code/i3_aux.sql"), collapse = "\n"), ";")[[1]]
for (stmt in schema_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}


calculate_i3  <-  function(KN){
  
}
cp <- dbGetQuery(con, "SELECT * FROM from_id_lookup") %>%
  filter(str_detect(from_id == KN)) %>%
  pull(from_key)

cp_list <- paste0("('", paste(l, collapse = "','"), "')")

travel_times <- dbGetQuery(con, paste0("SELECT * FROM  travel_times WHERE from_key IN ", cp_list,";")) %>%
  group_by(day,hour,to_key) %>%
  slice_min(tt_p01) %>%
  ungroup()

f <- dbGetQuery(con, "SELECT *
FROM cell_place_dist
WHERE to_key = 73836
ORDER BY distance_km;")  

c <- dbGetQuery(con, "SELECT
    cp_muni,
    to_key,
    day,
    hour,
    MIN(tt_p50) AS best_tt
FROM travel_times_scoped
GROUP BY
    cp_muni,
    to_key,
    day,
    hour;") %>%
  filter(cp_muni == "05315000") %>%
  filter(day == "2026-05-05") %>%
  filter(hour == "10")

t <- dbGetQuery(con, "SELECT * FROM from_id_lookup")

grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated") %>%
  left_join(t, by = join_by("id" == "to_id"))

tt_grid <- grid %>%
  left_join(c)

st_write(tt_grid, "code/temp/i3test.gpkg", "cgn_0505_10b", append = FALSE)
