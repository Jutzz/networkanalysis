-- ============================================================
-- 05_pair_scope_and_scoped_facts.sql
-- PREREQUISITE: run 06_schema_od_threshold.sql first (creates/refreshes
-- the od_threshold table, now including the Nahbereich tier)
--
-- Fixes vs. the draft:
--   1. Drop unused pair_id VARCHAR columns (never used in joins)
--   2. Materialize travel_times x pair_scope ONCE into travel_times_scoped
--      instead of joining the full 650M-row fact table on every query
--   3. Split numerator/denominator by rule_name (the 4 metrics must stay separate)
--   4. Avoid double-counting population when a municipality has >1 central place
--      (MIN travel time per cell/rule before summing population)
-- ============================================================

-- --- 1. clean up unused columns from the draft -----------------------------

ALTER TABLE travel_times DROP COLUMN IF EXISTS pair_id;
DROP TABLE IF EXISTS pair_scope;

-- --- 2. pair_scope ---------------------------------------------------------
-- Targets (central place's own municipality) are restricted to regbez = TRUE
-- (inside the area of interest). Cells (sources) are NOT restricted by
-- regbez -- population in buffer municipalities still counts toward
-- reachability of an in-scope central place, it just isn't itself reported
-- as a target municipality.

CREATE OR REPLACE TABLE pair_scope AS
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
  AND cc.gemeindeschluessel IS NOT NULL   -- exclude out_of_scope buffer-edge cells
  AND m_cp.regbez = TRUE;                 -- only report on target municipalities inside area of interest

-- --- 3. materialize the scoped fact table ONCE ------------------------------
-- This is the expensive join, but it only happens here, one time, instead of
-- on every query. Sorted by rule_name/cp_muni/day/hour so DuckDB's zone maps
-- can prune effectively for the grouped aggregations below.

CREATE OR REPLACE TABLE travel_times_scoped AS
SELECT
    t.from_key, t.to_key, t.day, t.hour,
    t.tt_p01, t.tt_p25, t.tt_p50, t.tt_p75, t.tt_p99,
    ps.cp_muni, ps.cp_class, ps.cell_muni, ps.cell_class,
    ps.population, ps.max_minutes, ps.rule_name
FROM travel_times t
JOIN pair_scope ps ON t.from_key = ps.from_key AND t.to_key = ps.to_key;

CREATE INDEX IF NOT EXISTS idx_tts_muni_rule ON travel_times_scoped(cp_muni, rule_name);

-- --- 4. corrected reachability view ------------------------------------------
-- Uses tt_p50 (median) as the reachability criterion -- CONFIRM this is what
-- you intend, swap to tt_p01/tt_p90 here if you meant best/worst case.
--
-- Fixes the double-counting issue: a cell's population is counted once per
-- (municipality, rule, day, hour) if reachable via its BEST central place in
-- that municipality, not once per central place.

CREATE OR REPLACE VIEW v_pct_reachable AS
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
JOIN denominator d USING (municipality_id, rule_name);

-- --- 5. mean + variability summary, per municipality per rule ---------------

CREATE OR REPLACE VIEW v_pct_reachable_summary AS
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
GROUP BY municipality_id, rule_name;