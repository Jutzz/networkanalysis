-- ============================================================
-- 01_schema_fact_table.sql
-- Core fact table + surrogate-key lookup tables for from_id/to_id
-- ============================================================

-- Lookup tables: map original character IDs to compact integer surrogate keys.
-- These surrogate keys will later be reused by central_places (from_key)
-- and census_cells (to_key) when the dimension tables are built, so no
-- re-mapping is ever needed downstream.

CREATE TABLE IF NOT EXISTS from_id_lookup (
    from_key INTEGER PRIMARY KEY,
    from_id  VARCHAR UNIQUE NOT NULL      -- original central place id
);

CREATE TABLE IF NOT EXISTS to_id_lookup (
    to_key INTEGER PRIMARY KEY,
    to_id  VARCHAR UNIQUE NOT NULL        -- original census cell id
);

-- Fact table: one row per (from, to, day, hour).
-- ~630M rows expected (210 files x ~3M rows).
-- USMALLINT covers 0-65535 minutes, comfortably enough for travel times.
-- UTINYINT covers 0-255, enough for hour-of-day (0-23).

CREATE TABLE IF NOT EXISTS travel_times (
    from_key   INTEGER   NOT NULL,
    to_key     INTEGER   NOT NULL,
    day        DATE      NOT NULL,
    hour       UTINYINT  NOT NULL,
    tt_p01     USMALLINT,
    tt_p25     USMALLINT,
    tt_p50     USMALLINT,
    tt_p75     USMALLINT,
    tt_p99     USMALLINT
);

-- Indexes: DuckDB doesn't need B-tree indexes for scan-heavy analytical
-- queries the way Postgres does (it uses zone maps / min-max pruning on
-- sorted data automatically), but we DO want the table physically sorted
-- so that pruning is effective. This is done at ingestion time by loading
-- data in day/hour order and periodically running:
--   PRAGMA force_checkpoint;
-- We add explicit indexes only on the lookup tables, where point lookups
-- during the ID-mapping join benefit from it.

CREATE UNIQUE INDEX IF NOT EXISTS idx_from_id_lookup ON from_id_lookup(from_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_to_id_lookup   ON to_id_lookup(to_id);
