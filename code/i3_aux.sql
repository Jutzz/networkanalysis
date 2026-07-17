-- ============================================================
-- 03_schema_dimensions.sql
-- Dimension tables: municipalities, central_places, census_cells
-- + the precomputed 50km cell-to-place distance table
-- ============================================================

CREATE TABLE IF NOT EXISTS municipalities (
    gemeindeschluessel VARCHAR PRIMARY KEY,
    name               VARCHAR,
    zentralitaet       VARCHAR,   -- raw value: Grundzentrum / Mittelzentrum / Oberzentrum
    centrality_class   UTINYINT,  -- 1 = Low (Grundzentrum), 2 = Medium (Mittelzentrum), 3 = High (Oberzentrum)
    regbez             BOOLEAN    -- TRUE if this municipality is inside the area of interest (not just the buffer)
);

CREATE TABLE IF NOT EXISTS central_places (
    from_key           INTEGER PRIMARY KEY,   -- matches from_id_lookup.from_key
    area_id            VARCHAR,               -- original id, = from_id_lookup.from_id
    gemeindeschluessel VARCHAR,               -- = KN
    centrality_class   UTINYINT,              -- inherited from municipalities via gemeindeschluessel
    regbez             BOOLEAN                -- inherited from municipalities: is this place inside the area of interest
);

-- gemeindeschluessel and centrality_class are NULL when out_of_scope is TRUE
-- (cell near the buffer edge, no valid municipality match)
CREATE TABLE IF NOT EXISTS census_cells (
    to_key             INTEGER PRIMARY KEY,   -- matches to_id_lookup.to_key
    cell_id            VARCHAR,               -- original id, = to_id_lookup.to_id
    gemeindeschluessel VARCHAR,               -- assigned via spatial join, cell centroid in municipality polygon
    centrality_class   UTINYINT,              -- inherited from municipalities
    regbez             BOOLEAN,               -- inherited from municipalities: is this cell inside the area of interest
    population         DOUBLE,
    out_of_scope       BOOLEAN DEFAULT FALSE  -- TRUE for cells near the buffer edge with no valid municipality match
);

CREATE TABLE IF NOT EXISTS cell_place_dist (
    from_key    INTEGER,
    to_key      INTEGER,
    distance_km DOUBLE
);

CREATE INDEX IF NOT EXISTS idx_central_places_gem ON central_places(gemeindeschluessel);
CREATE INDEX IF NOT EXISTS idx_census_cells_gem   ON census_cells(gemeindeschluessel);
CREATE INDEX IF NOT EXISTS idx_dist_from          ON cell_place_dist(from_key);
CREATE INDEX IF NOT EXISTS idx_dist_to            ON cell_place_dist(to_key);