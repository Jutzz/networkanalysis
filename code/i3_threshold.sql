-- ============================================================
-- Centrality classes:
--   0 = Nahbereich    (no formal central-place designation, below Grundzentrum)
--   1 = Low           (Grundzentrum)
--   2 = Medium        (Mittelzentrum)
--   3 = High           (Oberzentrum)
-- ============================================================

DROP TABLE IF EXISTS od_threshold;

CREATE TABLE od_threshold (
    rule_name   VARCHAR,
    from_class  UTINYINT,   -- origin (cell) municipality centrality
    to_class    UTINYINT,   -- destination (central place) municipality centrality
    same_muni   BOOLEAN,    -- TRUE only for the "own centre" rule
    max_minutes USMALLINT
);

INSERT INTO od_threshold VALUES
    ('low_to_high',        1,    3,    FALSE, 60),
    ('low_to_medium',      1,    2,    FALSE, 30),
    ('medium_to_high',     2,    3,    FALSE, 30),
    ('nahbereich_to_medium', 0,  2,    FALSE, 60),
    ('nahbereich_to_high',   0,  3,    FALSE, 90),
    ('own_centre',         NULL, NULL, TRUE,  30);