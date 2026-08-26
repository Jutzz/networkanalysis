# ============================================================
# 04_build_dimension_tables.R
# Builds municipalities, central_places, census_cells, and the
# cell_place_dist (50km) table from the geopackage layers.
#
# STEP 0 below only prints column names/layer names so you can confirm
# the CONFIG values further down are correct before anything is written.
# Run step 0 first, adjust CONFIG, then run the rest.
# ============================================================

library(sf)
library(data.table)
library(duckdb)
library(DBI)

# ---- CONFIG: adjust paths/layers/columns after inspecting step 0 ----

muni_gpkg   <- "geodata/dvg1nw.gpkg"
muni_layer  <- "gemeinden_regbez_25km"          # NULL = first/only layer; set explicitly if multiple layers exist
muni_key_col <- "KN"        # <- CONFIRM: municipalities' own Gemeindeschlüssel column (must match central_places$KN)
muni_name_col <- "GN"       # <- CONFIRM: municipality name column

cp_gpkg     <- "geodata/poi.gpkg"
cp_layer    <- "zentrale_orte"

grid_gpkg   <- "geodata/zensus.gpkg"
grid_layer  <- "regbez_zensus_populated_25km"
population_col <- "Einwohner"   # <- CONFIRM: population column name in census grid layer

db_path <- "output/db/i3ttm.duckdb"

# ---- STEP 0: diagnostics — run this first ----------------------------

st_layers(muni_gpkg)
st_layers(cp_gpkg)
st_layers(grid_gpkg)

muni_peek <- st_read(muni_gpkg, layer = muni_layer, quiet = TRUE)
cp_peek   <- st_read(cp_gpkg,   layer = cp_layer,   quiet = TRUE)
grid_peek <- st_read(grid_gpkg, layer = grid_layer, quiet = TRUE)

message("municipalities columns: ", paste(names(muni_peek), collapse = ", "))
message("central_places columns: ", paste(names(cp_peek), collapse = ", "))
message("census_grid columns: ",    paste(names(grid_peek), collapse = ", "))

message("municipalities CRS: ", st_crs(muni_peek)$input)
message("central_places CRS: ", st_crs(cp_peek)$input)
message("census_grid CRS: ",    st_crs(grid_peek)$input)

stopifnot(muni_key_col %in% names(muni_peek))
stopifnot(population_col %in% names(grid_peek))
stopifnot("regbez" %in% names(muni_peek))

rm(muni_peek, cp_peek, grid_peek); gc()

# ---- connect & create schema ------------------------------------------

con <- dbConnect(duckdb(), db_path)

schema_stmts <- strsplit(paste(readLines("code/i3_aux.sql"), collapse = "\n"), ";")[[1]]
for (stmt in schema_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

from_lookup <- as.data.table(dbGetQuery(con, "SELECT from_key, from_id FROM from_id_lookup"))
to_lookup   <- as.data.table(dbGetQuery(con, "SELECT to_key,   to_id   FROM to_id_lookup"))

# ---- STEP 1: municipalities --------------------------------------------

message("Loading municipalities ...")

muni_sf <- st_read(muni_gpkg, layer = muni_layer, quiet = TRUE)
muni_sf <- st_transform(muni_sf, 3035)   # reproject to match census/central places CRS

centrality_map <- c("Grundzentrum" = 1L, "Mittelzentrum" = 2L, "Oberzentrum" = 3L)

muni_dt <- as.data.table(st_drop_geometry(muni_sf))
setnames(muni_dt, muni_key_col, "gemeindeschluessel")
setnames(muni_dt, muni_name_col, "name")
muni_dt[, gemeindeschluessel := as.character(gemeindeschluessel)]
muni_dt[, centrality_class := centrality_map[zentralitaet]]
# NA zentralitaet = below Grundzentrum = "Nahbereich", a distinct tier (0),
# not a missing/unmapped value -- assign it explicitly
muni_dt[is.na(zentralitaet), centrality_class := 0L]
muni_dt[, regbez := as.logical(regbez)]   # TRUE = inside area of interest, FALSE = buffer only

# only warn on genuinely unexpected zentralitaet values (typos, new categories),
# NOT on NA (which is the expected/valid Nahbereich case handled above)
unmapped <- is.na(muni_dt$centrality_class) & !is.na(muni_dt$zentralitaet)
if (any(unmapped)) {
  bad <- unique(muni_dt$zentralitaet[unmapped])
  warning("Unmapped zentralitaet values found (will be NULL in centrality_class): ",
          paste(bad, collapse = ", "))
}

muni_out <- muni_dt[, .(gemeindeschluessel, name, zentralitaet, centrality_class, regbez)]
muni_out <- unique(muni_out, by = "gemeindeschluessel")

dbExecute(con, "DELETE FROM municipalities")
dbAppendTable(con, "municipalities", muni_out)
message(sprintf("  %d municipalities written (%d inside area of interest, %d buffer only, %d Nahbereich/no centrality)",
                nrow(muni_out), sum(muni_out$regbez, na.rm = TRUE),
                sum(!muni_out$regbez, na.rm = TRUE),
                sum(muni_out$centrality_class == 0L, na.rm = TRUE)))

# keep the reprojected polygons for later gpkg exports and for the
# cell -> municipality spatial join below
muni_sf_min <- muni_sf[, muni_key_col, drop = FALSE]
setnames_geom <- muni_key_col
names(muni_sf_min)[names(muni_sf_min) == muni_key_col] <- "gemeindeschluessel"
saveRDS(muni_sf_min, "output/db/municipalities_sf.rds")

# ---- STEP 2: central places ---------------------------------------------

message("Loading central places ...")

cp_sf <- st_read(cp_gpkg, layer = cp_layer, quiet = TRUE)   # already EPSG:3035
cp_dt <- as.data.table(st_drop_geometry(cp_sf))
cp_dt[, area_id := as.character(area_id)]
cp_dt[, KN := as.character(KN)]

# keep only central places that actually appear in the fact table
# Unlike census cells, central places genuinely ARE filtered down to those
# present in the fact table. A central place absent from from_id_lookup has
# zero travel-time data in any of the 210 files -- there is no O-D pair for
# it at all, so it can never contribute to any metric, and keeping it around
# would just be a place with an undefined from_key. This is not the same
# situation as the census grid cells above, which still carry meaningful
# population even without matrix data.
cp_dt <- cp_dt[area_id %in% from_lookup$from_id]

cp_dt <- merge(cp_dt, from_lookup, by.x = "area_id", by.y = "from_id", all.x = TRUE)
cp_dt <- merge(cp_dt, muni_out[, .(gemeindeschluessel, centrality_class, regbez)],
               by.x = "KN", by.y = "gemeindeschluessel", all.x = TRUE)

cp_out <- cp_dt[, .(from_key, area_id, gemeindeschluessel = KN, centrality_class, regbez)]

dbExecute(con, "DELETE FROM central_places")
dbAppendTable(con, "central_places", cp_out)
message(sprintf("  %d central places written (%d unmatched to any municipality, %d inside area of interest)",
                nrow(cp_out), sum(is.na(cp_out$centrality_class)), sum(cp_out$regbez, na.rm = TRUE)))

cp_sf_filtered <- cp_sf[cp_sf$area_id %in% cp_dt$area_id, ]
# NOTE: from_key is attached via match() on the id, not by row position --
# this is deliberately order-independent so it stays correct regardless of
# any reordering that happened in cp_dt during the merge()s above. Any code
# downstream that needs from_key for a given row of cp_sf_filtered MUST read
# it from this column, never by indexing cp_dt with a position derived from
# cp_sf_filtered (that mismatch was the root cause of the id/distance bug).
cp_sf_filtered$from_key <- cp_dt$from_key[match(cp_sf_filtered$area_id, cp_dt$area_id)]
saveRDS(cp_sf_filtered, "output/db/central_places_sf.rds")

# ---- STEP 3: census cells -------------------------------------------------

message("Loading census grid ...")

grid_sf <- st_read(grid_gpkg, layer = grid_layer, quiet = TRUE)   # already EPSG:3035
grid_sf$id <- as.character(grid_sf$id)

message(sprintf("  %d cells in the full census grid layer", nrow(grid_sf)))

# IMPORTANT: we deliberately do NOT filter grid_sf down to cells that appear
# in the travel time matrix (to_id_lookup). A cell can have real population
# but never be reached by any central place in the routed matrix (outside
# the routing engine's cutoff, network-isolated, etc.) -- dropping those
# cells here would silently understate the denominator population for every
# catchment percentage calculation, always biasing accessibility upward.
# Every grid cell is kept; cells with no matching travel_times rows will
# correctly show up later as "no data" / not reachable, while still counting
# toward the eligible population total.

# extend to_id_lookup with any grid cell ids not already registered from the
# fact table. Existing to_key values are NEVER reused or renumbered here --
# travel_times already references them -- only new ids get new keys appended.
new_ids <- setdiff(grid_sf$id, to_lookup$to_id)

if (length(new_ids) > 0) {
  message(sprintf("  %d grid cells are not present in the travel time matrix (never reached by any central place in any file) -- assigning new surrogate keys so their population still counts toward catchment denominators",
                  length(new_ids)))
  next_key <- if (nrow(to_lookup) > 0) max(to_lookup$to_key) + 1L else 1L
  new_lookup <- data.table(to_key = seq(next_key, by = 1L, length.out = length(new_ids)),
                           to_id = new_ids)
  dbAppendTable(con, "to_id_lookup", new_lookup)
  to_lookup <- rbind(to_lookup, new_lookup)
}

# centroids (grid cells are 100x100m squares, so centroid = center, exact)
grid_centroids <- st_centroid(grid_sf)

# spatial join: assign each cell to the municipality its centroid falls in
cell_muni <- st_join(grid_centroids, muni_sf_min, join = st_within, left = TRUE)

# Cells that don't match a municipality are near the edge of the buffer
# around the area of interest -- their true municipality may lie outside
# the study area entirely, so we do NOT assign a nearest municipality
# (that would silently attribute them to the wrong municipality, which
# would bias the "same municipality" and centrality-class rules).
# Instead: flag them explicitly as out_of_scope and leave gemeindeschluessel
# NULL. These cells should be excluded from accessibility calculations
# until the buffer is widened enough to cover their real municipality.
unmatched <- is.na(cell_muni$gemeindeschluessel)
cell_muni$out_of_scope <- unmatched
message(sprintf("  %d / %d cells (%.1f%%) unmatched by st_within -- flagged out_of_scope, gemeindeschluessel left NULL",
                sum(unmatched), nrow(cell_muni), 100 * mean(unmatched)))

grid_dt <- as.data.table(st_drop_geometry(cell_muni))
setnames(grid_dt, population_col, "population")
grid_dt <- merge(grid_dt, to_lookup, by.x = "id", by.y = "to_id", all.x = TRUE)
grid_dt <- merge(grid_dt, muni_out[, .(gemeindeschluessel, centrality_class, regbez)],
                 by = "gemeindeschluessel", all.x = TRUE)

cells_out <- grid_dt[, .(to_key, cell_id = id, gemeindeschluessel, centrality_class,
                         regbez, population, out_of_scope)]

if (any(cells_out$out_of_scope)) {
  message(sprintf("  NOTE: %d cells are out_of_scope and will have NULL gemeindeschluessel/centrality_class/regbez -- excluded downstream by pair_scope",
                  sum(cells_out$out_of_scope)))
}
message(sprintf("  %d cells inside area of interest, %d in buffer only",
                sum(cells_out$regbez, na.rm = TRUE), sum(!cells_out$regbez, na.rm = TRUE)))

dbExecute(con, "DELETE FROM census_cells")
dbAppendTable(con, "census_cells", cells_out)
message(sprintf("  %d census cells written", nrow(cells_out)))

saveRDS(grid_sf, "output/db/census_grid_sf.rds")
saveRDS(grid_centroids, "output/db/census_grid_centroids_sf.rds")

# ---- STEP 4: 50km cell -> central place distances -------------------------

message("Computing cell-to-place distances within 50km (this is the heavy step) ...")

# IMPORTANT: attach to_key directly onto grid_centroids via match() on the id
# (order-independent), so that when st_is_within_distance() returns row
# positions into grid_centroids, we can read the correct to_key straight off
# that same object -- never from grid_dt, whose row order is not guaranteed
# to match grid_centroids after the merge()s above (data.table's merge sorts
# by the join key by default). Indexing a reordered table by a position that
# was computed against a different object's row order was the root cause of
# central place ids / distances not matching up in the previous run.
grid_centroids$to_key <- to_lookup$to_key[match(grid_centroids$id, to_lookup$to_id)]

# st_is_within_distance uses a spatial index (STRtree), so this only computes
# exact distances for pairs that are plausibly close, not a full cross join
within_50km <- st_is_within_distance(grid_centroids, cp_sf_filtered, dist = 50000)

pair_idx <- data.table(
  cell_row  = rep(seq_along(within_50km), lengths(within_50km)),
  place_row = unlist(within_50km)
)

message(sprintf("  %s candidate pairs within 50km, computing exact distances ...",
                format(nrow(pair_idx), big.mark = ",")))

# exact pairwise distances only for the candidate pairs (chunked to limit memory)
chunk_size <- 2e6
dist_km <- numeric(nrow(pair_idx))
for (start in seq(1, nrow(pair_idx), by = chunk_size)) {
  end <- min(start + chunk_size - 1, nrow(pair_idx))
  idx <- start:end
  d <- st_distance(
    grid_centroids[pair_idx$cell_row[idx], ],
    cp_sf_filtered[pair_idx$place_row[idx], ],
    by_element = TRUE
  )
  dist_km[idx] <- as.numeric(d) / 1000
  message(sprintf("    ... %d / %d pairs", end, nrow(pair_idx)))
}

pair_idx[, distance_km := dist_km]
# read keys from the SAME sf objects used above (grid_centroids, cp_sf_filtered),
# not from grid_dt/cp_dt -- see note above
pair_idx[, to_key   := grid_centroids$to_key[cell_row]]
pair_idx[, from_key := cp_sf_filtered$from_key[place_row]]

dist_out <- pair_idx[!is.na(to_key) & !is.na(from_key), .(from_key, to_key, distance_km)]

# sanity check: every (from_key, to_key) pair should be unique -- a
# duplicate here would indicate a remaining alignment problem
dup_check <- dist_out[, .N, by = .(from_key, to_key)][N > 1]
if (nrow(dup_check) > 0) {
  warning(sprintf("  %d duplicate (from_key, to_key) pairs found in cell_place_dist -- investigate before proceeding",
                  nrow(dup_check)))
}

dbExecute(con, "DELETE FROM cell_place_dist")
dbAppendTable(con, "cell_place_dist", dist_out)
message(sprintf("  %s cell-place pairs within 50km written", format(nrow(dist_out), big.mark = ",")))

# ---- wrap up ---------------------------------------------------------------

dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
message("Dimension tables complete.")
