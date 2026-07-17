# ============================================================
# 02_build_grid_and_lookups.R
#
# Builds the census grid, central places, and municipality dimension
# tables FIRST -- directly from the geopackage layers, which are treated
# as ground truth. from_id_lookup and to_id_lookup are built here from the
# FULL grid / FULL central places layers, not by scanning the .fst travel
# time files.
#
# The fact table is ingested afterward by 03_ingest_fact_table.R, which
# validates every from_id/to_id against the lookups built here. Anything
# in the matrix that isn't in the reference geodata gets logged rather
# than silently dropped or silently given a fabricated key.
#
# STEP 0 only prints column names so you can confirm the CONFIG values
# below are correct before anything is written. Run it first.
# ============================================================

library(sf)
library(data.table)
library(dplyr)
library(duckdb)
library(DBI)

# ---- CONFIG -------------------------------------------------------------

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

# ---- STEP 0: diagnostics — run this first --------------------------------

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

# ---- connect & create schema ----------------------------------------------

con <- dbConnect(duckdb(), db_path)

ttm_stmts <- strsplit(paste(readLines("code/i3_ttms.sql"), collapse = "\n"), ";")[[1]]
for (stmt in ttm_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

aux_stmts <- strsplit(paste(readLines("code/i3_aux.sql"), collapse = "\n"), ";")[[1]]
for (stmt in aux_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

# ---- STEP 1: municipalities ------------------------------------------------

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

muni_sf_min <- muni_sf[, muni_key_col, drop = FALSE]
names(muni_sf_min)[names(muni_sf_min) == muni_key_col] <- "gemeindeschluessel"
saveRDS(muni_sf_min, "output/db/municipalities_sf.rds")

# ---- STEP 2: census grid + to_id_lookup (GROUND TRUTH) ----------------------

message("Loading census grid (ground truth for to_id_lookup) ...")

grid_sf <- st_read(grid_gpkg, layer = grid_layer, quiet = TRUE)   # already EPSG:3035
grid_sf$id <- as.character(grid_sf$id)

dup_ids <- grid_sf$id[duplicated(grid_sf$id)]
if (length(dup_ids) > 0) {
  stop(sprintf("Census grid has %d duplicate id values -- fix source data before proceeding. Examples: %s",
               length(dup_ids), paste(head(unique(dup_ids), 5), collapse = ", ")))
}

message(sprintf("  %d cells in the census grid", nrow(grid_sf)))

# the lookup IS the full grid -- no dependency on the fact table at all
to_id_sorted <- sort(grid_sf$id)
to_lookup <- data.table(to_key = seq_along(to_id_sorted), to_id = to_id_sorted)

dbExecute(con, "DELETE FROM to_id_lookup")
dbAppendTable(con, "to_id_lookup", to_lookup)
message(sprintf("  to_id_lookup built with %d entries from the census grid", nrow(to_lookup)))

grid_centroids <- st_centroid(grid_sf)
grid_centroids$to_key <- to_lookup$to_key[match(grid_centroids$id, to_lookup$to_id)]

# assign each cell to the municipality its centroid falls in
cell_muni <- st_join(grid_centroids, muni_sf_min, join = st_within, left = TRUE)

# cells with no municipality match are near the buffer edge -- their real
# municipality may lie outside the study area. Flag and leave NULL rather
# than assigning a (likely wrong) nearest municipality.
unmatched <- is.na(cell_muni$gemeindeschluessel)
cell_muni$out_of_scope <- unmatched
message(sprintf("  %d / %d cells (%.1f%%) unmatched by st_within -- flagged out_of_scope, gemeindeschluessel left NULL",
                sum(unmatched), nrow(cell_muni), 100 * mean(unmatched)))

grid_dt <- as.data.table(st_drop_geometry(cell_muni))
setnames(grid_dt, population_col, "population")
grid_dt <- merge(grid_dt, to_lookup, by.x = "id", by.y = "to_id", all.x = TRUE)
grid_dt <- merge(grid_dt, muni_out[, .(gemeindeschluessel, centrality_class, regbez)],
                 by = "gemeindeschluessel", all.x = TRUE)

cells_out <- grid_dt[, .(to_key.x, cell_id = id, gemeindeschluessel, centrality_class,
                         regbez, population, out_of_scope)] %>%
  dplyr::rename("to_key" = to_key.x)

if (any(cells_out$out_of_scope)) {
  message(sprintf("  NOTE: %d cells are out_of_scope and will have NULL gemeindeschluessel/centrality_class/regbez -- excluded downstream by pair_scope",
                  sum(cells_out$out_of_scope)))
}
message(sprintf("  %d cells inside area of interest, %d in buffer only",
                sum(cells_out$regbez, na.rm = TRUE), sum(!cells_out$regbez, na.rm = TRUE)))

dbExecute(con, "DELETE FROM census_cells")
dbAppendTable(con, "census_cells", cells_out)

saveRDS(grid_sf, "output/db/census_grid_sf.rds")
saveRDS(grid_centroids, "output/db/census_grid_centroids_sf.rds")

# ---- STEP 3: central places + from_id_lookup (GROUND TRUTH) -----------------

message("Loading central places (ground truth for from_id_lookup) ...")

cp_sf <- st_read(cp_gpkg, layer = cp_layer, quiet = TRUE)   # already EPSG:3035
cp_sf$area_id <- as.character(cp_sf$area_id)
cp_sf$KN <- as.character(cp_sf$KN)

dup_ids <- cp_sf$area_id[duplicated(cp_sf$area_id)]
if (length(dup_ids) > 0) {
  stop(sprintf("Central places has %d duplicate area_id values -- fix source data before proceeding. Examples: %s",
               length(dup_ids), paste(head(unique(dup_ids), 5), collapse = ", ")))
}

from_id_sorted <- sort(cp_sf$area_id)
from_lookup <- data.table(from_key = seq_along(from_id_sorted), from_id = from_id_sorted)

dbExecute(con, "DELETE FROM from_id_lookup")
dbAppendTable(con, "from_id_lookup", from_lookup)
message(sprintf("  from_id_lookup built with %d entries from central places", nrow(from_lookup)))

cp_sf$from_key <- from_lookup$from_key[match(cp_sf$area_id, from_lookup$from_id)]

cp_dt <- as.data.table(st_drop_geometry(cp_sf))
cp_dt <- merge(cp_dt, muni_out[, .(gemeindeschluessel, centrality_class, regbez)],
               by.x = "KN", by.y = "gemeindeschluessel", all.x = TRUE)

cp_out <- cp_dt[, .(from_key, area_id, gemeindeschluessel = KN, centrality_class, regbez)]

dbExecute(con, "DELETE FROM central_places")
dbAppendTable(con, "central_places", cp_out)
message(sprintf("  %d central places written (%d unmatched to any municipality, %d inside area of interest)",
                nrow(cp_out), sum(is.na(cp_out$centrality_class)), sum(cp_out$regbez, na.rm = TRUE)))

saveRDS(cp_sf, "output/db/central_places_sf.rds")

# ---- STEP 4: 50km cell -> central place distances ---------------------------

message("Computing cell-to-place distances within 50km (this is the heavy step) ...")

# st_is_within_distance uses a spatial index (STRtree), so this only computes
# exact distances for pairs that are plausibly close, not a full cross join.
# NOTE: the grid is now the FULL grid, so this covers every cell, not just
# ones that happen to appear in the travel-time matrix.
within_50km <- st_is_within_distance(grid_centroids, cp_sf, dist = 50000)

pair_idx <- data.table(
  cell_row  = rep(seq_along(within_50km), lengths(within_50km)),
  place_row = unlist(within_50km)
)

message(sprintf("  %s candidate pairs within 50km, computing exact distances ...",
                format(nrow(pair_idx), big.mark = ",")))

chunk_size <- 2e6
dist_km <- numeric(nrow(pair_idx))
for (start in seq(1, nrow(pair_idx), by = chunk_size)) {
  end <- min(start + chunk_size - 1, nrow(pair_idx))
  idx <- start:end
  d <- st_distance(
    grid_centroids[pair_idx$cell_row[idx], ],
    cp_sf[pair_idx$place_row[idx], ],
    by_element = TRUE
  )
  dist_km[idx] <- as.numeric(d) / 1000
  message(sprintf("    ... %d / %d pairs", end, nrow(pair_idx)))
}

pair_idx[, distance_km := dist_km]
# read keys from the SAME sf objects used above -- both to_key and from_key
# were attached via match() on the id, order-independent regardless of any
# reordering elsewhere
pair_idx[, to_key   := grid_centroids$to_key[cell_row]]
pair_idx[, from_key := cp_sf$from_key[place_row]]

dist_out <- pair_idx[!is.na(to_key) & !is.na(from_key), .(from_key, to_key, distance_km)]

dup_check <- dist_out[, .N, by = .(from_key, to_key)][N > 1]
if (nrow(dup_check) > 0) {
  warning(sprintf("  %d duplicate (from_key, to_key) pairs found in cell_place_dist -- investigate before proceeding",
                  nrow(dup_check)))
}

dbExecute(con, "DELETE FROM cell_place_dist")
dbAppendTable(con, "cell_place_dist", dist_out)
message(sprintf("  %s cell-place pairs within 50km written", format(nrow(dist_out), big.mark = ",")))

# ---- wrap up ----------------------------------------------------------------

dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
message("Grid, central places, municipalities, and lookups complete. Ready for 03_ingest_fact_table.R.")
