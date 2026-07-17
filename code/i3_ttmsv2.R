# ============================================================
# 03_ingest_fact_table.R
#
# Loads the .fst travel-time files into travel_times. Must run AFTER
# 02_build_grid_and_lookups.R, since from_id_lookup / to_id_lookup are now
# built from the census grid and central places geopackages (ground truth),
# not discovered from the matrix files.
#
# Every from_id/to_id in each file is validated against those lookups.
# Anything that doesn't match is logged to unmatched_ids_log.csv and
# excluded from travel_times, rather than silently ingested or silently
# dropped -- a mismatch here is a real signal that the routing data and
# the reference geodata are out of sync (different grid version, id
# formatting difference, etc.) and is worth investigating.
# ============================================================

library(fst)
library(data.table)
library(duckdb)
library(DBI)

# ---- config ---------------------------------------------------------

db_path   <- "output/db/i3ttm.duckdb"
fst_dir   <- "output/i3_ttm_hourly/"          # <- adjust to your actual folder
fst_files <- list.files(fst_dir, pattern = "\\.fst$", full.names = TRUE)

stopifnot(length(fst_files) > 0)
message(sprintf("Found %d .fst files", length(fst_files)))

# ---- connect & load ground-truth lookups -----------------------------

con <- dbConnect(duckdb(), db_path)

from_lookup <- as.data.table(dbGetQuery(con, "SELECT from_key, from_id FROM from_id_lookup"))
to_lookup   <- as.data.table(dbGetQuery(con, "SELECT to_key,   to_id   FROM to_id_lookup"))

stopifnot(
  "from_id_lookup is empty -- run 02_build_grid_and_lookups.R first" = nrow(from_lookup) > 0,
  "to_id_lookup is empty -- run 02_build_grid_and_lookups.R first"   = nrow(to_lookup) > 0
)

setkey(from_lookup, from_id)
setkey(to_lookup, to_id)

message(sprintf("Loaded ground-truth lookups: %d central places, %d census cells",
                nrow(from_lookup), nrow(to_lookup)))

# resumability: skip (day, hour) combos already loaded
loaded_dh <- dbGetQuery(con, "SELECT DISTINCT day, hour FROM travel_times")
loaded_key <- if (nrow(loaded_dh) > 0) paste(loaded_dh$day, loaded_dh$hour) else character(0)

unmatched_log <- vector("list", length(fst_files))

# ---- ingest -----------------------------------------------------------

for (i in seq_along(fst_files)) {
  f <- fst_files[i]
  dt <- read_fst(f, as.data.table = TRUE)
  
  dt[, `:=`(day = as.Date(departure), hour = as.integer(format(departure, "%H")))]
  
  this_dh <- paste(dt$day[1], dt$hour[1])
  if (this_dh %in% loaded_key) {
    message(sprintf("  [%d/%d] %s already loaded, skipping", i, length(fst_files), basename(f)))
    next
  }
  
  n_before <- nrow(dt)
  
  dt <- from_lookup[dt, on = "from_id"]   # adds from_key, NA if unmatched
  dt <- to_lookup[dt, on = "to_id"]       # adds to_key, NA if unmatched
  
  bad <- dt[is.na(from_key) | is.na(to_key)]
  if (nrow(bad) > 0) {
    unmatched_log[[i]] <- bad[, .(file = basename(f), from_id, to_id)]
    dt <- dt[!is.na(from_key) & !is.na(to_key)]
  }
  
  out <- dt[, .(from_key, to_key, day, hour,
                tt_p01 = travel_time_p01, tt_p25 = travel_time_p25,
                tt_p50 = travel_time_p50, tt_p75 = travel_time_p75,
                tt_p99 = travel_time_p99)]
  
  dbAppendTable(con, "travel_times", out)
  
  message(sprintf("  [%d/%d] loaded %s (%d / %d rows matched, day=%s hour=%02d)%s",
                  i, length(fst_files), basename(f), nrow(out), n_before,
                  dt$day[1], dt$hour[1],
                  if (nrow(bad) > 0) sprintf(" -- %d unmatched rows logged", nrow(bad)) else ""))
}

# ---- unmatched id report -----------------------------------------------

unmatched_log <- rbindlist(unmatched_log)
if (nrow(unmatched_log) > 0) {
  fwrite(unmatched_log, "output/db/unmatched_ids_log.csv")
  n_from <- uniqueN(unmatched_log$from_id)
  n_to   <- uniqueN(unmatched_log$to_id)
  warning(sprintf(
    "%s rows across all files had a from_id/to_id not present in the ground-truth lookups (%d distinct from_id, %d distinct to_id). See output/db/unmatched_ids_log.csv",
    format(nrow(unmatched_log), big.mark = ","), n_from, n_to
  ))
} else {
  message("No unmatched ids -- every from_id/to_id in the matrix matched the reference geodata.")
}

# ---- sanity checks & maintenance ----------------------------------------

n_rows <- dbGetQuery(con, "SELECT COUNT(*) n FROM travel_times")$n
n_days <- dbGetQuery(con, "SELECT COUNT(DISTINCT day) n FROM travel_times")$n
n_hours <- dbGetQuery(con, "SELECT COUNT(DISTINCT hour) n FROM travel_times")$n

message(sprintf("Done. travel_times has %s rows across %d days x %d hours",
                format(n_rows, big.mark = ","), n_days, n_hours))

dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
