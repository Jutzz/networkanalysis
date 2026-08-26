# Build the DuckDB fact table from .fst travel-time matrices.
# Pass 1: scan from_id/to_id columns only (cheap, columnar read via fst)
#         across all files -> build surrogate integer key lookups.
# Pass 2: read each file fully, map ids to surrogate keys, derive
#         day/hour from `departure`, append to the fact table.
library(tidyverse)
library(fst)
library(data.table)
library(duckdb)
library(DBI)

# ---- config -------------------------------------------------

db_path   <- "output/db/i3ttm.duckdb"
schema_sql <- "code/i3_ttms.sql"
fst_dir   <- "output/i3_ttm_hourly/"          # <- adjust to your actual folder
fst_files <- list.files(fst_dir, pattern = "\\.fst$", full.names = TRUE)

stopifnot(length(fst_files) > 0)
message(sprintf("Found %d .fst files", length(fst_files)))

# ---- connect & create schema --------------------------------

con <- dbConnect(duckdb(), db_path)

schema_stmts <- strsplit(paste(readLines(schema_sql), collapse = "\n"), ";")[[1]]
for (stmt in schema_stmts) {
  stmt <- trimws(stmt)
  if (nchar(stmt) > 0) dbExecute(con, stmt)
}

# ---- PASS 1: build ID lookups --------------------------------
# fst allows reading a subset of columns without touching the rest of the
# file on disk, so this pass is fast even though it opens all 210 files.

message("Pass 1: collecting distinct from_id / to_id values ...")

from_ids <- vector("list", length(fst_files))
to_ids   <- vector("list", length(fst_files))

for (i in seq_along(fst_files)) {
  ids <- read_fst(fst_files[i], columns = c("from_id", "to_id"), as.data.table = TRUE)
  from_ids[[i]] <- unique(ids$from_id)
  to_ids[[i]]   <- unique(ids$to_id)
  if (i %% 20 == 0) message(sprintf("  scanned %d / %d files", i, length(fst_files)))
}

from_id_unique <- sort(unique(unlist(from_ids)))
to_id_unique   <- sort(unique(unlist(to_ids)))

message(sprintf("  %d unique from_id (central places), %d unique to_id (census cells)",
                 length(from_id_unique), length(to_id_unique)))

from_lookup_df <- data.table(from_key = seq_along(from_id_unique), from_id = from_id_unique)
to_lookup_df   <- data.table(to_key   = seq_along(to_id_unique),   to_id   = to_id_unique)

# only populate if empty (idempotent / resumable script)
existing_from <- dbGetQuery(con, "SELECT COUNT(*) n FROM from_id_lookup")$n
if (existing_from == 0) {
  dbWriteTable(con, "from_id_lookup", from_lookup_df, append = TRUE)
  dbWriteTable(con, "to_id_lookup",   to_lookup_df,   append = TRUE)
  message("  lookup tables written to DuckDB")
} else {
  message("  lookup tables already populated, skipping write")
}

# pull lookups back as data.tables for fast in-R joins during pass 2
from_lookup <- as.data.table(dbGetQuery(con, "SELECT from_key, from_id FROM from_id_lookup"))
to_lookup   <- as.data.table(dbGetQuery(con, "SELECT to_key, to_id FROM to_id_lookup"))
setkey(from_lookup, from_id)
setkey(to_lookup, to_id)

# ---- PASS 2: load fact rows ----------------------------------

message("Pass 2: loading fact table ...")

# check which (day,hour) combos are already loaded, so re-running the
# script after an interruption doesn't duplicate data
loaded_dh <- dbGetQuery(con, "SELECT DISTINCT day, hour FROM travel_times")
loaded_key <- if (nrow(loaded_dh) > 0) paste(loaded_dh$day, loaded_dh$hour) else character(0)

for (i in seq_along(fst_files)) {
  f <- fst_files[i]
  dt <- read_fst(f, as.data.table = TRUE)

  # derive day/hour from departure (robust regardless of file naming)
  dt[, `:=`(day = as.Date(departure), hour = as.integer(format(departure, "%H")))]

  # skip if this day/hour is already in the DB (resumability)
  this_dh <- paste(dt$day[1], dt$hour[1])
  if (this_dh %in% loaded_key) {
    message(sprintf("  [%d/%d] %s already loaded, skipping", i, length(fst_files), basename(f)))
    next
  }

  # map character ids -> surrogate integer keys
  setkey(dt, from_id)
  dt <- from_lookup[dt, on = "from_id"]      # adds from_key
  setkey(dt, to_id)
  dt <- to_lookup[dt, on = "to_id"]          # adds to_key

  out <- dt[, .(from_key, to_key, day, hour,
                tt_p01 = travel_time_p01, tt_p25 = travel_time_p25,
                tt_p50 = travel_time_p50, tt_p75 = travel_time_p75,
                tt_p99 = travel_time_p99)]

  dbAppendTable(con, "travel_times", out)

  message(sprintf("  [%d/%d] loaded %s (%d rows, day=%s hour=%02d)",
                   i, length(fst_files), basename(f), nrow(out), dt$day[1], dt$hour[1]))
}

# ---- sanity checks & maintenance -------------------------------

n_rows <- dbGetQuery(con, "SELECT COUNT(*) n FROM travel_times")$n
n_days <- dbGetQuery(con, "SELECT COUNT(DISTINCT day) n FROM travel_times")$n
n_hours <- dbGetQuery(con, "SELECT COUNT(DISTINCT hour) n FROM travel_times")$n

message(sprintf("Done. travel_times has %s rows across %d days x %d hours",
                 format(n_rows, big.mark = ","), n_days, n_hours))

# checkpoint to disk and reclaim any WAL space
dbExecute(con, "CHECKPOINT")

dbDisconnect(con, shutdown = TRUE)
