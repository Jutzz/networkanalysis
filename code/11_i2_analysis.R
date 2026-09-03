library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(data.table)
library(dtplyr)
library(fst)
library(arrow)
library(here)
library(sf)
#Only the hourly version of this is used in the results. The other ones could be
#used to calculate EQs based on BQs of means of departures per hour.
#Read helper functions
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

source("code/dataenv.R")

method <- "weekday"
method_bq <- "median"

#Get dates of representative norm- and weekdays from checking in find_valid_dates.R
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")
nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
#Choose number of dates based on method set above.
ifelse(
  method == "weekday",
  date_select <- nonholiday_weekdays_fullservice,
  date_select <- nonholiday_normdays_fullservice
)

area <- st_read("geodata/dvg1nw.gpkg", "regbez25kmbuffer")
#TODO: Make usable for any area: import full zensus and stops data, filter by generic area (limited stops and dests)
mapping_matrix <- read_csv2(here("code/erschließung_mat_long_numeric_stingy.csv"))


zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)

polygrid100 <- zensus_grid %>%
  filter(Einwohner > 0) %>%
  st_filter(st_buffer(st_transform(area, crs = st_crs(zensus_grid)), 1000), .predicate = st_intersects)

ttm <- read.csv2("output/walk_20min_zensus_stops.csv") %>%
  select(from_id, to_id, travel_time_p01)


#Mapping travel times and Bedienungsqualität to respective erschließung values
erschließung_map <- function(grid) {
  grid %>%
    mutate(
      travel_time_cut = case_when(
        travel_time_p01 <= 6 ~ 6,
        travel_time_p01 <= 10 ~ 10,
        travel_time_p01 <= 15 ~ 15,
        travel_time_p01 <= 20 ~ 20,
        travel_time_p01 <= 25 ~ 25,
        TRUE ~ NA_real_
      )
    ) %>%
    left_join(
      mapping_matrix,
      by = c(
        "Bedienungsqualität" = "Bedienungsqualität",
        "travel_time_cut" = "minutes"
      )
    ) %>%
    rename(Erschließungsqualität = indicator)
}

#Join mit Stops, errechnen der Erschließungsqualität, Auswahl der Station mit der besten Erschließungsqualität je Zelle, schreiben
##TODO: Clean up, develop strategy for filenames/metadata, make faster!
# Allgemeine Funktion für die Erschließungsqualitätsanalyse
i2_mapping <- function(ttm, matrix, departure, bq_col) {
  bq_lookup <- stops_table[["Bedienungsqualität"]]
  names(bq_lookup) <- stops_table$stop_id
  
  ttm_with_quality <- ttm %>%
    left_join(stops_table, by = c("to_id" = "stop_id")) %>%
    select(
      from_id,
      to_id,
      travel_time_p01,
      departures_per_hour,
      Bedienungsqualität
    )
  
  ttm_with_quality$Bedienungsqualität <-
    bq_lookup[ttm_with_quality$to_id]
  
  eq <- erschließung_map(ttm_with_quality)
  
  dt <- as.data.table(eq)
  
  best_connections <- dt[order(Erschließungsqualität, travel_time_p01), .SD[1], by = from_id]
  
  grid_with_times <- st_drop_geometry(polygrid100) %>%
    select(id) %>%
    left_join(
      best_connections %>%
        select(
          from_id,
          to_id,
          travel_time_p01,
          Bedienungsqualität,
          Erschließungsqualität,
          departures_per_hour
        ),
      by = c("id" = "from_id")
    )
}

i2_mapping_daily <- function(ttm, matrix, departure, bq_col) {
  mapping <- matrix
  
  bq_lookup <- stops_table[["Bedienungsqualität"]]
  names(bq_lookup) <- stops_table$stop_id
  
  ttm_with_quality <- ttm %>%
    left_join(stops_table, by = c("to_id" = "stop_id")) %>%
    select(
      from_id,
      to_id,
      travel_time_p01,
      date,
      departures_per_hour,
      Bedienungsqualität
    )
  
  ttm_with_quality$Bedienungsqualität <-
    bq_lookup[ttm_with_quality$to_id]
  
  eq <- erschließung_map(ttm_with_quality)
  
  dt <- as.data.table(eq)
  
  best_connections <- dt[order(Erschließungsqualität, travel_time_p01), .SD[1], by = from_id]
  
  grid_with_times <- st_drop_geometry(polygrid100) %>%
    select(id) %>%
    left_join(
      best_connections %>%
        select(
          from_id,
          to_id,
          travel_time_p01,
          Bedienungsqualität,
          Erschließungsqualität,
          departures_per_hour
        ),
      by = c("id" = "from_id")
    )
}

i2_mapping_hourly <- function(ttm, matrix, departure, bq_col) {
  mapping <- matrix
  
  bq_lookup <- stops_table[["Bedienungsqualität"]]
  names(bq_lookup) <- stops_table$stop_id
  
  ttm_with_quality <- ttm %>%
    left_join(stops_table, by = c("to_id" = "stop_id")) %>%
    select(
      from_id,
      to_id,
      travel_time_p01,
      date,
      hour,
      departures_per_hour,
      Bedienungsqualität
    )
  
  ttm_with_quality$Bedienungsqualität <-
    bq_lookup[ttm_with_quality$to_id]
  
  eq <- erschließung_map(ttm_with_quality)
  
  dt <- as.data.table(eq)
  
  best_connections <- dt[order(Erschließungsqualität, travel_time_p01), .SD[1], by = from_id]
  
  grid_with_times <- st_drop_geometry(polygrid100) %>%
    select(id) %>%
    left_join(
      best_connections %>%
        select(
          from_id,
          to_id,
          travel_time_p01,
          Bedienungsqualität,
          Erschließungsqualität,
          departures_per_hour
        ),
      by = c("id" = "from_id")
    )
}

i2_mapping_meanhourly <- function(ttm, matrix, departure, bq_col) {
  mapping <- matrix
  
  bq_lookup <- stops_table[["Bedienungsqualität"]]
  names(bq_lookup) <- stops_table$stop_id
  
  ttm_with_quality <- ttm %>%
    left_join(stops_table, by = c("to_id" = "stop_id")) %>%
    select(
      from_id,
      to_id,
      travel_time_p01,
      hour,
      departures_per_hour,
      Bedienungsqualität
    )
  
  ttm_with_quality$Bedienungsqualität <-
    bq_lookup[ttm_with_quality$to_id]
  
  eq <- erschließung_map(ttm_with_quality)
  
  dt <- as.data.table(eq)
  
  best_connections <- dt[order(Erschließungsqualität, travel_time_p01), .SD[1], by = from_id]
  
  grid_with_times <- st_drop_geometry(polygrid100) %>%
    select(id) %>%
    left_join(
      best_connections %>%
        select(
          from_id,
          to_id,
          travel_time_p01,
          Bedienungsqualität,
          Erschließungsqualität,
          departures_per_hour
        ),
      by = c("id" = "from_id")
    )
}
#----Hourly----

table_name <- paste("hourly",
                    feed_date,
                    method,
                    min(date_select),
                    max(date_select),
                    sep = "_")

stops_core <- read_fst(paste0("output/",table_name, "_.fst"))

for (d in nonholiday_weekdays_fullservice) {
  for (h in 8:17) {
    stops_table <- stops_core[stops_core$date == as.character(d) &
                                stops_core$hour == h, ]
    
    g <- i2_mapping_hourly(ttm,
                           mapping_matrix,
                           departure = as_datetime(d) + hours(h),
                           bq_col = Bedienungsqualität)
    
    hgrid <- g %>%
      dplyr::mutate(date = as.character(d),
                    hour = h,
                    timestamp = ymd_hms(paste(d, hms::hms(hours = h)), tz = "Europe/Berlin")) %>%
      select(
        date,
        hour,
        id,
        to_id,
        travel_time_p01,
        Bedienungsqualität,
        departures_per_hour,
        Erschließungsqualität
      )
    
    fst::write_fst(hgrid, file.path("output/hourly_eq", paste0("eq_", d, "_", h, ".fst")))
  }
}


files <- list.files("output/hourly_eq/",
                    full.names = TRUE,
                    recursive = FALSE,
                    pattern = "eq_")

final <- data.table::rbindlist(lapply(files, fst::read_fst))

fst::write_fst(final, "output/hourly_eq/full/results_full_hour.fst")

#----Meanhourly----
table_name <- paste("hourmean",
                    feed_date,
                    method,
                    min(date_select),
                    max(date_select),
                    sep = "_")

stops_core <- read_fst(paste0("output/",table_name, "_.fst")) %>%
  rename("departures_per_hour" = mean_hour)

for (h in 8:17) {
  stops_table <- stops_core[stops_core$hour == h,]
  
  g <- i2_mapping_meanhourly(ttm,
                             mapping_matrix,
                             departure = h,
                             bq_col = Bedienungsqualität)
  
  hgrid <- g %>%
    dplyr::mutate(hour = h) %>%
    select(hour,
           id,
           to_id,
           travel_time_p01,
           Bedienungsqualität,
           departures_per_hour,
           Erschließungsqualität
          )
  
  fst::write_fst(hgrid, file.path("output/hourmean_eq", paste0("eq_", h, ".fst")))
}

files <- list.files("output/hourmean_eq/",
                    full.names = TRUE,
                    recursive = FALSE,
                    pattern = "eq_")

final <- data.table::rbindlist(lapply(files, fst::read_fst))

fst::write_fst(final, "output/hourmean_eq/full/results_full_hourmean.fst")
#----Daily----
table_name <- paste("daily",
                    feed_date,
                    method,
                    min(date_select),
                    max(date_select),
                    sep = "_")

stops_core <- read_fst(paste0("output/",table_name, "_.fst")) %>%
  rename("departures_per_hour" = mean_day)

for (d in nonholiday_weekdays_fullservice) {
    stops_table <- stops_core[stops_core$date == as.character(d),]
    
    g <- i2_mapping_daily(ttm,
                           mapping_matrix,
                           departure = as_datetime(d),
                           bq_col = Bedienungsqualität)
    
    hgrid <- g %>%
      dplyr::mutate(date = as.character(d),
                    timestamp = as.Date(d)) %>%
      select(
        date,
        id,
        to_id,
        travel_time_p01,
        Bedienungsqualität,
        departures_per_hour,
        Erschließungsqualität
      )
    
    fst::write_fst(hgrid, file.path("output/daily_eq", paste0("eq_", d, ".fst")))
}

files <- list.files("output/daily_eq/",
                    full.names = TRUE,
                    recursive = FALSE,
                    pattern = "eq_")

final <- data.table::rbindlist(lapply(files, fst::read_fst))

fst::write_fst(final, "output/daily_eq/full/results_full_day.fst")

#-----Total----
table_name <- paste("totalmean",
                    feed_date,
                    method,
                    min(date_select),
                    max(date_select),
                    sep = "_")

stops_core <- read_fst(paste0("output/",table_name, "_.fst")) %>%
  rename("departures_per_hour" = mean_total)

stops_table <- stops_core

g <- i2_mapping(ttm, mapping_matrix, bq_col = Bedienungsqualität)
  
hgrid <- g %>%
  dplyr::mutate(date = as.character(d),
                timestamp = as.Date(d)) %>%
  select(
    date,
    id,
    to_id,
    travel_time_p01,
    Bedienungsqualität,
    departures_per_hour,
    Erschließungsqualität
  )
  
fst::write_fst(hgrid, file.path("output/total_eq", paste0("eq_", d, ".fst")))


files <- list.files("output/total_eq/",
                    full.names = TRUE,
                    recursive = FALSE,
                    pattern = "eq_")

final <- data.table::rbindlist(lapply(files, fst::read_fst))

fst::write_fst(final, "output/total_eq/full/results_full_total.fst")



