#TODO: Add (correct) timestamp to hourly files, add min/max values to summary
#table, tables showing variability between hours, between days, between weeks.
#Disaggregation by Municipality, find way to build list of contiguos areas with
#over 200 people (to be reached). Find a way to get min and max to work with NAs.
library(dplyr)
library(tidyr)
library(data.table)
library(fst)
library(arrow)
library(here)
library(sf)
library(igraph)

mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

feed_date <- "20260518"

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

#TODO: Make usable for any area: import full zensus and stops data, filter by generic area (limited stops and dests)
mapping_matrix <- read_csv2(here("code/erschließung_mat_long_numeric.csv"))

stops_table <- st_read(
  here("geodata/Bedienungsqualität.gpkg"),
  paste(feed_date, method, min(date_select), max(date_select), sep = "_")
) %>%
  mutate(
    stop_type = case_when(
      stop_type_median == 1 ~ "train",
      stop_type_median == 2 ~ "tram",
      stop_type_median == 3 ~ "bus",
      stop_type_median == 4 ~ "other",
      TRUE ~ NA_character_ # Handles any other values
    )
  ) %>%
  filter(!is.na(bq_median))

stops_id <- st_drop_geometry(stops_table) %>%
  mutate(id = as.character(stop_id))

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)

#Mapping travel times and Bedienungsqualität to respective erschließung values
erschließung_map <- function(grid) {
  grid %>%
    mutate(
      travel_time_cut = case_when(
        travel_time_p01 <= 5 ~ 5,
        travel_time_p01 <= 8 ~ 8,
        travel_time_p01 <= 11 ~ 11,
        travel_time_p01 <= 15 ~ 15,
        travel_time_p01 <= 19 ~ 19,
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
i2_mapping <- function(ttm, matrix, departure, bq_col) {
  mapping <- matrix
  
  ttm_with_quality <- ttm %>%
    left_join(stops_id %>% select(id, {{bq_col}}), by = c("to_id" = "id")) %>%
    mutate(Bedienungsqualität = ifelse(is.na({{bq_col}}), Inf, {{bq_col}}))
  
  print(unique(ttm_with_quality$Bedienungsqualität))
  
  eq <- erschließung_map(ttm_with_quality)
  
  print(eq)
  
  best_connections <- eq %>%
    group_by(from_id) %>%
    arrange(Erschließungsqualität) %>%
    slice(1) %>%
    ungroup()
  
  grid_with_times <<- polygrid100 %>%
    left_join(
      best_connections %>%
        select(from_id, to_id, travel_time_p01, Erschließungsqualität),
      by = c("id" = "from_id")
    ) %>%
    mutate(start_time = departure) %>%
    mutate(end_time = start_time + minutes(travel_time_p01)) %>%
    left_join(stops_id, by = c("to_id" = "id"))
  
  travel_times_grid <<- grid_with_times %>%
    select(id, to_id, travel_time_p01, Einwohner, stop_id, geom)
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

stops_core <- read_fst("output/hourly_20260518_weekday_2026-05-04_2026-06-26_.fst")

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
                    timestamp = ) %>%
      select(
        date,
        hour,
        id,
        to_id,
        travel_time_p01,
        departures_per_hour,
        Erschließungsqualität
      )
    
    fst::write_fst(hgrid, file.path("output/hourly_eq", paste0("eq_", d, "_", h, ".fst")))
  }
}

files <- list.files("output/hourly_eq",
                    full.names = TRUE,
                    recursive = FALSE)

final <- data.table::rbindlist(lapply(files, fst::read_fst))

fst::write_fst(final, "output/hourly_eq/full/results_full.fst")

ds <- read_fst("output/hourly_eq/full/results_full.fst")

ds$Erschließungsqualität <- replace_na(ds$Erschließungsqualität, 8)

#arrow::write_parquet(final, "output/hourly_eq/full/results_full.parquet")

#Build Summary Tables across all hours of all days-----
#ds <- open_dataset("output/hourly_eq/full/results_full.parquet")

summary <- ds %>%
  group_by(id) %>%
  summarise(
    mean_eq = mean(Erschließungsqualität, na.rm = TRUE),
    median_eq = median(Erschließungsqualität, na.rm = TRUE),
    modal_eq = mode_value(Erschließungsqualität),
    best_eq = min(Erschließungsqualität),
    worst_eq = max(Erschließungsqualität),
    n_eq = n_distinct(Erschließungsqualität, na.rm = TRUE),
    n_stops = n_distinct(to_id, na.rm = TRUE)
  )

summary %>%
  left_join(zensus_grid) %>%
  st_write("code/temp/i2test.gpkg", "i2summary", append = FALSE)



changing_ids <- ds %>%
  group_by(id) %>%
  summarise(
    n_eq = n_distinct(Erschließungsqualität),
    n_stops = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  filter(n_eq > 1) %>%
  collect()

changes_grid <- zensus_grid %>%
  left_join(changing_ids)

st_write(changes_grid, "output/indikator_02.gpkg", "changes_hours")

#Get individual slices for detailed analysis----

eq_profile <- function(id, parquet_dir = "output/hourly_eq/full/results_full.parquet") {
  ds <- open_dataset(parquet_dir)
  
  ds %>%
    filter(id == !!id) %>%
    select(date,
           hour,
           Erschließungsqualität,
           to_id,
           departures_per_hour,
           travel_time_p01) %>%
    collect() %>%
    mutate(datetime = as.POSIXct(date) + hour * 3600) %>%
    arrange(datetime)
  #
  # stops_table <- stops_core[
  #   stops_core$stop_id %in% ds$]
}

eq_slice <- function(date,
                     hour = NULL,
                     parquet_dir = "output/hourly_eq/full/results_full.parquet") {
  query <- open_dataset(parquet_dir) %>%
    filter(date == !!date)
  
  if (!is.null(hour)) {
    query <- query %>%
      filter(hour == !!hour)
  }
  
  collect(query) %>%
    mutate(datetime = as.POSIXct(date) + hour * 3600)
}

d <- eq_slice(date = "2026-05-05", hour = NULL) %>%
  left_join(zensus_grid)

st_write(d,
         "code/temp/i2test.gpkg",
         layer = "i2slice_20260505",
         append = FALSE)



profile <- eq_profile("100mN31153E41401")

p <- ggplot(profile, aes(x = datetime, y = Erschließungsqualität)) +
  geom_path(group = "departures_per_hour") +
  geom_point(aes(color = to_id))

ggplotly(p)



  