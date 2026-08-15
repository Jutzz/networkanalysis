library(here)
library(dplyr)
library(fst)
library(arrow)
write_parquet(
  read_fst("output/hourly_eq/full/results_full_hour.fst"),
  "output/hourly_eq/full/results_full_hour.parquet"
)
write_parquet(
  read_fst("output/daily_eq/full/results_full_day.fst"),
  "output/daily_eq/full/results_full_day.parquet"
)
write_parquet(
  read_fst("output/hourmean_eq/full/results_full_hourmean.fst"),
  "output/hourmean_eq/full/results_full_hourmean.parquet"
)
library(dtplyr)
library(tidyr)
library(data.table)
library(sf)
library(ggplot2)
library(plotly)

group <- "daily"

#Read helper functions
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)
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

if (group == "hourly") {
  ds <- lazy_dt(read_fst("output/hourly_eq/full/results_full_hour.fst"))
} else if (group == "daily") {
  ds <- lazy_dt(read_fst("output/daily_eq/full/results_full_day.fst"))
} else if (group == "hourmean") {
  ds <- lazy_dt(read_fst("output/hourmean_eq/full/results_full_hourmean.fst"))
} else {
  ds <- lazy_dt(read_fst("output/total_eq/full/results_full_total.fst"))
}

gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")
kreise <- st_read("geodata/dvg1nw.gpkg", "dvg1krs_regbez")
#Build Summary Tables -----

summary <- ds %>%
  replace_na(list(Erschließungsqualität = 8)) %>%
  group_by(id) %>%
  summarise(
    mean_eq = mean(Erschließungsqualität, na.rm = TRUE),
    median_eq = median(Erschließungsqualität, na.rm = TRUE),
    modal_eq = mode_value(Erschließungsqualität),
    best_eq = min(Erschließungsqualität),
    worst_eq = max(Erschließungsqualität),
    range_eq = diff(range(Erschließungsqualität)),
    dist_eq = paste(sort(unique(
      Erschließungsqualität
    )), collapse = ","),
    n_eq = n_distinct(Erschließungsqualität, na.rm = TRUE),
    n_stops = n_distinct(to_id, na.rm = TRUE)
  ) %>%
  as.data.frame()

summary_grid <- summary %>%
  left_join(zensus_grid) %>%
  st_write(
    "results/indikator_02.gpkg",
    paste(
      "i2grid",
      group,
      min(date_select),
      max(date_select),
      feed_date,
      sep = "_"
    ),
    append = FALSE
  )

stats_by_gem <- summary_grid %>%
  group_by(ags, modal_eq) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(ags) %>%
  mutate(total_population = sum(population),
         percentage = round((population / total_population) * 100, 2)) %>%
  ungroup() %>%
  rename("Erschließungsqualität" = modal_eq)

dominant_accessibility <- as.data.frame(stats_by_gem) %>%
  group_by(ags) %>%
  slice_max(percentage, with_ties = FALSE) %>%
  select(ags, Erschließungsqualität) %>%
  rename(größterAnteil = Erschließungsqualität)

stats_by_gem_wide <- stats_by_gem %>%
  pivot_wider(
    id_cols = c("ags", "total_population"),
    names_from = Erschließungsqualität,
    values_from = c("population", "percentage")
  ) %>%
  left_join(dominant_accessibility, by = "ags") %>%
  left_join(gemeinden, by = join_by("ags" == "KN")) %>%
  filter(!is.na(GN)) %>%
  mutate(KNTRIM = substr(ags, 1, 5)) %>%
  left_join(st_drop_geometry(kreise) %>% select(GN, KNTRIM) %>% rename("Kreis" = GN)) %>%
  select(!KNTRIM) %>%
  st_as_sf()

st_write(
  stats_by_gem_wide,
  "results/indikator_02.gpkg",
  paste(
    "i2aggregated",
    group,
    min(date_select),
    max(date_select),
    feed_date,
    sep = "_"
  ),
  append = FALSE
)









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
eq_profile <- function(id, group = c("hour","meanhour","day")) {
  parquet_dir <- if (group == "hour") {
    "output/hourly_eq/full/results_full_hour.parquet"
  } else if (group == "day") {
    "output/daily_eq/full/results_full_day.parquet"
  } else {
    "output/hourmean_eq/full/results_full_hourmean.parquet"
  } 
  ds <- open_dataset(parquet_dir)
  
  ds <- ds %>%
    filter(id == !!id) %>%
    select(any_of(c("date", "hour")),
           Erschließungsqualität,
           to_id,
           departures_per_hour,
           travel_time_p01) %>%
    collect() %>%
    replace_na(list(Erschließungsqualität = 8))
    
  
  if ("date" %in% names(ds) & "hour" %in% names(ds)) {
    ds$timestamp <- as.POSIXct(ds$date) + (ds$hour * 3600)
  } else if ("date" %in% names(ds)) {
    ds$timestamp <- as.POSIXct(ds$date)
  } else if ("hour" %in% names(ds)) {
    ds$timestamp <- hms::hms(hours = ds$hour)
  } else {
    ds$timestamp <- NA
  }
 return(ds)
}

eq_slice <- function(date,
                     hour = NULL,
                     parquet_dir = "output/hourly_eq/full/results_full_hour.parquet") {
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



profile <- eq_profile("100mN30644E40713", group = "hourmean")

p <- ggplot(profile, aes(x = timestamp, y = )) +
  #geom_path(group = "departures_per_hour") +
  geom_point(aes(color = as.factor(travel_time_p01)))

p
ggplotly(p)
