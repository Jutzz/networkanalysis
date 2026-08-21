library(here)
library(dplyr)
library(arrow)
library(fst)
library(readr)
library(tidyr)
library(sf)
library(ggplot2)

gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250ss")
kreise <- st_read("geodata/dvg1nw.gpkg", "dvg1krs_regbez")

palette_gyr8 <- c(
  "1" = "#169542",
  "2" = "#77c35c",
  "3" = "#c4e687",
  "4" = "#ffffc0",  
  "5" = "#fec981",  
  "6" = "#f07c4a",
  "7" = "#d7191c",
  "8" = "#3f3f3f"
)

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)

stops_core <- read_fst("output/hourly_20260518_weekday_2026-05-04_2026-06-26_.fst")

bq_profile <- function(id){
  profile <- stops_core %>%
    filter(stop_id == !!id) %>%
    mutate(timestamp = as.POSIXct(date) + (hour * 3600))
}

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
           Bedienungsqualität,
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

eq_profile_stop <- function(stop_id, group = c("hour","meanhour","day")) {
  parquet_dir <- if (group == "hour") {
    "output/hourly_eq/full/results_full_hour.parquet"
  } else if (group == "day") {
    "output/daily_eq/full/results_full_day.parquet"
  } else {
    "output/hourmean_eq/full/results_full_hourmean.parquet"
  } 
  ds <- open_dataset(parquet_dir)
  
  ds <- ds %>%
    filter(to_id == !!stop_id) %>%
    select(id,
           any_of(c("date", "hour")),
           Erschließungsqualität,
           Bedienungsqualität,
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



profile <- eq_profile("100mN31021E41295", group = "hourmean")

p <- ggplot(profile, aes(x = timestamp, y = departures_per_hour)) +
  geom_line() +
  #geom_path(group = "timestamp") +
  geom_point(aes(color = as.factor(Erschließungsqualität))) 
  #scale_y_reverse()

p
ggplotly(p)

sprofile <- eq_profile_stop(stop_id = "de:05374:43852", group = "hourmean")

id <- bq_profile("de:05374:43852")

ggplot(id, aes(x = timestamp, y = departures_per_hour)) +
  geom_point()

ds <- lazy_dt(read_fst("output/hourmean_eq/full/results_full_hourmean.fst"))

ds_agg <- ds %>%
  left_join(zensus_grid %>% select(id, ags, Einwohner), by = "id") %>%
  replace_na(list(Erschließungsqualität = 8)) %>%
  group_by(ags, hour, Erschließungsqualität) %>%
  summarise(
    pop = sum(Einwohner),
    .groups = "drop"
  ) %>%
  group_by(ags, hour) %>%
  mutate(
    total_pop = sum(pop),
    percentage = 100 * pop / total_pop
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(!is.na(ags)) %>%
  left_join(gemeinden, by = join_by("ags" == "KN"))

ds_agg_krs <- ds %>%
  left_join(zensus_grid %>% select(id, ags, Einwohner), by = "id") %>%
  mutate(KNTRIM = substr(ags, 1, 5)) %>%
  left_join(st_drop_geometry(kreise) %>% select(GN, KNTRIM) %>% rename("Kreis" = GN)) %>%
  group_by(Kreis, Erschließungsqualität) %>%
  summarise(
    pop = sum(Einwohner),
    .groups = "drop"
  ) %>%
  group_by(Kreis) %>%
  mutate(
    total_pop = sum(pop),
    percentage = 100 * pop / total_pop
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(!is.na(Kreis))

ggplot(ds_agg, aes(x = as.factor(hour), y = percentage, fill = as.factor(Erschließungsqualität))) +
  geom_col() +
  scale_fill_manual(values = palette_gyr8)

ggplot(ds_agg_krs, aes(x = Kreis, y = percentage, fill = as.factor(Erschließungsqualität))) +
  geom_col() +
  scale_fill_manual(values = palette_gyr8) 
       