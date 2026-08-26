library(here)
library(dplyr)
library(fst)
library(arrow)
#These need to run before loading other packages (but only once after recalculating EQ)
#as parquet/arrow struggles with too many packages loaded (see https://github.com/apache/arrow/issues/50466)
 # write_parquet(
 #   read_fst("output/hourly_eq/full/results_full_hour.fst"),
 #   "output/hourly_eq/full/results_full_hour.parquet"
 # )
# write_parquet(
#   read_fst("output/daily_eq/full/results_full_day.fst"),
#   "output/daily_eq/full/results_full_day.parquet"
# )
# write_parquet(
#   read_fst("output/hourmean_eq/full/results_full_hourmean.fst"),
#   "output/hourmean_eq/full/results_full_hourmean.parquet"
# )
library(dtplyr)
library(tidyr)
library(data.table)
library(sf)
library(ggplot2)
library(plotly)


group <- "hourly" #hourly,daily,meanhour or total

#Read helper functions
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

source("code/dataenv.R")

method <- "weekday"

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)

grid_ids_regbez <- st_drop_geometry(zensus_grid) %>%
  filter(str_detect(ags, "^053")) %>%
  pull(id)

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
kreise <- st_read("geodata/dvg1nw.gpkg", "kreise_regbez_vg250")
#Build Summary Tables -----

summary <- ds %>%
  replace_na(list(Erschließungsqualität = 9)) %>%
  group_by(id) %>%
  arrange(date, hour) %>%
  summarise(
    mean_eq = mean(Erschließungsqualität),
    median_eq = median(Erschließungsqualität),
    p50_eq = quantile(Erschließungsqualität, probs = 0.5, type = 1),
    modal_eq = mode_value(Erschließungsqualität),
    best_eq = min(Erschließungsqualität),
    worst_eq = max(Erschließungsqualität),
    range_eq = diff(range(Erschließungsqualität)),
    dist_eq = paste(sort(unique(
      Erschließungsqualität
    )), collapse = ","),
    n_eq = n_distinct(Erschließungsqualität, na.rm = TRUE),
    mean_tt = mean(travel_time_p01, na.rm = TRUE),
    mean_bq = mean(Bedienungsqualität, na.rm = TRUE),
    n_stops = n_distinct(to_id, na.rm = TRUE),
    n_changes = sum(
      Erschließungsqualität != lag(Erschließungsqualität),
      na.rm = TRUE
    )
  ) %>%
  as.data.frame()

write_fst(summary, "results/i2_summary.fst")

summary_by_hour <- ds %>%
  replace_na(list(Erschließungsqualität = 9)) %>%
  group_by(id, hour) %>%
  summarise(
    p50_eq = quantile(Erschließungsqualität, probs = 0.5, type = 1)
    ) %>%
  as.data.frame()

write_fst(summary_by_hour, "results/i2_summary_by_hour.fst")

summary_by_date <- ds %>%
  replace_na(list(Erschließungsqualität = 9)) %>%
  group_by(id, date) %>%
  summarise(
    p50_eq = quantile(Erschließungsqualität, probs = 0.5, type = 1)
  ) %>%
  as.data.frame()

write_fst(summary_by_date, "results/i2_summary_by_date.fst")

# Calculate weekday once for each unique date
date_lookup <- unique(date_select)
weekday_lookup <- wday(date_lookup)

# Match the weekday back to every row


summary_by_weekday <- ds %>%
  filter(id %in% grid_ids_regbez) %>%
  mutate(weekday = weekday_lookup[match(date, date_lookup)]) %>%
  replace_na(list(Erschließungsqualität = 9)) %>%
  group_by(id, weekday) %>%
  summarise(
    p50_eq = quantile(Erschließungsqualität, probs = 0.5, type = 1)
  ) %>%
  as.data.frame()
  
write_fst(summary_by_weekday, "results/i2_summary_by_weekday.fst")
