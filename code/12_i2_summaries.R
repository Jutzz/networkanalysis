library(here)
library(dplyr)
library(fst)
library(arrow)
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
feed_date <- "20260518"

method <- "hourly"

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  st_as_sf() %>%
  select(id, ags, Einwohner)

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
    p50_eq = quantile(Erschließungsqualität, probs = 0.5, type = 1),
    modal_eq = mode_value(Erschließungsqualität),
    best_eq = min(Erschließungsqualität),
    worst_eq = max(Erschließungsqualität),
    range_eq = diff(range(Erschließungsqualität)),
    dist_eq = paste(sort(unique(
      Erschließungsqualität
    )), collapse = ","),
    n_eq = n_distinct(Erschließungsqualität, na.rm = TRUE),
    n_stops = n_distinct(to_id, na.rm = TRUE),
    n_changes = sum(
      Erschließungsqualität != lag(Erschließungsqualität),
      na.rm = TRUE
    )
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
  group_by(ags, p50_eq) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(ags) %>%
  mutate(total_population = sum(population),
         percentage = round((population / total_population) * 100, 2)) %>%
  ungroup() %>%
  rename("Erschließungsqualität" = p50_eq)

dominant_accessibility <- as.data.frame(stats_by_gem) %>%
  group_by(ags) %>%
  slice_max(percentage, with_ties = FALSE) %>%
  select(ags, Erschließungsqualität) %>%
  rename(größterAnteil = Erschließungsqualität)

stats_by_gem_wide <- stats_by_gem %>%
  pivot_wider(
    id_cols = c("ags", "total_population"),
    names_from = Erschließungsqualität,
    values_from = c("population", "percentage"),
    names_expand = TRUE,
    values_fill = 0, 
  ) %>%
  left_join(dominant_accessibility, by = "ags") %>%
  left_join(gemeinden, by = join_by("ags" == "KN")) %>%
  filter(!is.na(GN)) %>%
  mutate(
    index = (
        percentage_1 * 1 +
        percentage_2 * 2 +
        percentage_3 * 3 +
        percentage_4 * 4 +
        percentage_5 * 5 +
        percentage_6 * 6 +
        percentage_7 * 7 +
        percentage_8 * 8
    ) / 100
  ) %>%
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

stats_by_krs <- summary_grid %>%
  filter(!is.na(ags)) %>%
  mutate(KNTRIM = substr(ags, 1, 5)) %>%
  left_join(st_drop_geometry(kreise) %>% select(GN, KNTRIM) %>% rename("Kreis" = GN))

ggplot(stats_by_krs, aes(x = Kreis, y = )) +
  geom_col() +
  scale_y_reverse()
