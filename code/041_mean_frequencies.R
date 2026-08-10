#Creating Daily, Day-Of-Weekly and total freq tables
library(dplyr)
library(dtplyr)
library(readr)
library(lubridate)
library(data.table)
library(fst)
library(sf)
library(here)
#Read helper functions
files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"
method <- "weekday"

#Get dates of representative norm- and weekdays from checking in find_valid_dates.R
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")
nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
#Choose number of dates based on method set above.
ifelse(method == "weekday",
       date_select <- nonholiday_weekdays_fullservice,
       date_select <- nonholiday_normdays_fullservice)


zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

quality_lookup <- tribble(
  ~stop_type, ~freq_class, ~Bedienungsqualität,
  3, 2, 6,
  3, 3, 5,
  3, 4, 4,
  3, 5, 3,
  3, 6, 2,
  2, 2, 5,
  2, 3, 4,
  2, 4, 3,
  2, 5, 2,
  2, 6, 1,
  1, 2, 4,
  1, 3, 3,
  1, 4, 2,
  1, 5, 1,
  1, 6, 1,
  0, 1, 7,
  1, 1, 7,
  2, 1, 7,
  3, 1, 7,
  4, 1, 7,
  4, 2, 7,
  4, 3, 7,
  4, 4, 7,
  4, 5, 7,
  4, 6, 7
)

stops_core <- lazy_dt(read_fst(paste(
  "output/hourly",
  feed_date,
  method,
  min(date_select),
  max(date_select),
  ".fst", sep = "_")
  ))

stops_dailymean <- stops_core %>%
  select(!Bedienungsqualität) %>%
  replace_na(list(stop_type = 4)) %>%
  group_by(stop_id, date) %>%
  summarise(mean_day = mean(departures_per_hour),
            type_range = diff(range(stop_type)),
            types = paste(sort(unique(stop_type)), collapse = ","),
            stop_type = min(stop_type),
            .groups = "drop"
            )%>%
  as.data.frame() %>%
  left_join(zhv, by = join_by(stop_id == DHID)) %>%
  mutate(freq_class = findInterval(
    mean_day,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE 
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(Name, stop_id, Municipality, date, Bedienungsqualität, mean_day, stop_type, type_range, types, MunicipalityCode, geom)

variability_daily <- st_drop_geometry(stops_dailymean) %>%
  arrange(stop_id, date) %>%
  group_by(stop_id, Name) %>%
  mutate(
    diff = abs(mean_day - lag(mean_day))
  ) %>%
  summarise(
    mean_departures = mean(mean_day, na.rm = TRUE),
    median_departures = median(mean_day, na.rm = TRUE),
    min_departures = min(mean_day, na.rm = TRUE),
    max_departures = max(mean_day, na.rm = TRUE),
    stop_type_mean = max(stop_type),
    stop_type_median = floor(median(stop_type)),
    sum_abs_diff = sum(diff, na.rm = TRUE),
    mean_abs_diff = mean(diff, na.rm = TRUE),
    pct_variation = 100 * mean_abs_diff / mean_departures,
    min_quality = min(Bedienungsqualität, na.rm = TRUE),
    max_quality = max(Bedienungsqualität, na.rm = TRUE),
    quality_range = max_quality - min_quality,
    n_changes = sum(
      Bedienungsqualität != lag(Bedienungsqualität),
      na.rm = TRUE
    ),
    days_observed = n(),
    modal_quality = as.numeric(
      names(which.max(table(Bedienungsqualität)))
    ),
    pct_days_modal_quality =
      100 * max(table(Bedienungsqualität)) / n(),
    .groups = "drop"
  ) %>%  mutate(freq_class_mean = findInterval(
    mean_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  ) %>%  mutate(freq_class_median = findInterval(
    median_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  )%>%
  left_join(quality_lookup, by = join_by("stop_type_mean" == "stop_type", "freq_class_mean" == "freq_class")) %>%
  rename("bq_mean" = Bedienungsqualität) %>%
  left_join(quality_lookup, by = join_by("stop_type_median" == "stop_type", "freq_class_median" == "freq_class")) %>%
  rename("bq_median" = Bedienungsqualität) %>%
  left_join(zhv %>% select(DHID, Name, MunicipalityCode, Municipality, geom), by = join_by("stop_id" == "DHID" , Name)) %>%
  relocate(Name,
           stop_id,
           Municipality,
           bq_mean,
           bq_median,
           mean_departures,
           freq_class_mean,
           median_departures,
           freq_class_median,
           min_departures,
           max_departures,
           stop_type_mean,
           stop_type_median,
           sum_abs_diff,
           mean_abs_diff,
           pct_variation,
           min_quality,
           max_quality,
           quality_range,
           n_changes,
           days_observed,
           modal_quality,
           pct_days_modal_quality,
           MunicipalityCode)  


stops_dowmean <- stops_core %>%
  replace_na(list(stop_type = 4)) %>%
  mutate(dow = weekdays(date)) %>%
  group_by(stop_id, dow) %>%
  summarise(mean_dow = mean(departures_per_hour),
            type_range = diff(range(stop_type)),
            types = paste(sort(unique(stop_type)), collapse = ","),
            stop_type = min(stop_type)) %>%
  ungroup() %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by(stop_id == DHID)) %>%
  mutate(freq_class = findInterval(
    mean_dow,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(Name, stop_id, Municipality, dow, Bedienungsqualität, mean_dow, stop_type, type_range, types, MunicipalityCode, geom)

variability_dow <- st_drop_geometry(stops_dowmean) %>%
  arrange(stop_id, dow) %>%
  group_by(stop_id, Name) %>%
  mutate(
    diff = abs(mean_dow - lag(mean_dow))
  ) %>%
  summarise(
    mean_departures = mean(mean_dow, na.rm = TRUE),
    median_departures = median(mean_dow, na.rm = TRUE),
    min_departures = min(mean_dow, na.rm = TRUE),
    max_departures = max(mean_dow, na.rm = TRUE),
    stop_type_mean = max(stop_type),
    stop_type_median = floor(median(stop_type)),
    sum_abs_diff = sum(diff, na.rm = TRUE),
    mean_abs_diff = mean(diff, na.rm = TRUE),
    pct_variation = 100 * mean_abs_diff / mean_departures,
    min_quality = min(Bedienungsqualität, na.rm = TRUE),
    max_quality = max(Bedienungsqualität, na.rm = TRUE),
    quality_range = max_quality - min_quality,
    n_changes = sum(
      Bedienungsqualität != lag(Bedienungsqualität),
      na.rm = TRUE
    ),
    days_observed = n(),
    modal_quality = as.numeric(
      names(which.max(table(Bedienungsqualität)))
    ),
    pct_days_modal_quality =
      100 * max(table(Bedienungsqualität)) / n(),
    .groups = "drop"
  ) %>%  mutate(freq_class_mean = findInterval(
    mean_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  ) %>%  mutate(freq_class_median = findInterval(
    median_departures,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE)
  )%>%
  left_join(quality_lookup, by = join_by("stop_type_mean" == "stop_type", "freq_class_mean" == "freq_class")) %>%
  rename("bq_mean" = Bedienungsqualität) %>%
  left_join(quality_lookup, by = join_by("stop_type_median" == "stop_type", "freq_class_median" == "freq_class")) %>%
  rename("bq_median" = Bedienungsqualität) %>%
  left_join(zhv %>% select(DHID, Name, MunicipalityCode, Municipality, geom), by = join_by("stop_id" == "DHID" , Name)) %>%
  relocate(Name,
           stop_id,
           Municipality,
           bq_mean,
           bq_median,
           mean_departures,
           freq_class_mean,
           median_departures,
           freq_class_median,
           min_departures,
           max_departures,
           stop_type_mean,
           stop_type_median,
           sum_abs_diff,
           mean_abs_diff,
           pct_variation,
           min_quality,
           max_quality,
           quality_range,
           n_changes,
           days_observed,
           modal_quality,
           pct_days_modal_quality,
           MunicipalityCode) 


stops_totalmean <- stops_core %>%
  group_by(stop_id) %>%
  summarise(mean_total= mean(departures_per_hour),
            n_types = length(unique(stop_type)),
            types = paste(sort(unique(stop_type)), collapse = ","),
            stop_type = min(stop_type)) %>%
  ungroup %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by(stop_id == DHID)) %>%
  mutate(freq_class = findInterval(
    mean_total,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(Name, stop_id, Municipality, Bedienungsqualität, mean_total, stop_type, MunicipalityCode, geom)
