#Creating Daily, Day-Of-Weekly and total freq tables
library(dplyr)
library(dtplyr)
library(readr)
library(lubridate)
library(data.table)
library(fst)
library(sf)
library(here)

zhv_date <- "20260521"

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

stops_core <- lazy_dt(read_fst("output/hourly_20260518_weekday_2026-05-04_2026-06-26_.fst"))

stops_dailymean <- stops_core %>%
  select(!Bedienungsqualität) %>%
  group_by(stop_id, date) %>%
  summarise(mean_day = mean(departures_per_hour)) %>%
  ungroup()  %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by(stop_id == DHID)) %>%
  mutate(freq_class = findInterval(
    mean_day,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class))

stops_dowmean <- stops_core %>%
  mutate(dow = weekdays(date)) %>%
  group_by(stop_id, dow) %>%
  summarise(mean_dow = mean(departures_per_hour)) %>%
  ungroup() %>%
  as.data.frame()

stops_totalmean <- stops_core %>%
  group_by(stop_id) %>%
  summarise(mean_total= mean(departures_per_hour)) %>%
  ungroup %>%
  as.data.frame()
