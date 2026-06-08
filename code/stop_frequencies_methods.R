library(tidytransit)
library(gtfstools)
library(tidyverse)
library(timeDate)
library(here)
library(sf)
library(httr2)
library(jsonlite)
library(zoo)
library(extrafont)
library(fst)
extrafont::loadfonts()

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

#Set route_type ranking: train, tram, bus, everything else.
get_parent_dhid <- function(stop_id) {
  sub("^((de:[^:]+:[^:]+)).*$", "\\1", stop_id)
}

is_ifopt <- function(stop_id) {
  grepl("^de:[^:]+:[^:]+", stop_id)
}

route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700", "704"), 
  c("4","5","715", "1000")
)

route_rank_lookup <- setNames(
  rep(seq_along(route_type_ranks), lengths(route_type_ranks)),
  unlist(route_type_ranks)
)


get_route_rank <- function(rt) {
  unname(route_rank_lookup[as.character(rt)])
}

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
  4, 1, 7
)

#Change feed date and area name to used feed version (filename set in de_gtfs_cleaning).
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"

#Read pre-filtered GTFS-Feed
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_", feed_date, "_", area_name,".zip"))
zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

timestamp <- format(Sys.Date(), "%Y-%m-%d")

zhv_lookup  <- zhv %>%
  select(Name, DHID)  %>%
  st_drop_geometry()

stops <- gtfs_feed$stops %>%
  left_join(zhv_lookup, by = c("stop_name" = "Name"))

stops2 <- stops %>%
  mutate(
    grouping_id = case_when(
      !is.na(parent_station) & parent_station != "" ~ parent_station,
      is_ifopt(stop_id) ~ get_parent_dhid(stop_id),
      !is.na(DHID) ~ DHID,
      TRUE ~ stop_id
    )
  )
#Get dates of representative norm- and weekdays from checking earlier.
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")

nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")

method <- "weekday"

ifelse(method == "weekday",
         date_select <- nonholiday_weekdays_fullservice,
         date_select <- nonholiday_normdays_fullservice)

filtered_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% date_select)

filtered_trips_dates <- gtfs_feed$trips %>%
  select(trip_id, service_id, route_id) %>%
  left_join(gtfs_feed$routes %>% select(route_id, route_type)) %>%
  inner_join(filtered_services, by = "service_id") %>%
  filter(!route_type %in% c("101", "102", "201", ""))  %>%
  select(trip_id, date, route_type, route_id)


# library(DBI)
# library(duckdb)
# 
# con <- dbConnect(duckdb())
# 
# dbWriteTable(con, "stop_times", gtfs_feed$stop_times)
# dbWriteTable(con, "filtered_trips_dates", filtered_trips_dates)
# dbWriteTable(con, "stops2", stops2)
# 
# query <- "
# SELECT
#   st.trip_id,
#   st.stop_id,
#   st.departure_time,
#   st.arrival_time,
#   tr.date,
#   tr.route_type,
#   s.grouping_id
# FROM stop_times st
# JOIN filtered_trips_dates tr
#   ON st.trip_id = tr.trip_id
# LEFT JOIN stops2 s
#   ON st.stop_id = s.stop_id
# "
# 
# result <- dbGetQuery(con, query)

filtered_stop_times_dates <- gtfs_feed$stop_times %>%
  filter(pickup_type == 0) %>%
  select(trip_id, stop_id, departure_time, arrival_time) %>%
  filter(trip_id %in% filtered_trips_dates$trip_id) %>%
  left_join(filtered_trips_dates %>%
              select(trip_id, date, route_type, route_id), by = "trip_id") %>%
  left_join(stops2, by = "stop_id")  %>%
  mutate(route_rank = route_rank_lookup[as.character(route_type)])

write_fst(filtered_stop_times_dates, paste("code/temp/", feed_date, method, "filtered_stop_times_dates.fst", collapse = "_"))

filtered_stop_times_dates <- read_fst(paste("code/temp/", feed_date, method, "filtered_stop_times_dates.fst", collapse = "_"))

bq_weekday <- st_read("geodata/Bedienungsqualität.gpkg", "20260518_weekday_2026-05-04_2026-06-26")

bq_normday <- st_read("geodata/Bedienungsqualität.gpkg", "20260518_normday_2026-05-05_2026-06-25")


departure_counts_daily <- filtered_stop_times_dates %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  group_by(date, grouping_id) %>%
  summarise(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    date,
    grouping_id,
    fill = list(departures = 0)
  ) %>%
  mutate(
    departures_per_hour = departures / 10,
    stop_type = ifelse(departures == 0, 0, stop_type)
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
          departures_per_hour,
          vec = c(0, 2, 4, 6, 12, 24),
          rightmost.closed = FALSE
    )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(1,3,2,Name,4,5,22,23,21) %>%
  mutate(weekday = weekdays.Date(date)) %>%
  rename("stop_id" = grouping_id)


st_write(departure_counts_daily %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste("daily", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)


bq_daily <- st_read("geodata/Bedienungsqualität.gpkg", paste("daily", feed_date, method, min(date_select), max(date_select), sep = "_"))

departure_counts_median <- bq_daily %>%
  group_by(stop_id) %>%
  summarise(median = median(departures))

st_write(departure_counts_median, paste(paste("median", feed_date, method, min(date_select), max(date_select), sep = "_")))

variability_daily <- st_drop_geometry(bq_daily) %>%
  arrange(stop_id, date) %>%
  group_by(stop_id, Name) %>%
  mutate(
    diff = abs(departures_per_hour - lag(departures_per_hour))
  ) %>%
  summarise(
    mean_departures = mean(departures_per_hour, na.rm = TRUE),
    min_departures = min(departures_per_hour, na.rm = TRUE),
    max_departures = max(departures_per_hour, na.rm = TRUE),
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
    .groups = "drop"
  ) %>%
  filter(str_detect(stop_id,  "de:053"))


variability_daily_geo <- variability_daily %>%
  left_join(zhv %>% select(DHID,MunicipalityCode,Municipality,geom), by = join_by("stop_id" == "DHID")) %>%
  mutate(
    plot_file = paste0(
      gsub("[^[:alnum:]_-]", "_", Name),
      "__",
      gsub("[^[:alnum:]_-]", "_", stop_id),
      "__chg-", n_changes,
      "__rng-", quality_range,
      "__mean-", round(mean_departures, 1),
      ".png"
    )
  ) %>%
  mutate(depart_plot = paste0(gsub("[^[:alnum:]_-]", "_", stop_id), ".png"))

st_write(variability_daily_geo, "geodata/Bedienungsqualität.gpkg", paste("variablity_daily", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)

#variability_daily  <- st_read("geodata/Bedienungsqualität.gpkg", paste("variablity_daily", feed_date, "weekday", min(date_select), max(date_select), sep = "_"))

c0 <- variability_daily %>% filter(quality_range == 0)
c1 <- variability_daily %>% filter(quality_range == 1)
c2 <- variability_daily %>% filter(quality_range == 2)
c3 <- variability_daily %>% filter(quality_range == 3)
c4 <- variability_daily %>% filter(quality_range == 4)
c5 <- variability_daily %>% filter(quality_range == 5)
c6 <- variability_daily %>% filter(quality_range == 6)

c1_low <- c1 %>% filter(n_changes <= 1)

ggplot(departure_counts_daily %>% filter(!stop_id %in% c0$stop_id), aes(x = date, y = departures_per_hour, colour = as.factor(Bedienungsqualität))) +
  geom_point() +
  facet_wrap(~ Name)

ggplotly(f)

ggplot(variability_daily %>% filter(quality_range >= 0), aes(x = as.factor(quality_range), y = pct_variation)) +
  geom_boxplot()

ggplot(bq_weekday %>% filter(!is.na(Bedienungsqualität), stop_type == "3"), aes(x = as.factor(Bedienungsqualität), y = departures_per_hour)) +
  geom_boxplot()


one <- departures_hour(1)



outdir <- "appendix/figures/stopvar"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

bq_colors <- c(
  "1" = "#1a9641",
  "2" = "#8acc62",
  "3" = "#dbf09e",
  "4" = "#fedf9a",
  "5" = "#f59053",
  "6" = "#d7191c",
  "7" = "#b8c4cb"
)

departure_counts_daily <- st_read("geodata/Bedienungsqualität.gpkg", "daily_20260518_weekday_2026-05-04_2026-06-26")

st_drop_geometry(departure_counts_daily) %>%
  filter(str_detect(stop_id, "de:053")) %>%
  filter(!stop_id %in% c0$stop_id) %>%
  distinct(stop_id, Name) %>%
  left_join(
    variability_daily %>%
      select(stop_id, mean_departures, quality_range, n_changes),
    by = "stop_id"
  ) %>%
  pwalk(function(stop_id,
                 Name,
                 mean_departures,
                 quality_range,
                 n_changes) {
    
    dat <- departure_counts_daily %>%
      filter(
        !stop_id %in% c0$stop_id,
        stop_id == !!stop_id
      )
    
    p <- ggplot(
      dat,
      aes(
        x = date,
        y = departures_per_hour,
        fill = as.factor(Bedienungsqualität)
      )
    ) +
      geom_point(shape = 21, stroke = 1, color = "black") +
      geom_hline(
        yintercept = mean_departures,
        linetype = "dashed",
        colour = "black"
      ) +
      annotate(
        "text",
        x = max(dat$date, na.rm = TRUE),
        y = max(dat$departures_per_hour, na.rm = TRUE),
        hjust = 1,
        vjust = 1,
        label = paste0(
          "mean = ", round(mean_departures, 1),
          "\nchanges = ", n_changes,
          "\nrange = ", quality_range
        )
      ) +
      ggtitle(Name) +
      scale_fill_manual(name = "Bedienungsqualität", values = bq_colors) +
      scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d.%m", limits = c(as.Date("2026-05-04"), NA)) +
      theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))
    
    fname <- paste0(
      gsub("[^[:alnum:]_-]", "_", Name),
      "__",
      gsub("[^[:alnum:]_-]", "_", stop_id),
      "__chg-", n_changes,
      "__rng-", quality_range,
      "__mean-", round(mean_departures, 1)
    )
    
    ggsave(
      file.path(outdir, paste0(fname, ".png")),
      p,
      width = 12,
      height = 7,
      dpi = 600
    )
  })


departures_singlestop <- filtered_stop_times_dates %>%
  filter(grouping_id == "de:05378:38435") %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(timestamp = ymd_hms(paste(as.character(date), as.character(departure_time)))) %>%
  arrange(date, departure_time) %>%
  mutate(wday = weekdays(date))
  
set.seed(3)
ggplot(departures_singlestop, aes(x = departure_time, y = route_id, colour = as.factor(route_type))) +
  geom_point() +
  facet_wrap(~ wday)
  
outdir <- "appendix/figures/stop_routes"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

changing_stops <- filtered_stop_times_dates %>%
  filter(str_detect(grouping_id, "de:053"),
         !grouping_id %in% c0$stop_id) %>%
  distinct(grouping_id) %>%
  pull(grouping_id)

route_type_colors <- c(
  "0" = "#1b9e77",
  "1" = "#1b9e77",
  "106" = "#ff0000",
  "109" = "#ff0000",
  "201" = "gray20",
  "3" = "gray20",
  "700" = "gray20",
  "704" = "gray20"
)

for (sid in changing_stops) {
  
  df <- filtered_stop_times_dates %>%
    filter(grouping_id == sid) %>%
    filter(
      departure_time >= hms("08:00:00"),
      departure_time <= hms("18:00:00")
    ) %>%
    mutate(timestamp = ymd_hms(paste(as.character(date), as.character(departure_time)))) %>%
    left_join(gtfs_feed$routes %>% select(route_id, route_short_name, route_long_name), by = "route_id")
  
  if (nrow(df) == 0) next
  
  stop_name <- df$stop_name[1]  # assumes stop_name is present or joined earlier
  
  df <- df %>%
    mutate(route_label = paste0(route_id, " (", route_short_name, " ", ifelse(!is.na(route_long_name), route_long_name, ""), ")"))
  
  p <- ggplot(df, aes(timestamp, route_label, colour = as.factor(route_type))) +
    geom_point() +
    ggtitle(stop_name) +
    scale_colour_manual(name = "Route Type", values = route_type_colors, drop = FALSE, na.value = "grey80") +
    #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d.%m", limits = c(as.Date("2026-05-04"), NA)) +
    xlim(as_datetime("2026-05-04"), NA) +
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))
  
  ggsave(
    file.path(outdir, paste0(gsub("[^[:alnum:]_-]", "_", sid), ".png")),
    p,
    width = 20,
    height = 5,
    dpi = 600
  )
}





ggplot(st_drop_geometry(bq_normday) %>% filter(Bedienungsqualität == 5), aes(x = departures_per_hour, y = as.factor(Bedienungsqualität)))  +
  geom_point()

ggplot(bq_weekday, aes(x = departures_per_hour)) +
  geom_density()

bq_weekday %>%
  filter(departures_per_hour<100)%>%
  arrange(departures_per_hour) %>%
  mutate(idx = row_number()) %>%
  ggplot(aes(y = departures_per_hour, x = idx, color = as.factor(stop_type))) +
  geom_point(size = 1, alpha = 0.4) +
  labs(
    y = "Departures per hour",
    x = "Ordered stops",
    title = "Sorted stop frequency distribution"
  ) +
  theme_minimal() +
  facet_wrap(~ Bedienungsqualität)

departures_hour <- function(filter_hour, stop_times){
  filtered_stop_times_dates %>%
    mutate(hour = hour(departure_time)) %>%
    filter(hour == filter_hour)
}


departure_counts_hourly <- filtered_stop_times_dates %>%
  filter(str_detect(grouping_id, "^de:05315:")) %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(hour = hour(departure_time)) %>%
  group_by(date, hour, grouping_id) %>%
  reframe(
    departures = n(),
    stop_type = min(route_rank, na.rm = TRUE)
  ) %>%
  mutate(
    departures_per_hour = departures
  ) %>%
  left_join(zhv, by = join_by(grouping_id == DHID)) %>%
  mutate(freq_class = findInterval(
    departures_per_hour,
    vec = c(0, 2, 4, 6, 12, 24),
    rightmost.closed = FALSE
  )
  ) %>%
  left_join(quality_lookup, by = join_by(stop_type, freq_class)) %>%
  select(3:6,1:2,10:12,23,24,22)

departure_counts_hourlyy <- departure_counts_hourly %>%
  mutate(timestamp = as.POSIXct(
       paste(date, sprintf("%02d:00:00", hour)),
       format = "%Y-%m-%d %H:%M:%S",
       tz = "Europe/Berlin"))

dc_hbf <- departure_counts_hourlyy %>% filter(grouping_id == "de:05315:11201")

deps_hbf <- filtered_stop_times_dates %>% filter(grouping_id == "de:05315:11201") %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(hour = hour(departure_time)) %>%
  group_by(date, hour, grouping_id)

departure_counts_worst_case <- departure_counts_hourlyy  %>%
  filter(hour < 18)  %>%
  group_by(grouping_id) %>%
  slice_min(departures, n= 1, with_ties = FALSE)

ggplot(departure_counts_hourlyy %>% filter(grouping_id == "de:05315:11212", hour < 18), aes(x = hour, y = departures)) +
  geom_point()

st_write(departure_counts_worst_case %>%
           filter(!is.na(geom)), here("geodata/Bedienungsqualität.gpkg"),
         paste("hourly", "worst_case", feed_date, method, min(date_select), max(date_select), sep = "_"), append = FALSE)

shapes_as


df <- filtered_stop_times_dates %>%
  filter(pickup_type == 0,
         grouping_id == "de:05382:57952") %>%
  filter(
    departure_time >= hms("08:00:00"),
    departure_time <= hms("18:00:00")
  ) %>%
  mutate(timestamp = ymd_hms(paste(as.character(date), as.character(departure_time)))) %>%
  left_join(gtfs_feed$routes %>% select(route_id, route_short_name, route_long_name), by = "route_id") %>%
  mutate(
    dep_time = hms::as_hms(timestamp),
    date = as.Date(timestamp)
  ) %>%
  mutate(
    weekday = lubridate::wday(
      date,
      label = TRUE,
      week_start = 1
    )
  )

p <- ggplot(
  df %>% arrange(dep_time),
  aes(
    x = date,
    y = dep_time,
    colour = as.factor(route_id)
  )
) +
  geom_point(size = 1.5) +
  #ggtitle(stop_name) +
  # scale_colour_manual(
  #   name = "Route Type",
  #   values = route_type_colors,
  #   drop = FALSE,
  #   na.value = "grey80"
  # ) +
  scale_x_date(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%d.%m"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = windowsFont("Source Sans 3"))
  ) #+
facet_wrap(~weekday, ncol = 1)

library(tidytext)

enr_stop_times <- gtfs_feed$stop_times %>%
  left_join(gtfs_feed$trips %>% select(trip_id, route_id, service_id)) %>%
  left_join(gtfs_feed$routes %>% select(route_id, route_short_name, route_type))

fnz <- enr_stop_times %>%
  filter(route_id == "de:aac:05358|86:rtbus_3")

ggplot(
  fnz,
  aes(
    x = stop_sequence,
    y = departure_time,
    colour = stop_id
  )
) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~service_id, scales = "free")

  
  