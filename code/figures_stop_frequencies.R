#Change feed date and area name to used feed version (filename set in de_gtfs_cleaning).
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"
method <- "weekday"

#Setup----
options(java.parameters = "-Xmx20G")
library(r5r)
library(tidytransit)
library(gtfstools)
library(tidyverse)
library(plotly)
library(dtplyr)
library(timeDate)
library(here)
library(plotly)
library(sf)
library(httr2)
library(jsonlite)
library(zoo)
library(extrafont)
library(fst)
library(patchwork)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

get_school_holidays <- function(country, subdivision, start_date, end_date, lang = "DE") {
  resp <- httr2::request("https://openholidaysapi.org/SchoolHolidays") |>
    httr2::req_url_query(
      countryIsoCode = country,
      validFrom = start_date,
      validTo = end_date,
      languageIsoCode = lang,
      subdivisionCode = subdivision
    ) |>
    httr2::req_perform()
  
  school_holidays <- fromJSON(rawToChar(resp$body))
}

holidays_nrw <- function(year) {
  year <- as.integer(year)
  
  c(
    # Fixed nationwide
    as.Date(NewYearsDay(year)),
    as.Date(LaborDay(year)),
    as.Date(DEGermanUnity(year)),
    as.Date(ChristmasDay(year)),
    as.Date(BoxingDay(year)),
    as.Date(AllSaints(year)), 
    as.Date(GoodFriday(year)),
    as.Date(EasterMonday(year)),
    as.Date(Ascension(year)),
    as.Date(PentecostMonday(year)),
    as.Date(CorpusChristi(year))
  )
}

#If openholidays api breaks, use this to set school holidays manually.
#school_holidays <- c(
#  seq.Date(as.Date("2026-03-30"), as.Date("2026-04-11"), by = "day"),
#  seq.Date(as.Date("2026-07-20"), as.Date("2026-09-01"), by = "day"),
#  seq.Date(as.Date("2026-10-17"), as.Date("2026-10-31"), by = "day"),
#  seq.Date(as.Date("2026-12-23"), as.Date("2027-01-06"), by = "day"))

school_holidays_NW<- get_school_holidays(country = "DE",
                                         subdivision = "DE-NW",
                                         start_date = "2026-01-01",
                                         end_date = "2028-12-31")

school_holidays_RP<- get_school_holidays(country = "DE",
                                         subdivision = "DE-RP",
                                         start_date = "2026-01-01",
                                         end_date = "2028-12-31")

#Set route_type ranking: train, tram, bus, everything else.
route_type_ranks <- list(
  c("101","102","106","109"),   
  c("0","1"),                  
  c("201","3","700", "704"), 
  c("4","5","715")
)

get_route_rank <- function(rt) {
  which(vapply(route_type_ranks, function(grp) rt %in% grp, logical(1)))
}

holidays <- holidays_nrw(2026)

#Set up date lists of school holidays and public holidays for filtering stop time data.
raw_holidays <- bind_rows(school_holidays_NW, school_holidays_RP) %>%
  prepare_holidays()

school_holidays <- raw_holidays %>%
  expand_holidays()

#Begin analysis----
#Read pre-filtered GTFS-Feed
gtfs_feed <- tidytransit::read_gtfs(paste0("feeds/filtered/de_gtfs_", feed_date, "_", area_name,".zip"))

timestamp <- format(Sys.Date(), "%Y-%m-%d")

#normalwerktage present in feed
normdays <- {
  d <- unique(gtfs_feed$.$dates_services$date)
  wd <- wday(d, week_start = 1)
  mo <- month(d)
  
  d[wd %in% c(2, 3, 4) & mo %in% c(4, 5, 6, 9)]
}
#weekdays present in feed
weekdays <- {
  d <- unique(gtfs_feed$.$dates_services$date)
  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
}

#Manually choose a selection of days present in the feed
#weekdays <- {
#  d <- seq.Date(as.Date("2026-05-20"), as.Date("2026-05-21"), by = "day")
#  d[wday(d, week_start = 1) <= 5]  # week_start=1 makes 1=Mon … 7=Sun
#}

nonholiday_weekdays <- weekdays[!weekdays %in% c(holidays,school_holidays$date)]

nonholiday_normdays <- normdays[!normdays %in% c(holidays,school_holidays$date)]


#set up separate tables of GTFS components for easier joining
stops <- gtfs_feed$stops

stop_times <- gtfs_feed$stop_times

trips <- gtfs_feed$trips

routes <- gtfs_feed$routes

#Analysis of transit availability to check for feed inconsistencies, representative stretches.----
##TODO: Find stats and/or function to find representative dates, to check for large jumps in availability (holidays, schedule updates, partial feeds ending) and for variability across hours.
#Combine calendar, trips and routes to have a complete calendar of single trips with date and route info.
trip_calendar <- gtfs_feed$.$dates_services %>%
  inner_join(gtfs_feed$trips %>%
               select(service_id, trip_id, route_id),
             by = "service_id") %>%
  inner_join(gtfs_feed$routes %>%
               select(route_id, route_type, agency_id, route_short_name),
             by = "route_id") %>%
  filter(!route_type %in% c(102,101,201))

#Calculate simple trips per day for normdays (alternatively weekdays, nonholiday weekdays, all days, etc.)


trips_per_day <- trip_calendar %>%
  group_by(date) %>%
  summarise(
    trips = n()
  ) %>%
  filter(date %in% nonholiday_weekdays) %>%
  #filter(date < as.Date("2026-09-01")) %>%
  arrange(date) %>%
  mutate(week_delta = trips-lag(trips, 5)) %>%
  mutate(rolling_avg = rollmean(trips, 5, na.pad = TRUE)) %>%
  mutate(rollingavg_delta = rolling_avg-lag(rolling_avg, 5)) %>%
  mutate(absolute = abs(rollingavg_delta)) %>%
  mutate(jump = ifelse(date > as.Date("2026-09-01"), "post", "pre"))


activity_per_day <- trip_calendar %>%
  group_by(date, agency_id) %>%
  summarise(
    trips = n()
  ) %>%
  left_join(gtfs_feed$agency %>% dplyr::select(agency_id, agency_name)) %>%
  filter(date %in% weekdays)

top_agencies <- activity_per_day %>%
  group_by(agency_id, agency_name) %>%
  summarise(
    total_trips = sum(trips),
    .groups = "drop"
  ) %>%
  slice_max(total_trips, n = 10)

activity_top10 <- activity_per_day %>%
  filter(agency_id %in% c("7969", "12871", "7764")) %>%
  mutate(
    agency_short = recode(
      agency_name,
      "Aachener Straßenbahn und Energieversorgungs-AG" = "ASEAG",
      "Kölner VB" = "KVB",
      "Rheinbahn Bus" = "Rheinbahn"
    )
  )

activity_top10 <- activity_per_day %>%
  filter(agency_id %in% c("7973")) %>%
  mutate(
    agency_short = recode(
      agency_name,
      "Aachener Straßenbahn und Energieversorgungs-AG" = "ASEAG",
      "Kölner VB" = "KVB",
      "Rheinbahn Bus" = "Rheinbahn"
    )
  )


cutoff <- as.Date("2026-09-01")

agency_change <- activity_per_day %>%
  mutate(
    period = if_else(date < cutoff, "before", "after")
  ) %>%
  group_by(agency_id, agency_name) %>%
  group_modify(~{
    
    # ensure both periods exist
    if(length(unique(.x$period)) < 2) {
      return(tibble(
        mean_before = NA_real_,
        mean_after = NA_real_,
        delta = NA_real_,
        rel_change = NA_real_,
        p_value = NA_real_
      ))
    }
    
    before <- .x$trips[.x$period == "before"]
    after  <- .x$trips[.x$period == "after"]
    
    wt <- wilcox.test(before, after)
    
    tibble(
      mean_before = mean(before),
      mean_after  = mean(after),
      delta = mean(after) - mean(before),
      rel_change = mean(after) / mean(before) - 1,
      p_value = wt$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(delta = round(delta, 3)) %>%
  arrange(rel_change)

suspect <- agency_change %>%
  slice_min(rel_change, n = 6) #%>%
  pull(agency_id)

activity_per_day %>%
  filter(agency_id %in% suspect) %>%
  ggplot(aes(date, trips)) +
  geom_line() +
  facet_wrap(~agency_name, scales = "free_y") +
  geom_vline(
    xintercept = as.Date("2025-09-01"),
    linetype = "dashed"
  )


p <- ggplot(activity_top10, mapping = aes(x = date, y = trips, color = agency_short, text = agency_short)) +
  geom_rect(
    data = plotholidays,
    inherit.aes = FALSE,
    aes(
      xmin = startDate,
      xmax = endDate,
      ymin = -Inf,
      ymax = Inf,
      fill = name
    ),
    alpha = 0.2
  ) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  geom_point() +
  labs(color = "Verkehrsunternehmen", fill = element_blank(), y = "Anzahl Fahrten", x = "Datum")+
  theme(legend.position = "bottom") +
  #ylim(4000,9000) +
  #geom_label(data = plotholidays_labels, inherit.aes = FALSE,  aes(x = label_x, y = 87500, label = subdivision), vjust = 1.5, size = 3, fontface = "bold") +
  theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))

  

p

ggplotly(p, tooltip = "text", dynamicTicks = TRUE)

ggsave(p, filename = "document/figures/trips_per_agency_weekdays_2026-05-18.svg", width = 200, height = 100, units  = "mm", dpi = 300, limitsize = FALSE)  
#Create Plot of trips per day with holiday color bars
x_min <- min(trips_per_day$date, na.rm = TRUE)
x_max <- max(trips_per_day$date, na.rm = TRUE)

plotholidays <- raw_holidays %>%
  filter_holidays_for_plot(x_min, x_max)

plotholidays_labels <- plotholidays %>%
  mutate(
    label_x = startDate + (endDate - startDate) / 2
  )

f <- ggplot(trips_per_day, aes(x = date, y = trips)) +
  geom_rect(
    data = plotholidays,
    inherit.aes = FALSE,
    aes(
      xmin = startDate,
      xmax = endDate,
      ymin = -Inf,
      ymax = Inf,
      fill = name
    ),
    alpha = 0.2
  ) +
  geom_line(aes(y=rolling_avg, color = "gleitender Mittelwert (5 Tage)"), linewidth = 2) +
  geom_point() +
  labs(color = element_blank(), fill = NULL) +
  xlab("Datum") +
  ylab("Anzahl Fahrten") +
  scale_color_manual(values = c("gleitender Mittelwert (5 Tage)" = "red")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  theme_light() +
  geom_label(data = plotholidays_labels, inherit.aes = FALSE,  aes(x = label_x, y = 87500, label = subdivision), vjust = 1.5, size = 3, fontface = "bold") +
  theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))

f

ggsave(f, filename = "document/figures/trips_year_nonholiday_weekdays_2026-05-18.svg", width = 200, height = 100, units  = "mm", dpi = 300)  

ggplot(trips_per_day, aes(x = trips)) +
  geom_dotplot(aes(fill = jump), stackgroups = TRUE, binpositions = "all") +
  theme_light() +
  theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))

ggsave(last_plot(), filename = "document/figures/trips_dotplot_nonholiday_normdays_2026-05-18.svg", width = 240, height = 160, units  = "mm", dpi = 300)  

#R5 Setup
r5_network <- build_network(here("r5core_2026-05-18"), verbose = FALSE, overwrite = FALSE)
#dates = nonholiday_weekdays#

availability <- check_transit_availability(r5_network, dates = unique(gtfs_feed$.$dates_services$date)) %>%
  mutate(weekday = weekdays(as.Date(date))) %>%
  mutate(weekday_n =  format(as.Date(date),"%w")) %>%
  filter(active_services > 0) %>%
  arrange(date) %>%
  mutate(week_delta = active_services-lag(active_services, 7)) %>%
  mutate(rolling_avg = rollmean(pct_active, 7, na.pad = TRUE)) %>%
  mutate(rollingavg_delta = rolling_avg-lag(rolling_avg, 7)) %>%
  mutate(absolute = abs(rollingavg_delta))

jumps <- availability %>%
  mutate(absolute = abs(rollingavg_delta)) %>%
  filter(absolute > 2*(sd(rollingavg_delta, na.rm = TRUE)))

availability_week <- availability %>%
  group_by(weekday) %>%
  mutate(avg_pct = mean(pct_active))%>%
  mutate(avg_services = mean(active_services))%>%
  mutate(sd_services = sd(active_services)) %>%
  mutate(variance_services = var(active_services)) %>%
  select(5, 11:14) %>%
  distinct()

ggplot(availability, aes(x = date, y = pct_active)) +
  geom_line(aes(group = weekday,
                color = factor(weekday))) +
  geom_line(
    aes(
      y = rollmean(pct_active, 7, na.pad = TRUE),
      group = weekday_n
    ),
    color = "#ff0000"
  )

x_min <- min(availability$date, na.rm = TRUE)
x_max <- max(availability$date, na.rm = TRUE)

plotholidays <- raw_holidays %>%
  filter_holidays_for_plot(x_min, x_max)

plotholidays_labels <- plotholidays %>%
  mutate(
    label_x = startDate + (endDate - startDate) / 2
  )

ggplot(availability, aes(x = date, y = pct_active)) +
  geom_rect(
    data = plotholidays,
    inherit.aes = FALSE,
    aes(
      xmin = startDate,
      xmax = endDate,
      ymin = -Inf,
      ymax = Inf,
      fill = name,
      colour = subdivision
    ),
    alpha = 0.2
  ) +
  geom_point() +
  geom_line(aes(y=rolling_avg, color = "gleitender Mittelwert (7 Tage)"), linewidth = 2) +
  labs(title = "Aktive services im Jahresverlauf", subtitle = "auf Grundlage des DELFI-GTFS vom 18.05.2026", color = element_blank(), fill = NULL) +
  xlab("Datum") +
  ylab("Anteil aktiver services") +
  scale_color_manual(values = c("gleitender Mittelwert (7 Tage)" = "red")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  theme(legend.position = "bottom") +
  geom_label(data = plotholidays_labels, inherit.aes = FALSE,  aes(x = label_x, y = max(availability$pct_active), label = subdivision), vjust = 1.5, size = 3, fontface = "bold") +
  theme_light() +
  theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))

ggsave("document/figures/active_services_year_rollavg_2026-05-18.svg", get_last_plot(), )


ifelse(method == "weekday",
       date_select <- nonholiday_weekdays_fullservice,
       date_select <- nonholiday_normdays_fullservice)

filtered_services <- gtfs_feed$.$dates_services %>%
  filter(date %in% date_select)
#Stop Frequency Variability Figures
variability_daily  <- st_read("geodata/Bedienungsqualität.gpkg", paste("variablity_daily", feed_date, "weekday", min(date_select), max(date_select), sep = "_"))

daily <- read_fst(paste("output/daily", feed_date, method, min(date_select), max(date_select), ".fst", sep = "_"))

c0 <- variability_daily %>% filter(quality_range == 0)
c1 <- variability_daily %>% filter(quality_range == 1)
c2 <- variability_daily %>% filter(quality_range == 2)
c3 <- variability_daily %>% filter(quality_range == 3)
c4 <- variability_daily %>% filter(quality_range == 4)
c5 <- variability_daily %>% filter(quality_range == 5)
c6 <- variability_daily %>% filter(quality_range == 6)

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
    st_drop_geometry(variability_daily) %>%
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

outdir <- "appendix/figures/stop_routes"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

filtered_stop_times_dates <- read_fst("code/temp/20260518 weekday filtered_stop_times_dates.fst")

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
    filter(str_detect(stop_id, "de:053")) %>%
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


