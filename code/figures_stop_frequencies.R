#Change feed date and area name to used feed version (filename set in de_gtfs_cleaning).
feed_date <- "20260518"
zhv_date <- "20260521"
area_name <- "regbez"

#Setup----
options(java.parameters = "-Xmx20G")
library(r5r)
library(tidytransit)
library(gtfstools)
library(tidyverse)
library(plotly)
library(timeDate)
library(here)
library(plotly)
library(sf)
library(httr2)
library(jsonlite)
library(zoo)
library(extrafont)

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
    as.Date(AllSaints(year)),  # All Saints (NRW)
    as.Date(GoodFriday(year)),
    as.Date(EasterMonday(year)),
    as.Date(Ascension(year)),
    as.Date(PentecostMonday(year)),
    as.Date(CorpusChristi(year))  # NRW-specific
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

ggplot(availability, aes(x = active_services)) +
  geom_dotplot(aes(fill = as.factor(month(date))), stackgroups = TRUE, binpositions = "all")

daily_bq <- st_read("geodata/Bedienungsqualität.gpkg", "daily_20260518_weekday_2026-05-04_2026-06-26")

ggplot(daily_bq %>% filter(departures_per_hour <  20&departures_per_hour>2), aes(x =  weekday, y = departures_per_hour)) +
  geom_boxplot()


mean(daily_bq$departures_per_hour)
  