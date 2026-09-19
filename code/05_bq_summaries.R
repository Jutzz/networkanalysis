#This Script is for evaluating hourly Bedienungsqualität - not for calculatating
#Bedienungsqualität based on aggregated means of departures per hour!
library(dplyr)
library(dtplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(fst)
library(sf)
library(here)
library(extrafont)
source("code/dataenv.R")

#Set up base data

method <- "weekday"

#Get dates of representative norm- and weekdays from checking in find_valid_dates.R
nonholiday_weekdays_fullservice <- read_lines("code/temp/nonholiday_weekdays_cutoff.txt")
nonholiday_normdays_fullservice <- read_lines("code/temp/nonholiday_normdays_cutoff.txt")
#Choose number of dates based on method set above.
ifelse(
  method == "weekday",
  date_select <- nonholiday_weekdays_fullservice,
  date_select <- nonholiday_normdays_fullservice
)

gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")
kreise <- st_read("geodata/dvg1nw.gpkg", "dvg1krs_regbez")

zhv <- st_read(here("geodata/poi.gpkg"), paste0("zhv_", zhv_date))

#This is the base for everything. These are the actual hour values based on
#departures per hour at this hour. No aggregation.
stops_core <- lazy_dt(read_fst(
  paste(
    "output/hourly",
    feed_date,
    method,
    min(date_select),
    max(date_select),
    ".fst",
    sep = "_"
  )
))

#Variability Stats----
#Calculating statistics on hourly values - One Row per Stop
#TODO: Do this for daily variation and meanhour OF BEDIENUNGSQUALITÄT not departures.
variability_hourly <- st_drop_geometry(stops_core) %>%
  group_by(stop_id) %>%
  arrange(date, hour, .by_group = TRUE) %>%
  mutate(diff = abs(Bedienungsqualität - lag(Bedienungsqualität))) %>%
  summarise(
    mean_departures = mean(departures_per_hour),
    min_departures = min(departures_per_hour),
    max_departures = max(departures_per_hour),
    mean_routes = mean(n_routes),
    min_routes = min(n_routes),
    max_routes = max(n_routes),
    mean_bq = mean(Bedienungsqualität, na.rm = TRUE),
    p50_bq = quantile(Bedienungsqualität, probs = 0.5, type = 1),
    best_bq = min(Bedienungsqualität, na.rm = TRUE),
    worst_bq = max(Bedienungsqualität, na.rm = TRUE),
    dist_bq = paste(sort(unique(Bedienungsqualität)), collapse = ","),
    sum_abs_diff = sum(diff, na.rm = TRUE),
    mean_abs_diff = mean(diff, na.rm = TRUE),
    n_changes = sum(Bedienungsqualität != lag(Bedienungsqualität), na.rm = TRUE),
    hours_observed = n(),
    modal_quality = as.numeric(names(which.max(
      table(Bedienungsqualität)
    ))),
    pct_hours_modal_quality =
      100 * max(table(Bedienungsqualität)) / n(),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  mutate(range_bq = worst_bq - best_bq) %>%
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  select(
    Name,
    stop_id,
    Municipality,
    mean_routes,
    min_routes,
    max_routes,
    mean_departures,
    min_departures,
    max_departures,
    mean_bq,
    p50_bq,
    best_bq,
    worst_bq,
    range_bq,
    dist_bq,
    sum_abs_diff,
    mean_abs_diff,
    n_changes,
    hours_observed,
    modal_quality,
    pct_hours_modal_quality,
    MunicipalityCode,
    geom
  ) 

#Write full set of stops for ttm-Calculation
st_write(
  st_as_sf(variability_hourly),
  "output/Bedienungsqualität.gpkg",
  paste
  (
    "variability_hourly",
    feed_date,
    method,
    min(date_select),
    max(date_select),
    sep = "_"
  ),
  append = FALSE
)

#Write filtered set of stops for maps
st_write(
  st_as_sf(variability_hourly %>%
             filter(str_detect(MunicipalityCode, "^053"))),
  "results/Bedienungsqualität.gpkg",
  paste
  (
    "variability_hourly",
    feed_date,
    method,
    min(date_select),
    max(date_select),
    sep = "_"
  ),
  append = FALSE
)

variability_hourly_gem <- variability_hourly %>%
  group_by(MunicipalityCode, p50_bq) %>%
  reframe(
    stops = n(),
    mean_mode = mean(pct_hours_modal_quality),
    sumc = sum(n_changes) / stops
  )

#Total Plots----
stops_total <- stops_core %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  group_by(stop_id) %>%
  summarise(p50_bq = quantile(Bedienungsqualität, probs = 0.5, type = 1)) %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  left_join(gemeinden, by = join_by("MunicipalityCode" == "KN")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  select(stop_id, Name, p50_bq, GN, MunicipalityCode, RegioStaR7, Kreis)


#Zählen nach Kreis
counts_total_krs <- stops_total %>%
  group_by(p50_bq, Kreis) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Kreis) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach RegioStaR
counts_total_regiostar <- stops_total %>%
  group_by(p50_bq, RegioStaR7) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach Gemeinde
counts_total_gem <- stops_total %>%
  group_by(p50_bq, GN) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(GN) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

##Plotten----
kreis_order <- counts_total_krs %>%
  group_by(Kreis) %>%
  summarise(weighted_p50_bq = weighted.mean(p50_bq, w = count, na.rm = TRUE)) %>%
  arrange(weighted_p50_bq) %>%
  pull(Kreis)

counts_total_krs <- counts_total_krs %>%
  mutate(Kreis = factor(Kreis, levels = kreis_order))

total_kreis_plot <- ggplot(counts_total_krs, aes(
  x = Kreis,
  y = Anteil,
  fill = as.factor(p50_bq)
)) +
  geom_col() +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Kreis",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Bonn" = "BN",
      "Düren" = "DN",
      "Euskirchen" = "EU",
      "Heinsberg" = "HS",
      "Köln" = "K",
      "Leverkusen" = "LEV",
      "Städteregion Aachen" = "AC",
      "Oberbergischer Kreis" = "OBK",
      "Rhein-Erft-Kreis" = "REK",
      "Rhein-Sieg-Kreis" = "RSK",
      "Rheinisch-Bergischer Kreis" = "RBK"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

total_regio_plot <- ggplot(counts_total_regiostar,
                           aes(
                             x = as.factor(RegioStaR7),
                             y = Anteil,
                             fill = as.factor(p50_bq)
                           )) +
  geom_col() +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Kreis",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

combined <- (total_kreis_plot + (
  total_regio_plot +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
)) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))


combined
ggsave(
  combined,
  filename = "document/figures/stops_bq_hour_krsregio.svg",
  units = "mm",
  width = 210,
  height = 130,
  scale = 0.75
)


#Hourmean plots----
#Variation grouped by hour of the day
##Welche Bedieungsqualität hat ein Halt "meistens", je Stunde. p50 der BQ.
stops_ghour <- stops_core %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  group_by(stop_id, hour) %>%
  summarise(p50_bq = quantile(Bedienungsqualität, probs = 0.5, type = 1)) %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  left_join(gemeinden, by = join_by("MunicipalityCode" == "KN")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  select(stop_id,
         Name,
         hour,
         p50_bq,
         GN,
         MunicipalityCode,
         RegioStaR7,
         Kreis)

#Zählen nach Kreis
counts_ghour_krs <- stops_ghour %>%
  group_by(p50_bq, Kreis, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Kreis, hour) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach RegioStaR
counts_ghour_regiostar <- stops_ghour %>%
  group_by(p50_bq, RegioStaR7, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7, hour) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach Gemeinde
counts_ghour_gem <- stops_ghour %>%
  group_by(p50_bq, GN, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(GN , hour) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops)) %>%
  rename("Gemeinde" = GN)

counts_ghour <- list("Kreis" = counts_ghour_krs,
                     "Gemeinde" = counts_ghour_gem,
                     "RegioStaR7" = counts_ghour_regiostar)
##Plotten----
plot_group <- "Gemeinde"
plotscount <- counts_ghour[[plot_group]]
hour_plot <- ggplot(plotscount, aes(
  x = as.factor(hour),
  y = Anteil,
  fill = as.factor(p50_bq)
)) +
  geom_col() +
  facet_wrap(facets = plot_group) +
  labs(
    title = paste0("Anteil Halte pro ", plot_group, " und Bedienungsqualitätsklasse"),
    x = "Stunde",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

ggsave(
  plot = hour_plot,
  filename = paste0("appendix/figures/percbq/", plot_group, "_bq_per_hour.svg"),
  units = "mm",
  height = 210,
  width = 297
)

##Single plot per Gemeinde ----
for (GN in unique(stops_ghour$GN)) {
  counts <- stops_ghour %>%
    filter(GN == !!GN) %>%
    group_by(p50_bq, hour) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(total_stops = sum(count)) %>%
    mutate(Anteil = 100 * (count / total_stops))
  
  single_plot <- ggplot(counts, aes(
    x = as.factor(hour),
    y = Anteil,
    fill = as.factor(p50_bq)
  )) +
    geom_col() +
    labs(
      title = paste0("Anteil Halte in ", GN, " je Bedienungsqualitätsklasse"),
      x = "Stunde",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(
      values = palette_gyr7,
      labels = c(
        "1" = "I",
        "2" = "II",
        "3" = "III",
        "4" = "IV",
        "5" = "V",
        "6" = "VI",
        "7" = "< 2 Abfahrten / h"
      )
    ) +
    theme(text = element_text(family = windowsFont("Source Sans 3")),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(
    single_plot,
    filename = paste0(
      "appendix/figures/percbq/hourly_gem/",
      GN,
      "_hourly_bq.svg"
    )
  )
}

#Daily Plots----

#Variation grouped by date
##Welche Bedieungsqualität hat ein Halt "meistens", je Datum. p50 der BQ.
stops_gdate <- stops_core %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  group_by(stop_id, date) %>%
  summarise(p50_bq = quantile(Bedienungsqualität, probs = 0.5, type = 1)) %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  left_join(gemeinden, by = join_by("MunicipalityCode" == "KN")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  select(stop_id,
         Name,
         date,
         p50_bq,
         GN,
         MunicipalityCode,
         RegioStaR7,
         Kreis)

#Zählen nach Kreis
counts_gdate_krs <- stops_gdate %>%
  group_by(p50_bq, Kreis, date) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Kreis, date) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach RegioStaR
counts_gdate_regiostar <- stops_gdate %>%
  group_by(p50_bq, RegioStaR7, date) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RegioStaR7, date) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops))

#Zählen nach Gemeinde
counts_gdate_gem <- stops_gdate %>%
  group_by(p50_bq, GN, date) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(GN , date) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops)) %>%
  rename("Gemeinde" = GN)

counts_gdate <- list("Kreis" = counts_gdate_krs,
                     "Gemeinde" = counts_gdate_gem,
                     "RegioStaR7" = counts_gdate_regiostar)
##Plotten----
plot_group <- "Gemeinde"
plotscount <- counts_gdate[[plot_group]]
date_plot <- ggplot(plotscount, aes(
  x = as.factor(date),
  y = Anteil,
  fill = as.factor(p50_bq)
)) +
  geom_col() +
  facet_wrap(facets = plot_group) +
  labs(
    title = paste0("Anteil Halte pro ", plot_group, " und Bedienungsqualitätsklasse"),
    x = "Datum",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  NULL

ggsave(
  plot = date_plot,
  filename = paste0("appendix/figures/percbq/", plot_group, "_bq_per_date.svg"),
  units = "mm",
  height = 210,
  width = 297)

##Single plot per Gemeinde ----
for (GN in unique(stops_gdate$GN)) {
  counts <- stops_gdate %>%
    filter(GN == !!GN) %>%
    group_by(p50_bq, date) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(date) %>%
    mutate(total_stops = sum(count)) %>%
    mutate(Anteil = 100 * (count / total_stops))
  
  single_plot <- ggplot(counts, aes(
    x = as.factor(date),
    y = Anteil,
    fill = as.factor(p50_bq)
  )) +
    geom_col() +
    labs(
      title = paste0("Anteil Halte in ", GN, " je Bedienungsqualitätsklasse"),
      x = "Tag",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(
      values = palette_gyr7,
      labels = c(
        "1" = "I",
        "2" = "II",
        "3" = "III",
        "4" = "IV",
        "5" = "V",
        "6" = "VI",
        "7" = "< 2 Abfahrten / h"
      )
    ) +
    theme(
      text = element_text(family = windowsFont("Source Sans 3")),
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(
    single_plot,
    filename = paste0(
      "appendix/figures/percbq/dately_gem/",
      GN,
      "_dately_bq.svg"
    )
  )
}

#Combined plots----
muni <- "Heimbach"

muni_date <- ggplot(
  counts_gdate_gem %>%
    filter(Gemeinde == muni),  aes(
      x = as.factor(date),
      y = Anteil,
      fill = as.factor(p50_bq)
  )
) +
  geom_col() +
  labs(
    x = "Tag",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  guides(fill = guide_legend(nrow = 1))

muni_hour <- ggplot(
  counts_ghour_gem %>%
    filter(Gemeinde == muni),  aes(
      x = as.factor(hour),
      y = Anteil,
      fill = as.factor(p50_bq)
    )
) +
  geom_col() +
  labs(
    x = "Stunde",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1))

combined_plot <- (muni_date + (muni_hour +
                                 theme(axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),))) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(theme = theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom"
  ))

combined_plot

ggsave(combined_plot, filename = "document/figures/heimbach_day_hour.svg", height = 5)

#TAGESMITTEL for comparison between methods and visualization.

stops_day <- lazy_dt(read_fst(
  paste(
    "output/daily",
    feed_date,
    method,
    min(date_select),
    max(date_select),
    ".fst",
    sep = "_"
  )
))

stops_gdateday <- stops_day %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  as.data.frame() %>%
  left_join(gemeinden, by = join_by("MunicipalityCode" == "KN")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  select(stop_id,
         date,
         Bedienungsqualität,
         GN,
         MunicipalityCode,
         RegioStaR7,
         Kreis)


#Zählen nach Gemeinde
counts_gdateday_gem <- stops_gdateday %>%
  group_by(Bedienungsqualität, GN, date) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(GN , date) %>%
  mutate(total_stops = sum(count)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (count / total_stops)) %>%
  rename("Gemeinde" = GN)

muni_date_day <- ggplot(
  counts_gdateday_gem %>%
    filter(Gemeinde == muni),  aes(
      x = as.factor(date),
      y = Anteil,
      fill = as.factor(Bedienungsqualität)
    )
) +
  geom_col() +
  labs(
    x = "Tag",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  guides(fill = guide_legend(nrow = 1))

muni_ind <- stops_core %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  as.data.frame() %>%
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  left_join(gemeinden, by = join_by("MunicipalityCode" == "KN")) %>%
  filter(GN == muni) %>%
  mutate(timestamp = ymd_hms(paste(date, hms::hms(hours = hour)), tz = "Europe/Berlin"))
  
muni_ind_count <- muni_ind %>%
  group_by(Bedienungsqualität, timestamp) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(timestamp) %>%
  mutate(total_stops = sum(count)) %>%
  mutate(Anteil = 100*(count/total_stops))

plot_muni_ind <- ggplot(muni_ind_count, aes(x = as.factor(timestamp), y = Anteil, fill = as.factor(Bedienungsqualität))) +
  geom_col() +
  scale_fill_manual(values = palette_gyr7) +
  labs(
    x = "1-h-Intervall",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(
    values = palette_gyr7,
    labels = c(
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "< 2 Abfahrten / h"
    )
  ) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "none",
    axis.text.x =  element_blank())
  

muni_date_day
plot_muni_ind
ggsave(plot_muni_ind, filename = "document/figures/bq_heimbach_actual.svg", width = 120, units = "mm")
ggsave(muni_date_day, filename = "document/figures/bq_heimbach_daymean.svg", width = 120, units = "mm")


combined_plot <- ((muni_date) | muni_date_day  +
                    theme(axis.title.y = element_blank(),
                          axis.text.y = element_blank())) / (plot_muni_ind) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom"
  ))


ggsave(combined_plot, filename = "document/figures/heimbach_day_hour.svg", height = 7)










#OLD WITH WRONG FILES-------------
stops_dailymean <- read_fst(paste(
  "output/daily",
  feed_date,
  method,
  min(date_select),
  max(date_select),
  ".fst",
  sep = "_"
))

counts <- stops_dailymean %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  group_by(date, Bedienungsqualität, Municipality) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts <- counts %>%
  group_by(date, Municipality) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  ungroup()

counts_day <- counts %>%
  left_join(total_counts, by = c("date", "Municipality")) %>%
  mutate(percentage = (count / total_count) * 100) %>%
  select(date, Bedienungsqualität, Municipality, count, percentage) %>%
  left_join(st_drop_geometry(gemeinden) %>%
              select(GN, Kreis),
            by = join_by("Municipality" == "GN"))

for (k in unique(counts_day$Kreis)) {
  p <- ggplot(counts_day %>%
                filter(Kreis == k),
              aes(
                x = factor(date),
                y = percentage,
                fill = as.factor(Bedienungsqualität)
              )) +
    geom_col(position = "stack") +
    labs(
      title = "Anteil der Halte pro Tag und Bedienungsqualitätsklasse",
      x = "Tag",
      y = "Anteil der Halte in %",
      fill = "Bedienungsqualität"
    ) +
    facet_wrap(facets = "Municipality") +
    scale_fill_manual(values = palette_gyr7) +
    theme(text = element_text(family = windowsFont("Source Sans 3")),
          legend.position = "none") +
    scale_x_discrete(guide = guide_axis(angle = 90))
  
  ggsave(
    plot = p,
    path = "document/figures/krsgemstopsperc",
    filename = paste0(k, "_stops_per_gem_perc_bq_day.svg"),
    units = "mm",
    width = 350,
    height = 180
  )
}
for (muni in unique(counts_day$Municipality)) {
  p <- ggplot(
    counts %>%
      filter(Municipality == muni),
    aes(
      x = factor(date),
      y = percentage,
      fill = as.factor(Bedienungsqualität)
    )
  ) +
    geom_col(position = "stack") +
    labs(
      title = paste(
        "Anteil Halte pro Tag und Bedienungsqualitätsklasse",
        muni,
        sep = "\n"
      ),
      x = "Tag",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(values = palette_gyr) +
    theme(text = element_text(family = windowsFont("Source Sans 3"))) +
    scale_x_discrete(guide = guide_axis(angle = 90))
  
  ggsave(ggsave(
    plot = p,
    path = "appendix/figures/percbq/",
    filename = paste0(muni, "_perc_bq_day.svg")
  ))
}



stops_hourmean <- read_fst(paste(
  "output/hourmean",
  feed_date,
  method,
  min(date_select),
  max(date_select),
  ".fst",
  sep = "_"
))

counts <- stops_hourmean %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  group_by(hour, Bedienungsqualität, Municipality) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts <- counts %>%
  group_by(hour, Municipality) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  ungroup()

counts_hourmean <- counts %>%
  left_join(total_counts, by = c("hour", "Municipality")) %>%
  mutate(percentage = (count / total_count) * 100) %>%
  select(hour, Bedienungsqualität, Municipality, count, percentage) %>%
  left_join(st_drop_geometry(gemeinden) %>%
              select(GN, Kreis),
            by = join_by("Municipality" == "GN"))

for (k in unique(counts_hourmean$Kreis)) {
  p <- ggplot(
    counts_hourmean %>%
      filter(Kreis == k),
    aes(
      x = factor(hour),
      y = percentage,
      fill = as.factor(Bedienungsqualität)
    )
  ) +
    geom_col(position = "stack") +
    labs(
      title = "Anteil Halte pro Stunde und Bedienungsqualitätsklasse",
      x = "Stunde des Tages",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    facet_wrap(facets = "Municipality") +
    scale_fill_manual(values = palette_gyr7) +
    theme(text = element_text(family = windowsFont("Source Sans 3")))
  
  ggsave(
    plot = p,
    path = "document/figures/krsgemstopsperc/",
    filename = paste0(k, "_stops_per_gem_perc_bq_hour.svg"),
    units = "mm",
    width = 297,
    height = 210
  )
}

t <- ggplot(counts_hourmean,
            aes(
              x = factor(hour),
              y = percentage / (length(unique(Municipality))),
              fill = as.factor(Bedienungsqualität)
            )) +
  geom_col(position = "stack") +
  labs(title = "Anteil Halte pro Stunde und Bedienungsqualitätsklasse",
       x = "Stunde des Tages",
       y = "Anteil Halte (%)",
       fill = "Bedienungsqualität") +
  scale_fill_manual(values = palette_gyr7) +
  theme(text = element_text(family = windowsFont("Source Sans 3")))
t

ggsave(t,
       units = "mm",
       width = 210,
       filename = "document/figures/stops_bq_hour_total.svg")

for (muni in unique(counts_hourmean$Municipality)) {
  p <- ggplot(
    counts_hourmean %>%
      filter(Municipality == muni),
    aes(
      x = factor(hour),
      y = percentage,
      fill = as.factor(Bedienungsqualität)
    )
  ) +
    geom_col(position = "stack") +
    labs(
      title = paste(
        "Anteil Halte pro Stunde und Bedienungsqualitätsklasse",
        muni,
        sep = "\n"
      ),
      x = "Stunde des Tages",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(values = palette_gyr) +
    theme(text = element_text(family = windowsFont("Source Sans 3")))
  
  ggsave(ggsave(
    plot = p,
    path = "appendix/figures/percbq/hour",
    filename = paste0(muni, "_perc_bq_hour.svg")
  ))
}

p <- ggplot(
  counts_day %>%
    filter(Municipality == muni),
  aes(
    x = factor(date),
    y = percentage,
    fill = as.factor(Bedienungsqualität)
  )
) +
  geom_col(position = "stack") +
  labs(x = "Tag", y = "Anteil Halte (%)", fill = "Bedienungsqualität") +
  scale_fill_manual(values = palette_gyr) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position =  "none") +
  scale_x_discrete(guide = guide_axis(angle = 90))
q <- ggplot(
  counts_hourmean %>%
    filter(Municipality == muni),
  aes(
    x = factor(hour),
    y = percentage,
    fill = as.factor(Bedienungsqualität)
  )
) +
  geom_col(position = "stack") +
  labs(x = "Stunde", y = "Anteil Halte (%)", fill = "Bedienungsqualität") +
  scale_fill_manual(values = palette_gyr) +
  theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  )

combined_plot <- (p + q) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(theme = theme(
    text = element_text(family = windowsFont("Source Sans 3")),
    legend.position = "bottom"
  ))
combined_plot
ggsave(combined_plot, filename = "document/figures/heimbach_day_hour.svg", height = 5)
