#TODO: Get mean_tt and mean_bq per Gem to see effect of bq vs eq
library(here)
library(dplyr)
library(dtplyr)
library(zoo)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(fst)
library(extrafont)
library(sf)
library(patchwork)
source("code/dataenv.R")

group = "hourly"

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

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  select(id, ags, Einwohner)
gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")
kreise <- st_read("geodata/dvg1nw.gpkg", "kreise_regbez_vg250")

summary <- read_fst("results/i2_summary.fst") %>%
  left_join(zensus_grid)

#Create geodata grid as statistic of all hours.----
summary_grid <- summary %>%
  filter(str_detect(ags, "^053")) %>%
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

summary_ags <- summary %>%
  left_join(gemeinden, join_by("ags" == "KN")) %>%
  filter(str_detect(ags, "^053"))

stats_by_gem <- summary_ags %>%
  group_by(ags, p50_eq) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(ags) %>%
  mutate(total_population = sum(population),
         percentage = round((population / total_population) * 100, 2)) %>%
  ungroup() %>%
  rename("Erschließungsqualität" = p50_eq)

#Kreis aggregieren----
stats_by_krs <- summary_ags %>%
  group_by(Kreis, p50_eq) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Kreis) %>%
  mutate(total_population = sum(population),
         percentage = round((population / total_population) * 100, 2)) %>%
  ungroup() %>%
  rename("Erschließungsqualität" = p50_eq)

kreis_order <- stats_by_krs %>%
  group_by(Kreis) %>%
  summarise(weighted_eq = weighted.mean(Erschließungsqualität, w = population, na.rm = TRUE)) %>%
  arrange(weighted_eq) %>%
  pull(Kreis)

write_lines(kreis_order, "code/kreis_order.txt")

stats_by_krs <- stats_by_krs %>%
  mutate(Kreis = factor(Kreis, levels = kreis_order))

kreis_plot <- ggplot(stats_by_krs, aes(x = Kreis, y = percentage, fill = as.factor(Erschließungsqualität))) +
  geom_col() +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Kreis",
    y = "Bevölkerungsanteil",
    fill = "Erschließungsqualität"
  ) +
  scale_fill_manual(
    values = palette_eq,
    labels = c(
      "1" = "A",
      "2" = "B",
      "3" = "C",
      "4" = "D",
      "5" = "E",
      "6" = "F",
      "7" = "G",
      "8" = "BQ > VI",
      "9" = "Kein Halt erreicht"
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

#RegioStaR aggregieren----
stats_by_regiostar <- summary_ags %>%
  group_by(RegioStaR7, p50_eq) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(RegioStaR7) %>%
  mutate(total_population = sum(population),
         percentage = round((population / total_population) * 100, 2)) %>%
  ungroup() %>%
  rename("Erschließungsqualität" = p50_eq)

regiostar_plot <- ggplot(stats_by_regiostar, aes(x = as.factor(RegioStaR7), y = percentage, fill = as.factor(Erschließungsqualität))) +
  geom_col() +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Regionalstatistischer Raumtyp (RegioStaR7)",
    y = "Bevölkerungsanteil",
    fill = "Erschließungsqualität"
  ) +
  scale_fill_manual(
    values = palette_eq,
    labels = c(
      "1" = "A",
      "2" = "B",
      "3" = "C",
      "4" = "D",
      "5" = "E",
      "6" = "F",
      "7" = "G",
      "8" = "BQ > VI",
      "9" = "Kein Halt erreicht"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

combined <- (kreis_plot + (
  regiostar_plot +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
)) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

combined

ggsave(
  combined,
  filename = "document/figures/pop_eq_hour_krsregio.svg",
  units = "mm",
  width = 210,
  height = 130,
  scale = 0.9
)

#Hourly Plots----
summary_by_hour <- read_fst("results/i2_summary_by_hour.fst") %>%
  left_join(zensus_grid) %>%
  filter(str_detect(ags, "^053")) %>%
  left_join(gemeinden, join_by("ags" == "KN"))

##by Kreis----
pop_hour_krs <- summary_by_hour %>%
  group_by(p50_eq, Kreis, hour) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Kreis, hour) %>%
  mutate(total_pop = sum(population)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (population / total_pop))

ggplot(pop_hour_krs, aes(x = as.factor(hour), y = Anteil, fill = as.factor(p50_eq))) +
  geom_col() +
  scale_fill_manual(values = palette_eq) +
  facet_wrap(facets = "Kreis")

##by RegioStaR----
pop_hour_regio <- summary_by_hour %>%
  group_by(p50_eq, RegioStaR7, hour) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(RegioStaR7, hour) %>%
  mutate(total_pop = sum(population)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (population / total_pop))

ggplot(pop_hour_regio, aes(x = as.factor(hour), y = Anteil, fill = as.factor(p50_eq))) +
  geom_col() +
  scale_fill_manual(values = palette_eq) +
  facet_wrap(facets = "RegioStaR7")

##by Gemeinde----
pop_hour_gem <- summary_by_hour %>%
  group_by(p50_eq, GN, hour) %>%
  summarise(population = sum(Einwohner, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population)) %>%
  ungroup() %>%
  mutate(Anteil = 100 * (population / total_pop))

plot_pop_hour_gem <- ggplot(pop_hour_gem, aes(x = as.factor(hour), y = Anteil, fill = as.factor(p50_eq))) +
  geom_col() +
  facet_wrap(facets = "GN") +
  labs(
    title = paste0("Bevölkerungsanteil pro Erschließungsqualitätsklasse"),
    x = "Stunde",
    y = "Bevölkerung (%)",
    fill = "Erschließungsqualität"
  ) +
  scale_fill_manual(
    values = palette_eq,
    labels = c(
      "1" = "A",
      "2" = "B",
      "3" = "C",
      "4" = "D",
      "5" = "E",
      "6" = "F",
      "7" = "G",
      "8" = "BQ > VI",
      "9" = "Kein Halt erreicht"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

plot_pop_hour_gem

ggsave(plot_pop_hour_gem,
       filename = "appendix/figures/perceq/Gemeinde_eq_per_hour.svg",
       units = "mm",
       height = 210,
       width = 297)

for(GN in unique(pop_hour_gem$GN)){
  hours <- pop_hour_gem %>%
    filter(GN == !!GN)
  
  plot <- ggplot(hours, aes(x = as.factor(hour), y = Anteil, fill = as.factor(p50_eq))) +
    geom_col() +
    labs(
      title = paste0(GN),
      x = "Stunde",
      y = "Bevölkerung (%)",
      fill = "Erschließungsqualität"
    ) +
    scale_fill_manual(
      values = palette_eq,
      labels = c(
        "1" = "A",
        "2" = "B",
        "3" = "C",
        "4" = "D",
        "5" = "E",
        "6" = "F",
        "7" = "G",
        "8" = "BQ > VI",
        "9" = "Kein Halt erreicht"
      )
    ) +
    theme(text = element_text(family = windowsFont("Source Sans 3")),
          legend.position = "none") +
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(plot,
         filename = paste0("appendix/figures/perceq/hourly/", GN, "_eq_per_hour.svg"),
         units = "mm",
         height = 210,
         width = 210,
         scale = 0.5)
  
  ggsave(plot,
         filename = paste0("appendix/figures/perceq/hourly/foratlas/", GN, "_eq_per_hour.png"),
         units = "mm",
         height = 210,
         width = 210,
         scale = 0.5)
}


#Map Polygons with dominant accessibility and wide format for QGIS diagram----
dominant_accessibility <- as.data.frame(stats_by_gem) %>%
  group_by(ags) %>%
  slice_max(percentage, with_ties = FALSE) %>%
  select(ags, Erschließungsqualität) %>%
  rename(größterAnteil = Erschließungsqualität)

soll_eq <- tribble(~"RegioStaR7", ~"class", ~"perc",
                   71,"D",95,
                   72,"E",95,
                   73,"F",85,
                   74,"F",65,
                   75,"F",90,
                   76,"F",75,
                   77,"F",55)

stats_by_gem_wide <- stats_by_gem %>%
  mutate(Erschließungsqualität = LETTERS[Erschließungsqualität]) %>%
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
      percentage_A * 1 +
        percentage_B * 2 +
        percentage_C * 3 +
        percentage_D * 4 +
        percentage_E * 5 +
        percentage_F * 6 +
        percentage_G * 7 +
        percentage_H * 8 +
        percentage_I * 9
    ) / 100
  ) %>%
  st_as_sf() %>%
  left_join(soll_eq) %>%
  rowwise() %>%
  mutate(
    percentage_sum = sum(
      c_across(starts_with("percentage_"))[
        1:match(
          paste0("percentage_", class),
          names(pick(starts_with("percentage_")))
        )
      ],
      na.rm = TRUE
    ),
    meets_soll = percentage_sum >= perc
  ) %>%
  ungroup() %>%
  mutate(soll_diff = percentage_sum - perc)

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
    
profile <- eq_profile("100mN30734E41340", group = "hour")

p <- ggplot(profile, aes(x = timestamp, y = departures_per_hour)) +
  geom_line() +
  #geom_path(group = "timestamp") +
  geom_point(aes(color = as.factor(Erschließungsqualität))) 
#scale_y_reverse()

p
ggplotly(p)

  