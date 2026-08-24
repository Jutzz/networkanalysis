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
library(patchwork)
source("code/dataenv.R")

group = "hourly"

zensus_grid <- st_read(here("geodata/zensus.gpkg"), "regbez_zensus_populated") %>%
  select(id, ags, Einwohner)
gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")
kreise <- st_read("geodata/dvg1nw.gpkg", "kreise_regbez_vg250")

summary <- read_fst("results/i2_summary.fst") %>%
  left_join(zensus_grid)

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












#Map Polygons with dominant accessibility and wide format for QGIS diagram----
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
    
  

  