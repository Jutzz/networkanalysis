library(here)
library(dplyr)
library(dtplyr)
library(sf)
library(fst)
library(ggplot2)
library(extrafont)
library(patchwork)

files.sources = list.files("code/helper/", full.names = TRUE)
sapply(files.sources, source)

source("code/dataenv.R")

capture <-lazy_dt(read_fst("output/i3_shortest_hourly/full/captured_mintt.fst"))
zensus_grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated") %>%
  filter(!is.na(Einwohner))
gemeinden <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250")

safe_min <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    min(x, na.rm = TRUE)
  }
}

summary_total <- capture %>%
  group_by(id) %>%
  summarize(
    min_gz =  as.numeric(safe_min(mind_gz)),
    max_gz =  max(mind_gz),
    mean_gz = mean(mind_gz, na.rm = TRUE),
    min_mz =  as.numeric(safe_min(mind_mz)),
    max_mz =  max(mind_mz),
    mean_mz = mean(mind_mz, na.rm = TRUE),
    min_oz =  as.numeric(safe_min(mind_oz)),
    max_oz =  max(mind_oz),
    mean_oz = mean(mind_oz, na.rm = TRUE),
    perc_gz = mean(capt_gz),
    perc_mz = mean(capt_mz),
    perc_oz = mean(capt_oz),
    capt_gz = mean(capt_gz) >= 0.5,
    capt_mz = mean(capt_mz) >= 0.5,
    capt_oz = mean(capt_oz) >= 0.5#,
    #capt_mz_sb = mean(capt_mzsb)
  ) %>%
  as.data.frame() %>%
  mutate(
    spread_gz = coalesce(max_gz, 61) - min_gz,
    spread_mz = coalesce(max_mz, 61) - min_mz,
    spread_oz = coalesce(max_oz, 61) - min_oz
  )

grid <- zensus_grid %>%
  filter(str_detect(ags, "^053")) %>%
  left_join(summary_total, join_by(id)) %>%
  left_join(st_drop_geometry(gemeinden), join_by(ags ==  KN))

st_write(grid,"results/indikator_03.gpkg", "capture_grid", append = FALSE)

grid <- st_read("results/indikator_03.gpkg", "capture_grid")
plot_data <- st_drop_geometry(grid)

kreis_order <- read_lines("code/kreis_order.txt")

#Aggregation auf Gemeindeebene für Übersichtskarten
gem_summary <- plot_data %>%
  group_by(ags) %>%
  summarise(
    total_pop = sum(Einwohner, na.rm = TRUE),
    capt_gz = sum(Einwohner[capt_gz], na.rm = TRUE),
    capt_mz = sum(Einwohner[capt_mz], na.rm = TRUE),
    capt_oz = sum(Einwohner[capt_oz], na.rm = TRUE),
    mean_gz = mean(mean_gz, na.rm = TRUE),
    mean_mz = mean(mean_mz, na.rm = TRUE),
    mean_oz = mean(mean_oz, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Anteil_gz = capt_gz / total_pop,
    Anteil_mz = capt_mz / total_pop,
    Anteil_oz = capt_oz / total_pop
  ) %>%
  left_join(gemeinden, join_by(ags == KN))

st_write(gem_summary, "results/indikator_03.gpkg", "i3_summary_gem", append = FALSE)
#Plots Erreichbarkeit----
##Aggregieren----
mean_capt <- plot_data %>%
  mutate(across(c(zentralitaet, RegioStaR7, Kreis), as.factor)) %>%
  pivot_longer(
    cols = c(capt_gz, capt_mz, capt_oz),
    names_to = "indicator",
    values_to = "mean_value"
  ) %>%
  pivot_longer(
    cols = c(Kreis, RegioStaR7, zentralitaet),
    names_to = "aggregation",
    values_to = "aggregation_value"
  ) %>%
  mutate(
    class = mean_value
  ) %>%
  group_by(aggregation, aggregation_value, indicator, class) %>%
  summarise(
    population = sum(Einwohner),
    .groups = "drop"
  ) %>%
  group_by(aggregation, aggregation_value, indicator) %>%
  mutate(
    total_pop = sum(population),
    Anteil = 100 * population / total_pop
  ) %>%
  ungroup() %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("capt_gz", "capt_mz", "capt_oz"),
      labels = c("Grundzentrum", "Mittelzentrum", "Oberzentrum")
    ),
    aggregation = factor(
      aggregation,
      levels = c("Kreis", "RegioStaR7", "zentralitaet"),
      labels = c("Kreis", "RegioStaR7", "Zentralität")
    )
  )
##Regiostar----
regiostar_capt_plot <- ggplot(mean_capt %>%
                                filter(aggregation == "RegioStaR7"), aes(x = aggregation_value, y = Anteil, fill = as.factor(class))) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "RegioStaR7",
    y = "Bevölkerungsanteil"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))
##Kreis----
krs_capt_plot <- ggplot(mean_capt %>%
                          filter(aggregation == "Kreis") %>%
                          mutate(Kreis = factor(aggregation_value, levels = kreis_order)), aes(x = Kreis, y = Anteil, fill = as.factor(class))) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "Kreis",
    y = "Bevölkerungsanteil"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
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

##Zentralität----
zent_capt_plot <- ggplot(mean_capt %>%
                           filter(aggregation == "Zentralität"), aes(x = aggregation_value, y = Anteil, fill = as.factor(class))) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "Zentralität",
    y = "Bevölkerungsanteil"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

##Zusammenlegen----
combo_capt <- (krs_capt_plot +
                 theme(
                   axis.title.y = element_blank()
                 ))/
  (regiostar_capt_plot +
     theme(
       strip.background = element_blank(),
       strip.text.x = element_blank()
     ))/
  (zent_capt_plot +
     theme(
       strip.background = element_blank(),
       strip.text.x = element_blank(),
       axis.title.y = element_blank()
     ))+
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom",
                                text = element_text(family = windowsFont("Source Sans 3")))) &
  theme(legend.title = element_blank())

combo_capt

ggsave("document/figures/i3_all_total.svg", combo_capt, units = "mm", width = 204, height = 247)

#Plots Reisezeiten----
##Aggregieren----
mean_class <- plot_data %>%
  mutate(across(c(zentralitaet, RegioStaR7, Kreis), as.factor)) %>%
  pivot_longer(
    cols = c(mean_gz, mean_mz, mean_oz),
    names_to = "indicator",
    values_to = "mean_value"
  ) %>%
  pivot_longer(
    cols = c(Kreis, RegioStaR7, zentralitaet),
    names_to = "aggregation",
    values_to = "aggregation_value"
  ) %>%
  mutate(
    tt_class = if_else(mean_value == 0, 10, ceiling(mean_value / 10) * 10)
  ) %>%
  group_by(aggregation, aggregation_value, indicator, tt_class) %>%
  summarise(
    population = sum(Einwohner),
    .groups = "drop"
  ) %>%
  group_by(aggregation, aggregation_value, indicator) %>%
  mutate(
    total_pop = sum(population),
    Anteil = 100 * population / total_pop
  ) %>%
  ungroup() %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("mean_gz", "mean_mz", "mean_oz"),
      labels = c("Grundzentrum", "Mittelzentrum", "Oberzentrum")
    ),
    aggregation = factor(
      aggregation,
      levels = c("Kreis", "RegioStaR7", "zentralitaet"),
      labels = c("Kreis", "RegioStaR7", "Zentralität")
    ),
    tt_class = factor(
      ifelse(is.na(tt_class), "> 60", as.character(tt_class)),
      levels = c("0", "10", "20", "30", "40", "50", "60", "> 60")
    )
  )
##Kreis----
tt_krs <- ggplot(mean_class %>%
                   filter(aggregation == "Kreis") %>%
                   mutate(Kreis = factor(aggregation_value, levels = kreis_order)), aes(x = Kreis, y = Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "Kreis",
    y = "Bevölkerungsanteil",
    fill = "mittlere Reisezeit (min, <=)"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_tt
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

##Regiostar----
tt_regiostar <- ggplot(mean_class %>%
                         filter(aggregation == "RegioStaR7"), aes(x = aggregation_value, y = Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "RegioStaR7",
    y = "Bevölkerungsanteil",
    fill = "mittlere Reisezeit (min, <=)"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_tt,
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

##Zentralität----
tt_zent <- ggplot(mean_class %>%
                    filter(aggregation == "Zentralität"), aes(x = aggregation_value, y = Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "indicator") +
  labs(
    x = "Zentralität Startort",
    y = "Bevölkerungsanteil",
    fill = "mittlere Reisezeit (min, <=)"
    #title = "Oberzentrum",
  ) +
  scale_fill_manual(
    values = palette_tt
  ) +
  scale_x_discrete(
    labels = c(
      "Grundzentrum" = "GZ",
      "Mittelzentrum" = "MZ",
      "Oberzentrum" = "OZ"
    )
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

##Zusammenlegen----
combo_tt <- (tt_krs +
               theme(
                 axis.title.y = element_blank()
               ))/
  (tt_regiostar +
     theme(
       strip.background = element_blank(),
       strip.text.x = element_blank()
     ))/
  (tt_zent +
     theme(
       strip.background = element_blank(),
       strip.text.x = element_blank(),
       axis.title.y = element_blank()
     ))+
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom",
                                text = element_text(family = windowsFont("Source Sans 3")))) &
  geom_col(position = position_stack(reverse = TRUE))


combo_tt

ggsave("document/figures/i3_tts_agg.svg", combo_tt, units = "mm", width = 204, height = 247)

#Indikatorplots Stündlich je Gemeinde----
##Aggregieren----
summary_hour <- capture %>%
  group_by(id,hour) %>%
  summarize(mean_gz = mean(mind_gz, na.rm = TRUE),
            mean_mz = mean(mind_mz, na.rm = TRUE),
            mean_oz = mean(mind_oz, na.rm = TRUE),
            capt_gz = mean(capt_gz) > 0.5,
            capt_mz = mean(capt_mz) > 0.5,
            capt_oz = mean(capt_oz) > 0.5#,
            #capt_mz_sb = mean(capt_mzsb)
  ) %>%
  as.data.frame() %>%
  left_join(st_drop_geometry(zensus_grid)) %>%
  left_join(st_drop_geometry(gemeinden), join_by(ags ==  KN))

write_fst(summary_hour %>% st_drop_geometry(), "results/i3_summary_by_hour.fst")
summary_hour <- read_fst("results/i3_summary_by_hour.fst")

summary_date <- capture %>%
  group_by(id,date) %>%
  summarize(mean_gz = mean(mind_gz, na.rm = TRUE),
            mean_mz = mean(mind_mz, na.rm = TRUE),
            mean_oz = mean(mind_oz, na.rm = TRUE),
            capt_gz = mean(capt_gz) > 0.5,
            capt_mz = mean(capt_mz) > 0.5,
            capt_oz = mean(capt_oz) > 0.5#,
            #capt_mz_sb = mean(capt_mzsb)
  ) %>%
  as.data.frame() %>%
  left_join(st_drop_geometry(zensus_grid)) %>%
  left_join(st_drop_geometry(gemeinden), join_by(ags ==  KN))

write_fst(summary_date %>% st_drop_geometry(), "results/i3_summary_by_date.fst")

##Plots Indikator----
gem_hour_gz <- summary_hour %>%
  group_by(capt_gz,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

gem_hour_mz <- summary_hour %>%
  group_by(capt_mz,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

gem_hour_oz <- summary_hour %>%
  group_by(capt_oz,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

gem_gz <- ggplot(gem_hour_gz, aes(x = as.factor(hour), y = 100*Anteil, fill = capt_gz)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
    )
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Grundzentrum",
    title = "Angebundene Bevölkerungsanteile: Grundzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") 

gem_mz <- ggplot(gem_hour_mz, aes(x = as.factor(hour), y = 100*Anteil, fill = capt_mz)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
    )
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Mittelzentrum",
    title = "Angebundene Bevölkerungsanteile: Mittelzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") 

gem_oz <- ggplot(gem_hour_oz, aes(x = as.factor(hour), y = 100*Anteil, fill = capt_oz)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_capt,
    labels = c(
      "TRUE" = "Erreicht",
      "FALSE" = "Nicht erreicht"
    )
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Oberzentrum",
    title = "Angebundene Bevölkerungsanteile: Oberzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") 

ggsave("appendix/figures/perci3/Gemeinde_i3-gz_per_hour.svg", gem_gz, units = "mm",
       width = 350, height = 210)

ggsave("appendix/figures/perci3/Gemeinde_i3-mz_per_hour.svg", gem_mz, units = "mm",
       width = 350, height = 210)

ggsave("appendix/figures/perci3/Gemeinde_i3-oz_per_hour.svg", gem_oz, units = "mm",
       width = 350, height = 210)

for(gem in unique(gem_hour_oz$GN)){
  
  gz_plot <- ggplot(
    gem_hour_gz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = capt_gz)
  ) +
    geom_col() +
    scale_fill_manual(
      values = palette_capt,
      labels = c(
        "TRUE" = "Erreicht",
        "FALSE" = "Nicht erreicht"
      )
    ) +
    labs(
      x = "Stunde",
      y = "Bevölkerungsanteil",
      subtitle = "Grundzentrum"
    )
  
  mz_plot <- ggplot(
    gem_hour_mz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = capt_mz)
  ) +
    geom_col() +
    scale_fill_manual(
      values = palette_capt,
      labels = c(
        "TRUE" = "Erreicht",
        "FALSE" = "Nicht erreicht"
      )
    ) +
    labs(
      x = "Stunde",
      y = NULL,
      subtitle = "Mittelzentrum"
    )
  
  oz_plot <- ggplot(
    gem_hour_oz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = capt_oz)
  ) +
    geom_col() +
    scale_fill_manual(
      values = palette_capt,
      labels = c(
        "TRUE" = "Erreicht",
        "FALSE" = "Nicht erreicht"
      )
    ) +
    labs(
      x = "Stunde",
      y = NULL,
      subtitle = "Oberzentrum"
    )
  
  combined_plot <- (gz_plot | mz_plot | oz_plot) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(gem, ": Angebundene Bevölkerung")
    ) &
    theme(
      text = element_text(family = windowsFont("Source Sans 3")),
      legend.position = "bottom", legend.title = element_blank()
    )
  
  
  ggsave(
    paste0(gem, "_i3_per_hour.svg"),
    combined_plot,
    path = "appendix/figures/perci3/hourly_capt/",
    units = "mm",
    width = 420,
    height = 210
  )
}

##Plots Reisezeit----
tt_levels <- names(palette_tt)

tt_scale <- scale_fill_manual(
  values = palette_tt,
  breaks = names(palette_tt),
  limits = names(palette_tt),
  drop = FALSE,
  na.translate = FALSE
)

tt_gem_hour_gz <- summary_hour %>%
  mutate(tt_class = if_else(mean_gz == 0, 10, ceiling(mean_gz / 10) * 10))  %>%
  group_by(tt_class,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup() %>%
  mutate(tt_class = factor(
    ifelse(is.na(tt_class), "> 60", as.character(tt_class)),
    levels = tt_levels
  ))

tt_gem_hour_mz <- summary_hour %>%
  mutate(tt_class = if_else(mean_mz == 0, 10, ceiling(mean_mz / 10) * 10))  %>%
  group_by(tt_class,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup() %>%
  mutate(tt_class = factor(
    ifelse(is.na(tt_class), "> 60", as.character(tt_class)),
    levels = tt_levels
  ))


tt_gem_hour_oz <- summary_hour %>%
  mutate(tt_class = if_else(mean_oz == 0, 10, ceiling(mean_oz / 10) * 10))  %>%
  group_by(tt_class,GN, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(GN, hour) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup() %>%
  mutate(tt_class = factor(
    ifelse(is.na(tt_class), "> 60", as.character(tt_class)),
    levels = tt_levels
  ))


tt_gem_gz <- ggplot(tt_gem_hour_gz, aes(x = as.factor(hour), y = 100*Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_tt
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Mittlere Reisezeit (min, <=)",
    title = "Bevölkerungsanteile je Reisezeit: Grundzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

tt_gem_mz <- ggplot(tt_gem_hour_mz, aes(x = as.factor(hour), y = 100*Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_tt
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Mittlere Reisezeit (min, <=)",
    title = "Bevölkerungsanteile je Reisezeit: Mittelzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


tt_gem_oz <- ggplot(tt_gem_hour_oz, aes(x = as.factor(hour), y = 100*Anteil, fill = tt_class)) +
  geom_col() +
  facet_wrap(facets = "GN") +
  scale_fill_manual(
    values = palette_tt
  ) +
  labs(
    x = "Stunde",
    y = "Bevölkerungsanteil",
    fill = "Mittlere Reisezeit (min, <=)",
    title = "Bevölkerungsanteile je Reisezeit: Oberzentrum"
  ) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

ggsave("appendix/figures/perci3/Gemeinde_tt_i3-gz_per_hour.svg", tt_gem_gz, units = "mm",
       width = 350, height = 210)

ggsave("appendix/figures/perci3/Gemeinde_tt_i3-mz_per_hour.svg", tt_gem_mz, units = "mm",
       width = 350, height = 210)

ggsave("appendix/figures/perci3/Gemeinde_tt_i3-oz_per_hour.svg",tt_gem_oz, units = "mm",
       width = 350, height = 210)

for(gem in unique(tt_gem_hour_oz$GN)){
  
  gz_plot <- ggplot(
    tt_gem_hour_gz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = tt_class)
  ) +
    geom_col(show.legend = TRUE) +
    tt_scale +
    labs(
      x = "Stunde",
      y = "Bevölkerungsanteil",
      subtitle = "Grundzentrum"
    )
  
  mz_plot <- ggplot(
    tt_gem_hour_mz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = tt_class)
  ) +
    geom_col(show.legend = TRUE) +
    tt_scale +
    labs(
      x = "Stunde",
      y = NULL,
      subtitle = "Mittelzentrum"
    ) +
    theme(axis.text.y = element_blank())
  
  oz_plot <- ggplot(
    tt_gem_hour_oz %>% filter(GN == gem),
    aes(x = as.factor(hour), y = 100 * Anteil, fill = tt_class)
  ) +
    geom_col(show.legend = TRUE) +
    tt_scale +
    labs(
      x = "Stunde",
      y = NULL,
      subtitle = "Oberzentrum"
    ) +
    theme(axis.text.y = element_blank())
  
  combined_plot <- (gz_plot | mz_plot | oz_plot) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(gem, ": Reisezeiten")
    ) &
    labs(fill = "Reisezeit (min, <=)") &
    theme(
      text = element_text(family = windowsFont("Source Sans 3")),
      legend.position = "bottom"
    ) &
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(
    paste0(gem, "_tt_i3_per_hour.svg"),
    combined_plot,
    path = "appendix/figures/perci3/hourly_tt/",
    units = "mm",
    width = 420,
    height = 210,
    scale = 0.5
  )
}







