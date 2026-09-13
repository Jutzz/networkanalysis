library(here)
library(dplyr)
library(dtplyr)
library(sf)
library(ggplot2)

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
    capt_gz = mean(capt_gz) > 0.5,
    capt_mz = mean(capt_mz) > 0.5,
    capt_oz = mean(capt_oz) > 0.5#,
            #capt_mz_sb = mean(capt_mzsb)
            ) %>%
  as.data.frame() 

grid <- zensus_grid %>%
  filter(str_detect(ags, "^053")) %>%
  left_join(summary_total, join_by(id)) %>%
  left_join(st_drop_geometry(gemeinden), join_by(ags ==  KN))

st_write(grid,"code/temp/i3test.gpkg", "captgrid", append = FALSE)

kreis_total <- grid %>%
  group_by(capt_oz, Kreis) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(Kreis) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

ggplot(kreis_total, aes(x = Kreis, y = Anteil, fill = capt_oz)) +
  geom_col()


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
  left_join(zensus_grid) %>%
  left_join(gemeinden, join_by(ags ==  KN))

kreis_hour <- summary_hour %>%
  group_by(capt_gz, Kreis, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(Kreis) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

ggplot(kreis_hour, aes(x = as.factor(hour), y = Anteil, fill = capt_gz)) +
  geom_col() +
  facet_wrap(facets = "Kreis")
