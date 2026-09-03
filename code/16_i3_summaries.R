library(here)
library(dplyr)
library(dtplyr)
library(sf)
library(ggplot2)

capture <-lazy_dt(read_fst("output/i3_shortest_hourly/full/captured_mintt.fst"))
zensus_grid <- st_read("geodata/zensus.gpkg", "regbez_zensus_populated") %>%
  filter(!is.na(Einwohner))

summary_total <- capture %>%
  group_by(id) %>%
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

grid <- zensus_grid %>%
  left_join(summary_total, join_by(id))

st_write(grid,"code/temp/i3test.gpkg", "captgrid", append = FALSE)

kreis_total <- summary_total %>%
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
  group_by(capt_mz, Kreis, hour) %>%
  summarise(population = sum(Einwohner), .groups = "drop") %>%
  group_by(Kreis) %>%
  mutate(total_pop = sum(population), .groups = "drop") %>%
  mutate(Anteil = population/total_pop) %>%
  ungroup()

ggplot(kreis_hour, aes(x = as.factor(hour), y = Anteil, fill = capt_mz)) +
  geom_col() +
  facet_wrap(facets = "Kreis")
