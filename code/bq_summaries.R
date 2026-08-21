library(dplyr)
library(dtpyly)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(fst)
library(sf)

#Set up base data
palette_gyr7 <- c(
  "1" = "#169542",
  "2" = "#8acc62",
  "3" = "#dbf09e",
  "4" = "#fedf9a",  
  "5" = "#ef7b4a",
  "6" = "#d7191c",
  "7" = "#3f3f3f"
)
method <- "weekday"
feed_date <- "20260518"

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

gemeinden <- gemeinden %>%
  mutate(krsKN = str_pad(substr(gemeinden$KN, 1,5), 8, "right", 0)) %>%
  left_join(st_drop_geometry(kreise) %>%
              select(GN, KN) %>%
              rename("Kreis" = GN), by = join_by("krsKN" == KN)) %>%
  select(!krsKN)


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

#Calculating statistics on hourly values - One Row per Stop----
variability_hourly <- st_drop_geometry(stops_core) %>%
  group_by(stop_id) %>%
  arrange(date, hour) %>%
  mutate(diff = abs(Bedienungsqualität - lag(Bedienungsqualität))) %>%
  summarise(
    mean_departures = mean(departures_per_hour),
    min_departures = min(departures_per_hour),
    max_departures = max(departures_per_hour),
    mean_bq = mean(Bedienungsqualität, na.rm = TRUE),
    p50_bq = quantile(Bedienungsqualität, probs = 0.5, type = 1),
    best_bq = min(Bedienungsqualität, na.rm = TRUE),
    worst_bq = max(Bedienungsqualität, na.rm = TRUE),
    dist_bq = paste(sort(unique(
      Bedienungsqualität
    )), collapse = ","),
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
  left_join(zhv, by = join_by("stop_id" == "DHID")) %>%
  select(
    Name,
    stop_id,
    Municipality,
    mean_departures,
    min_departures,
    max_departures,
    mean_bq,
    p50_bq,
    best_bq,
    worst_bq,
    dist_bq,
    sum_abs_diff,
    mean_abs_diff,
    n_changes,
    hours_observed,
    modal_quality,
    pct_hours_modal_quality,
    MunicipalityCode,
    geom
  ) %>%
  filter(str_detect(MunicipalityCode, "^053"))


st_write(st_as_sf(
  variability_hourly),
  "results/Bedienungsqualität.gpkg",
  paste
  (
    "variability_hourly",
    feed_date,
    method,
    min(date_select),
    max(date_select),
    ".fst",
    sep = "_"
  ), append = FALSE
)






stops_hour <- read_fst(paste(
  "output/hourly",
  feed_date,
  method,
  min(date_select),
  max(date_select),
  ".fst", sep = "_")
)

counts_krs <- stops_hour %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  left_join(zhv %>%
              select(DHID, Name, MunicipalityCode), by = join_by("stop_id" == "DHID")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  left_join(st_drop_geometry(gemeinden) %>%
              select(KN, Kreis), by = join_by("MunicipalityCode" == "KN")) %>%
  group_by(Bedienungsqualität, Kreis) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts <- counts_krs %>%
  group_by(Kreis) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  ungroup()


counts_krs <- counts_krs %>%
  left_join(total_counts)  %>%
  mutate(Anteil = (count/total_count)*100) %>%
  select(!total_count)

r <- ggplot(counts_krs, aes(x = Kreis, y = Anteil, fill = as.factor(Bedienungsqualität))) +
  geom_col(position = "stack") +
  labs(
    title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse",
    x = "Kreis",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr7) +
  theme(text = element_text(family = windowsFont("Source Sans 3")))

#Anteil Stundenprofile!
r

counts_krs_median <- stops_hour %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  group_by(stop_id) %>%
  summarise(p50_bq = quantile(probs = 0.5, Bedienungsqualität, type = 1)) %>%
  left_join(zhv %>%
              select(DHID, Name, MunicipalityCode), by = join_by("stop_id" == "DHID")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  left_join(st_drop_geometry(gemeinden) %>%
              select(KN, Kreis), by = join_by("MunicipalityCode" == "KN")) %>%
  group_by(p50_bq, Kreis) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts_median <- counts_krs_median %>%
  group_by(Kreis) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  ungroup()

counts_krs_median <- counts_krs_median %>%
  left_join(total_counts_median)  %>%
  mutate(Anteil = (count/total_count)*100) %>%
  select(!total_count)

kreis_order <- counts_krs_median %>%
  group_by(Kreis) %>%
  summarise(
    weighted_p50_bq = weighted.mean(p50_bq, w = count, na.rm = TRUE)
  ) %>%
  arrange(weighted_p50_bq) %>%
  pull(Kreis)

counts_krs_median <- counts_krs_median %>%
  mutate(Kreis = factor(Kreis, levels = kreis_order))

#Anteil Stundenmediane!
k <- ggplot(counts_krs_median, aes(x =  Kreis, y = Anteil, fill = as.factor(p50_bq))) +
  geom_col(position = "stack") +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Kreis",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr7,
                    labels = c(
                      "1" = "I",
                      "2" = "II",
                      "3" = "III",
                      "4" = "IV",
                      "5" = "V",
                      "6" = "VI",
                      "7" = "< 2 Abfahrten / h"
                    )) +
  scale_x_discrete(labels = c(
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
  )) +
  theme(text = element_text(family = windowsFont("Source Sans 3")), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


k
ggsave(k, filename = "document/figures/stops_bq_hour_krs.svg", units = "mm", width = 210, scale = 0.8)

#RegioStaR-----
counts_regio_median <- stops_hour %>%
  replace_na(list(Bedienungsqualität = 8)) %>%
  group_by(stop_id) %>%
  summarise(p50_bq = quantile(probs = 0.5, Bedienungsqualität, type = 1),
            best_bq = min(Bedienungsqualität),
            worst_bq = max(Bedienungsqualität)) %>%
  left_join(zhv %>%
              select(DHID, Name, MunicipalityCode), by = join_by("stop_id" == "DHID")) %>%
  filter(str_detect(MunicipalityCode, "^053")) %>%
  left_join(st_drop_geometry(gemeinden) %>%
              select(KN, RegioStaR17, RegioStaR7), by = join_by("MunicipalityCode" == "KN")) %>%
  group_by(p50_bq, RegioStaR7) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts_median <- counts_regio_median %>%
  group_by(RegioStaR7) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  ungroup()

counts_regio_median <- counts_regio_median %>%
  left_join(total_counts_median)  %>%
  mutate(Anteil = (count/total_count)*100) %>%
  select(!total_count)

regio_order <- counts_regio_median %>%
  group_by(RegioStaR7) %>%
  summarise(
    weighted_p50_bq = weighted.mean(p50_bq, w = count, na.rm = TRUE)
  ) %>%
  arrange(weighted_p50_bq) %>%
  pull(RegioStaR7)

counts_regio_median <- counts_regio_median %>%
  mutate(RegioStaR7 = factor(RegioStaR7, levels = regio_order))

#Anteil Stundenmediane!
r <- ggplot(counts_regio_median, aes(x =  as.factor(RegioStaR7), y = Anteil, fill = as.factor(p50_bq))) +
  geom_col(position = "stack") +
  labs(
    #title = "Anteil Halte pro Kreis und Bedienungsqualitätsklasse (",
    x = "Regionalstatistischer Raumtyp (RegioStaR7)",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr7,
                    labels = c(
                      "1" = "I",
                      "2" = "II",
                      "3" = "III",
                      "4" = "IV",
                      "5" = "V",
                      "6" = "VI",
                      "7" = "< 2 Abfahrten / h"
                    )) +
  theme(text = element_text(family = windowsFont("Source Sans 3")), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


r

ggsave(r, filename = "document/figures/stops_bq_hour_regiostar.svg", units = "mm", width = 210, height = 130, scale = 0.75)
combined <- (k+ (r +
                   theme(
                     axis.text.y = element_blank(),
                     axis.title.y = element_blank()))) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))


combined
ggsave(combined, filename = "document/figures/stops_bq_hour_krsregio.svg", units = "mm", width = 210, height = 130, scale = 0.75)

stops_dailymean <- read_fst(paste("output/daily", feed_date, method, min(date_select), max(date_select), ".fst", sep = "_"))

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
              select(GN, Kreis), by = join_by("Municipality" == "GN"))

for(k in unique(counts_day$Kreis)) {
  p <- ggplot(counts_day %>%
                filter(Kreis == k), aes(x = factor(date), y = percentage, fill = as.factor(Bedienungsqualität))) +
    geom_col(position = "stack") +
    labs(
      title = "Anteil der Halte pro Tag und Bedienungsqualitätsklasse",
      x = "Tag",
      y = "Anteil der Halte in %",
      fill = "Bedienungsqualität"
    ) +
    facet_wrap(facets = "Municipality") +
    scale_fill_manual(values = palette_gyr7) +
    theme(text = element_text(family = windowsFont("Source Sans 3")), legend.position = "none") +
    scale_x_discrete(guide=guide_axis(angle = 90))
  
  ggsave(plot = p, path = "document/figures/krsgemstopsperc", filename = paste0(k,"_stops_per_gem_perc_bq_day.svg"), units = "mm", width = 350, height = 180)
}
for(muni in unique(counts_day$Municipality)) {
  p <- ggplot(counts %>%
                filter(Municipality == muni), aes(x = factor(date), y = percentage, fill = as.factor(Bedienungsqualität))) +
    geom_col(position = "stack") +
    labs(
      title = paste("Anteil Halte pro Tag und Bedienungsqualitätsklasse", muni, sep = "\n"),
      x = "Tag",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(values = palette_gyr) +
    theme(text = element_text(family = windowsFont("Source Sans 3"))) +
    scale_x_discrete(guide=guide_axis(angle = 90))
  
  ggsave(ggsave(plot = p, path = "appendix/figures/percbq/", filename = paste0(muni,"_perc_bq_day.svg")))
}

muni <- "Heimbach"


stops_hourmean <- read_fst(paste("output/hourmean", feed_date, method, min(date_select), max(date_select), ".fst", sep = "_"))

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
              select(GN, Kreis), by = join_by("Municipality" == "GN"))

for(k in unique(counts_hourmean$Kreis)) {
  p <- ggplot(counts_hourmean %>%
                filter(Kreis == k), aes(x = factor(hour), y = percentage, fill = as.factor(Bedienungsqualität))) +
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
  
  ggsave(plot = p, path = "document/figures/krsgemstopsperc/", filename = paste0(k,"_stops_per_gem_perc_bq_hour.svg"), units = "mm", width = 297, height = 210)
}

t <- ggplot(counts_hourmean, aes(x = factor(hour), y = percentage/(length(unique(Municipality))), fill = as.factor(Bedienungsqualität))) +
  geom_col(position = "stack") +
  labs(
    title = "Anteil Halte pro Stunde und Bedienungsqualitätsklasse",
    x = "Stunde des Tages",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr7) +
  theme(text = element_text(family = windowsFont("Source Sans 3")))
t

ggsave(t, units = "mm", width = 210, filename = "document/figures/stops_bq_hour_total.svg")

for(muni in unique(counts_hourmean$Municipality)) {
  p <- ggplot(counts_hourmean %>%
                filter(Municipality == muni), aes(x = factor(hour), y = percentage, fill = as.factor(Bedienungsqualität))) +
    geom_col(position = "stack") +
    labs(
      title = paste("Anteil Halte pro Stunde und Bedienungsqualitätsklasse", muni, sep = "\n"),
      x = "Stunde des Tages",
      y = "Anteil Halte (%)",
      fill = "Bedienungsqualität"
    ) +
    scale_fill_manual(values = palette_gyr) +
    theme(text = element_text(family = windowsFont("Source Sans 3")))
  
  ggsave(ggsave(plot = p, path = "appendix/figures/percbq/hour", filename = paste0(muni,"_perc_bq_hour.svg")))
}

p <- ggplot(counts_day %>%
              filter(Municipality == muni), aes(x = factor(date), y = percentage, fill = as.factor(Bedienungsqualität))) +
  geom_col(position = "stack") +
  labs(
    x = "Tag",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        legend.position =  "none") +
  scale_x_discrete(guide=guide_axis(angle = 90))
q <- ggplot(counts_hourmean %>%
              filter(Municipality == muni), aes(x = factor(hour), y = percentage, fill = as.factor(Bedienungsqualität))) +
  geom_col(position = "stack") +
  labs(
    x = "Stunde",
    y = "Anteil Halte (%)",
    fill = "Bedienungsqualität"
  ) +
  scale_fill_manual(values = palette_gyr) +
  theme(text = element_text(family = windowsFont("Source Sans 3")),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")

combined_plot <- (p + q) +
  plot_layout(ncol = 2, guides = "collect" 
  ) +
  plot_annotation(theme = theme(text = element_text(family = windowsFont("Source Sans 3")), legend.position = "bottom")
  )
combined_plot 
ggsave(combined_plot, filename = "document/figures/heimbach_day_hour.svg", height = 5)               

