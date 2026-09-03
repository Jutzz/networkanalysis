library(dplyr)
library(tibble)
library(rosmium)
library(sf)
library(readr)
library(stringr)
library(httr2)
library(here)
library(readxl)
library(lubridate)
options(timeout = 1000)


regbez10km <- st_read("geodata/regbez10kmbuffer.geojson")
regbez25km <- st_read("geodata/regbez25kmbuffer.geojson")

kürzel <- c(
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

vg_250_krs <- st_read("geodata/base_data/DE_VG250.gpkg", query = 
                        "SELECT GEN,AGS_0,geom FROM vg250_krs WHERE AGS LIKE '053%'") %>%
  rename("GN" = GEN,
         "KN" = AGS_0) %>%
  left_join(enframe(kürzel, name = "GN", value = "kürzel"))

regiostar <- read_xlsx("geodata/base_data/2024 RegioStaR-Referenzdateien_Mobilthek.xlsx", sheet = "ReferenzGebietsstand2024") %>%
  select(gem_24, RegioStaR17, RegioStaR7) %>%
  filter(str_detect(gem_24, "^53")) %>%
  mutate(gem_24 = str_pad(gem_24, width = 8, side = "left", pad = "0"))

gem_regbez <- st_read("geodata/dvg1nw.gpkg", "gemeinden_regbez_kln") %>%
  select(KN, zentralitaet)  %>%
  st_drop_geometry()

vg_250 <- st_read("geodata/base_data/DE_VG250.gpkg", query = 
                    "SELECT GEN,AGS,geom FROM vg250_gem WHERE AGS LIKE '05%' OR ags LIKE '07%'") %>%
  st_filter(st_transform(regbez25km, crs = st_crs(25832)), .predicate = st_intersects) %>%
  rename("GN" = GEN,
         "KN" = AGS) %>%
  mutate(regbez = ifelse(str_detect(KN, "^053"), TRUE, FALSE)) %>%
  left_join(gem_regbez)

vg_250_regbez <- vg_250 %>%
  filter(str_detect(KN, "^053")) %>%
  left_join(regiostar, join_by("KN" == "gem_24"))

vg_250_regbez <- vg_250_regbez %>%
  mutate(KRSKN = str_pad(substr(KN, 0,5), width = 8, side = "right", pad = "0")) %>%
  left_join(st_drop_geometry(vg_250_krs %>%
                               rename("Kreis" = GN)), by = join_by("KRSKN" == "KN")) %>%
  select(!c("KRSKN", "kürzel"))

st_write(vg_250, "geodata/dvg1nw.gpkg", "gemeinden_regbez_25km", append = FALSE)
st_write(vg_250_regbez, "geodata/dvg1nw.gpkg", "gemeinden_regbez_vg250", append = FALSE)
st_write(vg_250_krs, "geodata/dvg1nw.gpkg", "kreise_regbez_vg250", append = FALSE)


dldate <- format(Sys.Date(), format = "%Y%m%d")
dldate_zhv <- format(Sys.Date(), format = "%Y-%m-%d")
#Download latest OSM-extract for NRW.
osm_req <- request("https://download.geofabrik.de/europe/germany/nordrhein-westfalen-latest.osm.pbf")
osmresp <- req_perform(osm_req, path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"))

#Extract Regierungsbezirk plus buffer. Osmium needs to be locally available.Using 25 km for Indikator 03 (50 km Radius catchment)
rosmium::extract(input_path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"), extent = regbez10km, output_path = paste0("osmdata/regbez10km-", dldate, ".osm.pbf"), overwrite = TRUE)
rosmium::extract(input_path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"), extent = regbez25km, output_path = paste0("osmdata/regbez25km-", dldate, ".osm.pbf"), overwrite = TRUE)
rosmium::extract(input_path = paste0("osmdata/nordrhein-westfalen-", dldate, ".osm.pbf"), extent = st_bbox(st_transform(vg_250, crs = st_crs(4326))), output_path = paste0("osmdata/dvgregbez25km-", dldate, ".osm.pbf"), overwrite = TRUE)

zhv_req <- request("https://www.opendata-oepnv.de/fileadmin/datasets/delfi/20260521_zHV_gesamt.zip")
zhvresp <- req_perform(zhv_req, path = paste0("geodata/zhv/", dldate, "_zHV_gesamt.zip"))

unzip(zipfile = "geodata/zhv/zHV_aktuell_csv.2026-05-21.zip",exdir = paste0("geodata/zhv/", dldate, "_zHV_gesamt"))

zhv <- read_csv2(paste0("geodata/zhv/",dldate,"_zHV_gesamt/zHV_aktuell_csv.",dldate_zhv,".csv"))

zhv_geo <- st_as_sf(zhv, coords = c("Longitude", "Latitude"), crs = st_crs(4326))

st_write(zhv_geo, here("geodata/poi.gpkg"), paste0("zhv_",dldate), append = FALSE)

