library(sf)
library(tidyverse)

laea_grid <- st_read("geodata/grids.gpkg", "100mregbez10kmbuffer")
zensus_csv <- read_csv2("geodata/base_data/Zensus2022_Bevoelkerungszahl_100m-Gitter.csv")

zensus_cleaned <- zensus_csv %>%
  mutate(
    E_origin = x_mp_100m - 50,
    N_origin = y_mp_100m - 50,
    E_code = E_origin / 100,
    N_code = N_origin / 100,
    gitterid = sprintf(
      "100mN%dE%d",
      as.integer(N_code),
      as.integer(E_code)
    )
  ) %>%
  select(gitterid, Einwohner)

zensus_grid <- left_join(laea_grid, zensus_cleaned, by = join_by("id" == "gitterid"))
zensus_grid_populated <- zensus_grid %>%
  filter(Einwohner > 0)

st_write(zensus_grid, "geodata/grids.gpkg", "zensus_grid_100m_regbez", append = FALSE)
st_write(zensus_grid_populated, "geodata/zensus.gpkg", "regbez_zensus_populated", append = FALSE)
