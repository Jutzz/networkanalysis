library(here)
library(dplyr)
library(sf)

files <- list.files(
  path = "geodata/base_data/zensus_auxvalues/",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# 2. keep only files with "100m" in name
files_100m <- files[str_detect(files, "100m")]

# helper function to read + clean one file
read_clean <- function(file) {
  df <- read_csv2(file, progress = TRUE)
  
  # remove unwanted columns if present
  df <- df %>%
    dplyr::select(-any_of(c("x_mp_100m", "y_mp_100m", "werterlaeuternde_Zeichen")))
  
  # keep only ID + ONE value column
  # (assumes exactly one remaining non-ID column)
  value_cols <- setdiff(names(df), "GITTER_ID_100m")
  
  if (length(value_cols) != 1) {
    warning(paste("File has unexpected columns:", file))
  }
  
  df %>%
    dplyr::select(GITTER_ID_100m, all_of(value_cols))
}

# 3. read all auxiliary tables
aux_list <- map(files_100m, read_clean)

zensus_final <- reduce(aux_list, left_join, by = "GITTER_ID_100m", .init = zensus)

zensus_cleaned <- zensus_final %>%
  mutate(across(-c("GITTER_ID_100m", "geom"), ~ gsub("–", 0, .))) %>%
  mutate(across(-c("GITTER_ID_100m", "geom"), as.numeric))

st_write(zensus_cleaned, "indikatoren/geodata/zensus.gpkg", "zensus2022_auxvalues", append = FALSE)

