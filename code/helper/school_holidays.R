library(dplyr)
library(purrr)
library(tidyr)
library(httr2)
library(jsonlite)

get_school_holidays <- function(country, subdivision, start_date, end_date, lang = "DE") {
  resp <- httr2::request("https://openholidaysapi.org/SchoolHolidays") |>
    httr2::req_url_query(
      countryIsoCode = country,
      validFrom = start_date,
      validTo = end_date,
      languageIsoCode = lang,
      subdivisionCode = subdivision
    ) |>
    httr2::req_perform()
  
  school_holidays <- fromJSON(rawToChar(resp$body))
}

prepare_holidays <- function(df) {
  df %>%
    select(2, 3, 5, 9) %>%
    transmute(
      name = map_chr(name, \(x) {
        x$text[x$language == "DE"][1]
      }),
      subdivision = map_chr(subdivisions, \(x) {
        x$shortName[1]
      }),
      startDate = as.Date(startDate),
      endDate   = as.Date(endDate)
    )
}

expand_holidays <- function(df) {
  df %>%
    mutate(
      date = map2(startDate, endDate, ~ seq(.x, .y, by = "day"))
    ) %>%
    unnest(date) %>%
    select(name, subdivision, date)
}

filter_holidays_for_plot <- function(df, x_min, x_max) {
  df %>%
    filter(
      endDate >= x_min,
      startDate <= x_max
    ) %>%
    mutate(
      startDate = pmax(startDate, x_min),
      endDate   = pmin(endDate, x_max)
    ) %>%
    mutate(
      startDate = pmax(startDate, x_min),
      endDate   = pmin(endDate, x_max) + 1
    )
  
}