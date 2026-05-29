library(tidyverse)
library(lubridate)
library(zoo)
#This function checks if a feed contains agencies whose trips don't extend to the end of the feed.

check_gtfs_discont <- function(gtfs_feed, dates, trip_calendar){
  activity_per_day <- trip_calendar %>%
    group_by(date, agency_id) %>%
    summarise(
      trips = n()
    ) %>%
    left_join(gtfs_feed$agency %>% dplyr::select(agency_id, agency_name)) %>%
    filter(date %in% dates)
  
  feed_end <- max(activity_per_day$date)
  
  agency_coverage <- activity_per_day %>%
    group_by(agency_id, agency_name) %>%
    summarise(
      last_date = max(date),
      active_days = n(),
      average_trips = mean(trips),
      .groups = "drop"
    ) %>%
    mutate(
      ends_early = last_date < feed_end
    )
  
  dropouts <- agency_coverage %>%
    filter(ends_early) %>%
    arrange(last_date)
  
  if(nrow(dropouts) > 0) {
    
    warning_text <- paste0(
      nrow(dropouts),
      " agencies do not extend to the last date of the supplied date vector (",
      feed_end,
      ").\n\n",
      paste0(
        "- ",
        dropouts$agency_name,
        ": ",
        dropouts$last_date,
        collapse = "\n"
      )
    )
    
    warning(warning_text)
    return(dropouts)
  }
}