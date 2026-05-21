java_to_dt <- function(obj) {
  
  # check input
  if(class(obj)[1] != "jobjRef"){
    stop("Input must be an object of class 'jobjRef'")}
  
  # get column names from Java table
  columns <- obj$getColumnNames()
  
  # get the contents of each column in a vector, and return them in a list
  dt <- lapply(columns, function(column_name) {
    # check column data type, so we can call the appropriate Java function
    column_type <- obj$getColumnType(column_name)
    
    if (column_type == "String") { v <- obj$getStringColumn(column_name) }
    if (column_type == "Integer") { v <- obj$getIntegerColumn(column_name) }
    if (column_type == "Long") { v <- obj$getLongColumn(column_name) }
    if (column_type == "Double") { v <- obj$getDoubleColumn(column_name) }
    if (column_type == "Boolean") { v <- obj$getBooleanColumn(column_name) }
    return(v)
  })
  
  # convert list of vectors to a data.table, and rename columns accordingly
  data.table::setDT(dt)
  data.table::setnames(dt, new = columns)
}

#' data.table to speedMap
#'
#' @description Converts a `data.frame` with road OSM id's and respective speeds
#'              to a Java Map<Long, Float> for use by r5r_network.
#'
#' @param dt data.frame/data.table. Table specifying the
#'        speed modifications. The table must contain columns \code{osm_id} and
#'        \code{max_speed}.
#' @return A speedMap (Java HashMap<Long, Float>)
#' @family java support functions
#' @keywords internal

check_transit_availability <- function(r5r_network,
                                       dates = NULL,
                                       start_date = NULL,
                                       end_date = NULL
) {
  # Check inputs
  checkmate::assert_class(r5r_network, "r5r_network")
  jcore <- r5r_network@jcore
  
  # Argument validation for date inputs (CONSOLIDATED)
  is_valid_dates_vector <- !is.null(dates) &&
    is.null(start_date) &&
    is.null(end_date)
  is_valid_date_range <- is.null(dates) &&
    !is.null(start_date) &&
    !is.null(end_date)
  
  if (!is_valid_dates_vector && !is_valid_date_range) {
    cli::cli_abort(
      c(
        "Incorrect date arguments provided.",
        "i" = "Please specify dates using one of the following methods:",
        "*" = "Use the {.arg dates} argument to provide a vector of specific dates.",
        "*" = "Use both {.arg start_date} and {.arg end_date} to provide a continuous date range.",
        "x" = "You cannot mix these methods or provide an incomplete date range."
      )
    )
  }
  
  # Helper function to parse and validate date inputs
  parse_date_input <- function(date_input, arg_name) {
    # Pass Date objects through directly
    if (inherits(date_input, "Date")) {
      return(date_input)
    }
    
    if (!is.character(date_input)) {
      cli::cli_abort(
        "{.arg {arg_name}} must be a vector of character strings or Date objects."
      )
    }
    
    # Use regex to strictly check for "YYYY-MM-DD" format
    is_iso_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", date_input)
    if (any(!is_iso_format)) {
      cli::cli_abort(c(
        "x" = "Invalid date format found in {.arg {arg_name}}.",
        "i" = "Please use the strict {.val 'YYYY-MM-DD'} format for all date strings."
      ))
    }
    
    # Use a tryCatch block to convert potential errors from as.Date() into NAs
    parsed_dates_list <- lapply(date_input, function(d) {
      tryCatch(
        {
          as.Date(d)
        },
        error = function(e) {
          # If as.Date fails, return NA instead of throwing an error
          as.Date(NA)
        }
      )
    })
    parsed_dates <- do.call("c", parsed_dates_list)
    
    # Final check for NAs, which now correctly indicate logically impossible dates
    if (anyNA(parsed_dates)) {
      cli::cli_abort(c(
        "x" = "Could not parse all values in {.arg {arg_name}}.",
        "i" = "One or more dates are logically invalid (e.g., '2025-02-29')."
      ))
    }
    
    return(parsed_dates)
  }
  
  # Prepare the list of dates to check
  if (!is.null(dates)) {
    dates_as_date <- parse_date_input(dates, "dates")
    dates_formatted <- format(dates_as_date, "%Y-%m-%d")
  } else {
    start_d <- parse_date_input(start_date, "start_date")
    end_d <- parse_date_input(end_date, "end_date")
    
    if (length(start_d) > 1 || length(end_d) > 1) {
      cli::cli_abort(
        "{.arg start_date} and {.arg end_date} must each be a single date."
      )
    }
    if (start_d > end_d) {
      cli::cli_abort(
        "{.arg start_date} must be before or the same as {.arg end_date}."
      )
    }
    
    date_sequence <- seq(from = start_d, to = end_d, by = "day")
    dates_formatted <- format(date_sequence, "%Y-%m-%d")
  }
  
  # Function to process a single date by querying the Java object
  process_single_date <- function(date_str) {
    services_java <- jcore$getTransitServicesByDate(date_str)
    services_dt <- java_to_dt(services_java)
    
    if (nrow(services_dt) == 0) {
      return(data.table::data.table(
        date = as.Date(date_str),
        total_services = 0L,
        active_services = 0L,
        pct_active = 0.0
      ))
    }
    
    total_s <- nrow(services_dt)
    active_s <- sum(services_dt$active_on_date, na.rm = TRUE)
    pct_s <- if (total_s > 0) active_s / total_s else 0.0
    
    return(data.table::data.table(
      date = as.Date(date_str),
      total_services = total_s,
      active_services = active_s,
      pct_active = pct_s
    ))
  }
  
  # Apply function to all dates and bind results into a single data.table
  results_list <- lapply(dates_formatted, process_single_date)
  final_dt <- data.table::rbindlist(results_list)
  
  return(final_dt)
}