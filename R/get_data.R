#' Get data
#'
#' @description
#' Fetch weather forecast for next 10 days from the
#' Norwegian Meteorological Institute (met)
#' using "https://api.met.no/weatherapi/".
#'
#' @param latitude A numeric latitude in degrees
#' @param longitude A numeric longitude in degrees
#'
#' @returns A data.table of met forecasts. See [variables()]
#'
#' @examples
#' fetch_forecast(64, 24) # A forecast for Pikkupirtti.
#'
#' @export
fetch_forecast <- function(latitude, longitude) {
  url <- sprintf(
    paste0(
      "https://api.met.no/weatherapi/",
      "locationforecast/2.0/compact",
      "?lat=%f&lon=%f"
    ),
    latitude,
    longitude
  )

  data <-
    httr2::request(url) |>
    httr2::req_user_agent("snail (lassi.myllykoski@gmail.com)") |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)


  longitude <- data$geometry$coordinates[[1]]
  latitude <- data$geometry$coordinates[[2]]
  altitude <- data$geometry$coordinates[[3]]

  updated_at <- data$properties$meta$updated_at |>
    lubridate::ymd_hms(tz = Sys.timezone()) |>
    suppressMessages()

  ts <- data$properties$timeseries
  time <- vapply(ts, function(x) x$time, FUN.VALUE = character(1)) |>
    lubridate::ymd_hms(tz = Sys.timezone()) |>
    suppressMessages()

  # grasp general weather properties
  property <- function(name){
    vapply(ts, function(x) {x$data$instant$details[[name]]}, FUN.VALUE = numeric(1))
  }

  properties <- names(ts[[1]]$data$instant$details)


  # grasp weather code
  symbol_code <- function(z){
    vapply(ts, function(x) {
      y <- x$data[[z]]$summary$symbol_code
      if(is.null(y) || !length(y)) return(as.character(NA))
      return(y[1])
    }, FUN.VALUE = character(1))
  }

  symbol_code_01 <- symbol_code("next_1_hours")
  symbol_code_06 <- symbol_code("next_6_hours")
  symbol_code_12 <- symbol_code("next_12_hours")


  # grasp precipitation
  precipitation_amount <- function(z){
    vapply(ts, function(x) {
      y <- x$data[[z]]$details$precipitation_amount
      if(is.null(y) || !length(y)) return(NA)
      return(y[1])
    }, FUN.VALUE = numeric(1))
  }

  precipitation_01 <- precipitation_amount("next_1_hours")
  precipitation_06 <- precipitation_amount("next_6_hours")
  # precipitation_12 <- precipitation_amount("next_12_hours")


  # collect all into a data.table
  dt <- as.data.table(lapply(properties, property)) |> setnames(properties)
  cbind(updated_at, latitude, longitude, altitude, time, dt,
        symbol_code_01, symbol_code_06, symbol_code_12,
        precipitation_01, precipitation_06)[time > updated_at]
}

#' Variables
#'
#' @description
#' Description of variables used
#'
#' @returns A data.table of variable names, labels abbreviations and units.
#'
#' @examples
#' variables()
#'
#' @export

variables <- function(){
  .variables
}

