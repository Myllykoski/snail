#' Weather Forecast
#'
#' @description
#' Plot weather forecast for next 10 days.
#'
#' @param latitude A numeric latitude in degrees.
#' @param longitude A numeric longitude in degrees.
#' @param data A data.table of forecasts. The output of [forecast()] or [fetch_forecast()].
#' @param ... other arguments passed on to [labs()] method.
#'
#' @returns A ggplot2 object.
#'
#' @details
#' Either coordinates or data shall be given.
#' The data is ignored if both coordinates are given.
#' [fetch_forecast()] is used to get data when missing.
#'
#' @examples
#' plot_forecast(64, 24, title = "Pikkupirtti", subtitle = Sys.time())
#' plot_forecast(data = fetch_forecast(60.2665, 24.896), title = "Kuninkaantammi")
#'
#' @export
plot_forecast <- function(latitude = NULL, longitude = NULL, data = NULL, ...) {
  dt <- forecast(latitude = latitude, longitude = longitude, data = data, name = "rain.p")

  dt[, rain := 100 * rain.p]
  dt[, Pf := Pf + 1000]

  dt1 <- melt(dt,
              id.vars = "time",
              measure.vars = c("rain", "Hf", "Pf", "Tf", "Cf", "Wf", "WDf")
  )

  dt1[, variable := factor(
    variable,
    levels = c("Tf", "rain", "Cf", "Wf", "WDf", "Pf", "Hf")
  )]

  weather_labels <- c(
    Tf   = "Temperature \u00b0C",
    rain = "Rain Probability %",
    Cf   = "Cloud Fraction %",
    Wf   = "Wind Speed m/s",
    WDf  = "Wind Direction degrees",
    Pf   = "Air Pressure hPa - 1000",
    Hf   = "Relative Humidity %"
  )

  scale_y_100 <- function(...) {
    scale_y_continuous(limits = c(0, 100),
                       breaks = 0:2*50, minor_breaks = 0:10*10, ...)
  }

  unwrap <- function(x) {
    dd <- diff(x)
    dd <- (dd + 180) %% 360 - 180

    d <- c(x[1], x[1] + cumsum(dd))
    d <- d - 360 * floor((min(d) + max(d) - 360) / 720)

    s <- mean(d) %/% 360
    d <- d - s*360
  }

  dt1[variable == "WDf", value := unwrap(value)]
  wind_limits <- function() {
    r <- dt1[variable == "WDf", range(value)]
    c(min(r[1], 0), max(r[2], 360))
  }

  add_caption <- function(p, ...) {
    labs <- rlang::list2(...)
    caption <- "Based on data from MET Norway"

    if (!is.null(labs$caption)) {
      caption <- paste(caption, labs$caption, sep = "\n")
    }

    labs$caption <- caption
    p + do.call(ggplot2::labs, labs)
  }

  p <- ggplot(dt1, aes(time, value)) +
    geom_line() +
    scale_x_datetime(date_breaks = "day", date_minor_breaks = "6 hours", date_labels = "%a %d",
                     limits = lubridate::floor_date(range(dt1$time), unit = "6 hours") + lubridate::hours(c(0,6)),
                     expand = 0) +
    scale_y_continuous()

  p <- add_caption(p, ...)

  p +
    facet_wrap(
      ~ variable,
      ncol = 1,
      scales = "free_y",
      labeller = as_labeller(weather_labels)
    ) +
    ggh4x::facetted_pos_scales(
      y = list(
        variable == "Tf" ~ scale_y_continuous(limits = c(-20,30),
                                              breaks = seq(-40,40,10), minor_breaks = seq(-40,40,5)),
        variable == "rain" ~ scale_y_100(),
        variable == "Cf" ~ scale_y_100(),
        variable == "Wf" ~ scale_y_continuous(limits = c(0, 30),
                                              breaks = 0:4*10, minor_breaks = 0:8*5),
        variable == "WDf" ~ scale_y_continuous(limits = wind_limits(),
                                               breaks = -3:7*90),
        variable == "Pf" ~ scale_y_continuous(limits = c(940, 1050),
                                              breaks = seq(900, 1100, 50)),
        variable == "Hf" ~ scale_y_100()
      )
    )
}
