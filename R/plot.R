#' Weather Forecast
#'
#' @description
#' Plot 10 day hourly weather forecasts for given locations.
#'
#' @inheritParams fetch_forecast
#' @inheritParams forecast data
#'
#' @param format A string giving the formatting specification for the date labels.
#'   Codes are defined in [strftime()].
#'   Default "%a %d.%m" (weekday name, day of the month and month).
#' @param ... Other arguments passed to [ggplot2::labs()].
#'
#' @inherit forecast details
#'
#' @returns A ggplot2 object with [variables()] vs. time plotted on panels and
#' locations on colored lines.
#'
#' @seealso [fetch_forecast()], [forecast()]
#'
#' @examples
#' plot_forecast(64, 24,
#'               title = "Pikkupirtti",
#'               subtitle = as.character(Sys.time(), digits = 0)
#'               )
#'
#' myfc <- fetch_forecast(60.2665, 24.896)
#' plot_forecast(data = myfc, title = "Kuninkaantammi")
#'
#' plot_forecast(
#'   latitude = c(69.043, 69.02),
#'   longitude = c(20.857, 20.79),
#'   location = c(1025, 472),
#'   title = "Weather forecasts for Saanatunturi (elevation 1025 m) and Kilpisjärvi (472 m)"
#'   )
#'
#' @export
plot_forecast <- function(latitude = NULL, longitude = NULL, location = NULL, data = NULL,
                          format = "%a %d.%m", ...){

  fc <- forecast(latitude = latitude,
                 longitude = longitude,
                 location = location,
                 data = data,
                 p_column = "rain.p")

  # use the probability given in data if available
  if(!is.null(data) && !is.null(attr(data, "p_column"))) {
    fc[, rain := 100 * get(attr(data, "p_column"))]
  } else {
    fc[, rain := 100 * rain.p]
  }

  fc[, Pf := Pf + 1000]

  weather_levels <- c("Tf", "rain", "Cf", "Wf", "WDf", "Pf", "Hf")

  # reshape forecast to long
  fc.long <- melt(fc,
              id.vars = c("location", "time"),
              measure.vars = weather_levels
  )


  fc.long[, variable := ordered(
    variable,
    levels = weather_levels
  )]

  fc.long[, group := 0]


  scale_y_100 <- function(...) {
    scale_y_continuous(limits = c(0, 100),
                       breaks = 0:2*50, minor_breaks = 0:10*10, ...)
  }

  # unwrap wind direction so that it doesn't jump anywhere
  unwrap <- function(x) {
    dd <- diff(x)
    dd <- (dd + 180) %% 360 - 180

    d <- c(x[1], x[1] + cumsum(dd))
    d <- d - 360 * floor((min(d) + max(d) - 360) / 720)

    s <- mean(d) %/% 360
    d <- d - s*360
  }

  fc.long[variable == "WDf", value := unwrap(value), location]

  # wrap wind direction back at 0 and 360 if outside limits
  lim.WD <- c(-10,370)


  wd <- fc.long[variable == "WDf"]

  if(!all(range(wd$value) %between% lim.WD)) {

    # Insert group breaks to times where direction crosses 0 or 360 degrees.

    gr <- function(x) {
      br <- x %/% 360
      b <- br != shift(br)
      b[1] <- TRUE

      cumsum(b)
    }

    setorder(wd, location, time)

    add_breaks <- function(loc) {
      dt <- wd[location == loc] [, `:=`(group = gr(value), angle = value %% 360)]

      interpolate <- function(i){
        d1 <- dt[c(i-1, i)]
        y <- stats::approx((d1$value + 180) %% 360, d1$time, 180)$y |> as.POSIXct(lubridate::tz(d1$time))
        d1[, `:=`(time = y, angle = round((value %% 360) / 360) * 360)]
      }

      b <- dt[, which(group != shift(group))]
      dt1 <- rbind(
        dt,
        rbindlist(lapply(b, interpolate))
      )

      setorder(dt1, group, time)
      dt1[, `:=`(value = angle, angle = NULL)]
    }

    wd1 <- lapply(unique(wd$location), add_breaks) |> rbindlist()

    fc.long <- rbind(
      fc.long[variable != "WDf"],
      wd1
    )
  }

  setorder(fc.long, variable, location, group, time)

  add_caption <- function(p, ...) {
    labs <- rlang::list2(...)
    caption <- "Based on data from MET Norway and precipitation model by the author"

    if (!is.null(labs$caption)) {
      caption <- paste(caption, labs$caption, sep = "\n")
    }

    labs$caption <- caption
    p + do.call(ggplot2::labs, labs)
  }

  weather_labels <- c(
    Tf   = "Temperature, \u00b0C",
    rain = "Precipitation Probability, %",
    Cf   = "Cloud Fraction, %",
    Wf   = "Wind Speed, m/s",
    WDf  = "Wind Direction, degrees",
    Pf   = "Air Pressure, hPa",
    Hf   = "Relative Humidity, %"
  )

  time_unit <- "6 hours"
  time_limits <- range(fc.long$time) |> lubridate::floor_date(time_unit)
  time_limits[2] <- time_limits[2] + lubridate::period(time_unit)

  dayrange <- lubridate::floor_date(time_limits, unit = "day") + lubridate::days(c(0,1))

  xmajor <- lubridate::floor_date(seq.POSIXt(dayrange[1], dayrange[2] +
    lubridate::days(1), by = "day"), unit = "day") |>
    unique()

  xminor <- seq.POSIXt(dayrange[1], dayrange[2], by = time_unit) |>
    lubridate::floor_date(unit = time_unit) |>
    unique()

  make_ticks <- function(x, by, base = 0){
    lower <- floor((min(x) - base) / by)
    upper <- ceiling((max(x) - base) / by) + 1e-9 / by

    seq(lower, upper) * by + base
  }

  if(length(fc.long$location |> unique()) > 1){
    p <- ggplot(fc.long, aes(time, value, colour = location))
  } else {
    p <- ggplot(fc.long, aes(time, value))
  }

  # n_locations <- length(fc.long$location |> unique())
  # p <- ggplot(fc.long, aes(time, value)) +
  #   theme(legend.position = if (n_locations == 1L) "none" else "right")

  p <- p +
    scale_y_continuous() +
    scale_x_datetime(limits = time_limits,
                     breaks = xmajor,
                     minor_breaks = xminor,
                     date_labels = format,
                     expand = 0)

  lim <- function(x, lo, hi) fc.long[variable == x, range(c(value, lo, hi))]

  lim.T <- lim("Tf", -20, 30)
  lim.P <- lim("Pf", 940, 1060)
  lim.W <- lim("Wf", 0, 20)

  p <-  p +
    geom_point(shape = 3, size = 0.8, color = "grey") +
    geom_line(aes(group = paste(location, group))) +

    facet_wrap(
      ~ variable,
      ncol = 1,
      scales = "free_y",
      labeller = as_labeller(weather_labels)
    ) +

    ggh4x::facetted_pos_scales(
      y = list(
        variable == "Tf" ~ scale_y_continuous(
          limits = lim.T,
          breaks = make_ticks(lim.T, 20),
          minor_breaks = make_ticks(lim.T, 5)
          ),
        variable == "rain" ~ scale_y_100(),
        variable == "Cf" ~ scale_y_100(),
        variable == "Wf" ~ scale_y_continuous(
          limits = lim.W,
          breaks = make_ticks(lim.W, 10),
          minor_breaks = make_ticks(lim.W, 2)
        ),
        variable == "WDf" ~ scale_y_continuous(
          limits = lim.WD,
          breaks = seq(0, 360, 90),
          minor_breaks = seq(0, 360, 45)
          ),
        variable == "Pf" ~ scale_y_continuous(
          limits = lim.P,
          breaks = make_ticks(lim.P, 50, 1000),
          minor_breaks = make_ticks(lim.P, 10, 1000)
          ),
        variable == "Hf" ~ scale_y_100()
      )
    )

  compass_point <- data.table(
    variable = factor("WDf", levels = weather_levels, ordered = TRUE),
    time = time_limits[1] + lubridate::hours(1),
    value = c(0, 90, 180, 270, 360),

    # label = c("N\u2193", "E\u2190", "S\u2191", "W\u2192", "N\u2193")
    label = c("N v", "E <", "S ^", "W >", "N v")
  )

  theme <- ggplot2::theme_get() + p$theme
  axis_text <- ggplot2::calc_element("axis.text.y", theme)

  p <- p + geom_text(
    data = compass_point,
    aes(x = time, y = value, label = label),
    inherit.aes = FALSE,
    family = axis_text$family,
    colour = axis_text$colour,
    fontface = axis_text$face,
    size = axis_text$size * 25.4 / 72,
    hjust = 0,
    nudge_x = 0
  )

  p <- add_caption(p, ...)

  p
}

