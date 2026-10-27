#' Transformations
#'
#' @description
#' Compress values greater than 1 and amplify values less than 1.
#'
#' @param x A numeric vector. Any x shall be greater of equal to 0.
#' @param shrink A numeric < 1.
#'
#' @return A numeric vector.
#'
#' @note shrink values > 1 are accepted, but then x values above 1 are amplified.
#'
#' @seealso [amplify()], [rain_shrink()]
#'
#' @examples
#' compress(c(0, 0.1, 1, 10), rain_shrink())
#'
#' @export
compress <- function(x, shrink) {
  if(shrink != 0) x ^ shrink else log(x)
}

#' Transformations
#'
#' @description
#' Amplify values greater than 1 and compress values less than 1.
#'
#' @param x A numeric vector. Any x shall be greater of equal to 0.
#' @param shrink A numeric < 1.
#'
#' @return A numeric vector.
#'
#' @note shrink values > 1 are accepted, but then x values above 1 are compressed.
#'
#' @seealso [compress()], [rain_shrink()]
#'
#' @examples
#' amplify(compress(0.5, rain_shrink()), rain_shrink())
#'
#' @export
amplify <- function(x, shrink) {
  if(shrink != 0)
    x ^ (1/shrink) else exp(x)
}

#' Transformations
#'
#' @description
#' The shrink parameter used to compress
#' forecast precipitation amounts for
#' rain probability prediction.
#'
#' @return A numeric.
#'
#' @seealso [amplify()], [compress()]
#'
#' @examples
#' print (rain_shrink())
#'
#' @export
rain_shrink <- function() {
  .rain_shrink
}

#' Preprocess Data
#'
#' @description
#' Prepare raw forecast data to be used in precipitation probability predictions.
#'
#' @param data A data.table as from [fetch_forecast()]
#'
#' @details
#' The columns in the [fetch_forecast()] output will be renamed to their abbreviations
#' as defined in the [variables()] table.
#'
#' @returns A data.table
#'
#' @export
preprocess_rain_v1 <- function(data){
  dt0 <- copy(data)[, advance := as.numeric(time - updated_at, unit = "hours")]

  if(any(dt0$advance <= 0)) {
    stop("`time` must be greater than `updated at`.")
  }

  setnames(dt0, .variables$variable, .variables$abbreviation, skip_absent = TRUE)

  dt <- dt0[
    , list(time,
        lat, lon,
        R01f.c = compress(R01f, .rain_shrink),
        R06f.c = compress(R06f, .rain_shrink),
        R01f.m = factor(1 * is.na(R01f)),
        R06f.m = factor(1 * is.na(R06f)),
        advance, Cf,
        Pf = Pf - 1000,
        Tf, Hf, WDf,Wf,
        S01f, S06f, S12f
    )]

  dt[, `:=`(R01f.c = fifelse(R01f.m == 1, 0, R01f.c),
            R06f.c = fifelse(R06f.m == 1, 0, R06f.c)
  )]

  # copy extra columns not present in .variables
  add_cols <- setdiff(names(data), .variables$variable)
  dt <- cbind(dt, data[, .SD, .SDcols = add_cols])

  dt
}

#' Model Predictions
#'
#' @description
#' Get essential elements of a GLM object needed for predictions.
#' Data used for fitting is left out from the compact object
#' thus saving space and making transfer of large objects faster.
#'
#' @param object a fitted object of class inheriting from "glm".
#'
#' @returns A list with formula, coefficients, terms, xlevels, contrasts, family and offset from the model.
#'
#' @seealso [predict_compact_glm()]
#'
#' @export
compact_glm <- function(object) {
  stopifnot(inherits(object, "glm"))

  list(
    formula = stats::formula(object),
    coefficients = stats::coef(object),
    terms = stats::delete.response(stats::terms(object)),
    xlevels = object$xlevels,
    contrasts = object$contrasts,
    family = object$family,
    offset = attr(stats::terms(object), "offset")
  )
}


#' Predict Compact GLM Fits
#'
#' @description
#' Obtains predictions from a fitted compact generalized linear model object.
#'
#' @param object A list of elements of a fitted GLM object needed for predictions. See [compact_glm()].
#' @param data A data.frame in which to look for variables with which to predict.
#' @param type A character. The type of prediction required.
#' The default type = "response" gives the predictions on the scale of the response variable;
#' the alternative type = "link" is on the scale of the linear predictors.
#' For a binomial model the "response" gives the predicted probabilities and
#' the "link" predicted probabilities are on logit scale.
#'
#' @returns A numeric vector of predictions.
#'
#' @seealso [predict.glm()]
#'
#' @export
predict_compact_glm <- function(object, data,
                                type = c("response", "link")) {
  type <- match.arg(type)

  X <- stats::model.matrix(
    object$terms,
    data = data,
    contrasts.arg = object$contrasts,
    xlev = object$xlevels
  )

  beta <- object$coefficients

  # Align columns with the fitted coefficients
  missing <- setdiff(names(beta), colnames(X))
  if (length(missing))
    stop("Missing model-matrix columns: ",
         paste(missing, collapse = ", "))

  X <- X[, names(beta), drop = FALSE]

  eta <- drop(X %*% beta)

  if (type == "link")
    return(eta)

  if (object$family$link == "logit")
    return(stats::plogis(eta))

  object$family$linkinv(eta)
}

#' Weather Forecast
#'
#' @description
#' Pre-process weather variables and supplement
#' forecast by predicted hourly precipitation probability.
#'
#' @inheritParams fetch_forecast
#'
#' @param data optional data.table, see details. Output of [forecast()] or [fetch_forecast()].
#' @param p_column A character name for precipitation probability column.
#' If NULL the probability is not computed. Default 'rain.p'.
#'
#' @details
#' Either coordinates or `data` must be supplied.
#' Coordinates may be vectors of equal length.
#' If both coordinates and `data` are supplied, `data` is ignored.
#' [fetch_forecast()] is used to retrieve forecasts when coordinates
#' are supplied.
#'
#' @returns A data.table. See [variables()].
#' The hourly precipitation probability is predicted using [precipitation_pb_summary()].
#'
#' @seealso [fetch_forecast()], [plot_forecast()]
#'
#' @examples
#' forecast(64, 24) # Pikkupirtti.
#' forecast(data = fetch_forecast(60.2665, 24.896)) # Kuninkaantammi.
#'
#' @export
forecast <- function(latitude = NULL, longitude = NULL, location = NULL, data = NULL, p_column = "rain.p") {

  if (!is.null(latitude) || !is.null(longitude)) {
    data <- fetch_forecast(latitude = latitude, longitude = longitude, location = location)
  } else if (is.null(data)) {
    stop("Supply coordinates or data.")
  }

  x <- (stats::formula(.rain) |> all.vars())[-1]
  if(all(x %in% names(data))){
    dt <- copy(data)
  } else {
    dt <- preprocess_rain_v1(data)
  }

  # compute probability only when p_column is missing and not NULL
  if(!is.null(p_column)){
    if(!p_column %in% names(dt)){
      dt[, .p. := predict_compact_glm(.rain, data = dt, type = "response")]
      setnames(dt, ".p.", p_column)
    }

    setattr(dt, "p_column", p_column)
  }

  dt[]
}

#' Weather Forecast
#'
#' @description
#' Predict hourly precipitation probability.
#'
#' @param data A data.table, output of [forecast()] or [fetch_forecast()].
#' @inheritParams predict_compact_glm type
#'
#' @returns A numeric vector of precipitation probabilities.
#'
#' @examples
#' myfc <- fetch_forecast(latitude = 60.2665, longitude = 24.896, location = "Kuninkaantammi")
#' precipitation_pb(myfc, type = "link")
#'
#' @export
precipitation_pb <- function(data,
                             type = c("response", "link")) {

  x <- (stats::formula(.rain) |> all.vars())[-1]
  if(all(x %in% names(data))){
    return(predict_compact_glm(.rain, data = data, type = type))
  }

  dt <- preprocess_rain_v1(data)
  found <- x %in% names(dt)
  if(all(found)) {
    return(predict_compact_glm(.rain, data = dt, type = type))
  }
}

#' Precipitation Probability
#'
#' @description
#' Show summary of precipitation probability model and training data.
#'
#' @returns None (invisible NULL).
#'
#' @seealso [stations()]
#'
#' @examples
#' precipitation_pb_summary()
#'
#' @export
precipitation_pb_summary <- function(){
  i <- sapply(all.vars(stats::terms(.rain)), function(x) {
    i <- sapply(.variables$abbreviation, grepl, x = x) |> which()
    ifelse(length(i), i, NA)
  })
  p <- .variables[i, variable] |> paste(collapse = ", ")
  x <- paste("Predictors:", p)

  cat(
    paste("Precipitation model version:", .rain_version),
    paste("Response: precipitation >", .threshold, "mm/hour"),

    paste(strwrap(x, width = 50), collapse = "\n\t"),

    paste("Begin data gathering:", lubridate::date(.data_summary$start)),
    paste("End data gathering:", lubridate::date(.data_summary$stop)),
    paste("Number of weather stations:", .data_summary$stations),
    paste("Training data size:", .data_summary$rows, "rows"),

    sep = "\n"
  )
}

#' Precipitation Probability
#'
#' @description
#' Show Finnish weather stations from which the training, testing and proofing observations are gathered.
#'
#' @returns A data.table with station fmisid, name and lat, lon coordinates.
#'
#' @seealso [precipitation_pb_summary()]
#'
#' @examples
#' stations()
#'
#' @export
stations <- function(){
  return(.stations)
}
