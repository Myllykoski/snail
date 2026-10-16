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
#' @returns A data.table
#'
#' @export
preprocess_rain_v1 <- function(data){
  dt0 <- copy(data)[, advance := as.numeric(time - updated_at, unit = "hours")]

  setnames(dt0, .variables$variable, .variables$abbreviation)

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

  dt[]
}

#' Model Predictions
#'
#' @description
#' Get essential elements of a GLM object needed for predictions. See [predict_compact_glm()].
#' Data used for fitting is left out from the compact object
#' thus saving space and making transfer of large objects faster.
#'
#' @param object a fitted object of class inheriting from "glm".
#'
#' @returns A list with formula, coefficients, terms, xlevels, contrasts, family and offset from the model.
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
#' @param newdata A data.frame in which to look for variables with which to predict.
#' @param type A character the type of prediction required.
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
predict_compact_glm <- function(object, newdata,
                                type = c("response", "link")) {
  type <- match.arg(type)

  X <- stats::model.matrix(
    object$terms,
    data = newdata,
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
#' Pre-process weather variables and predict hourly rain probability.
#'
#' @param latitude A numeric latitude in degrees.
#' @param longitude A numeric longitude in degrees.
#' @param data A data.table of met forecasts.
#' @param name A character name for rain probability column. Default 'rain.p'
#'
#' @returns A data.table. See [variables()]
#'
#' @details
#' Either coordinates or data shall be given.
#' The data is ignored if both coordinates are given.
#' [fetch_forecast()] is used to get data when missing.
#'
#' @examples
#' forecast(64, 24) # Pikkupirtti.
#' forecast(data = fetch_forecast(60.2665, 24.896)) # Kuninkaantammi.
#'
#' @export
forecast <- function(latitude = NULL, longitude = NULL, data = NULL, name = "rain.p") {
  if(!(is.null(latitude) || is.null(longitude))) {
    data <- fetch_forecast(latitude, longitude)
  } else if (is.null(data)) {
    stop("Coordinates and data are missing.")
  }

  x <- (stats::formula(.rain) |> all.vars())[-1]
  if(all(x %in% names(dt))){
    dt <- copy(data)
  } else {
    dt <- preprocess_rain_v1(data)
  }

  if(!name %in% names(dt)){
    dt[, .p. := predict_compact_glm(.rain, newdata = dt, type = "response")]
    setnames(dt, ".p.", name)
  }

  dt[]
}

#' Rain Probability
#'
#' @description
#' Show rain probability model version and training data details.
#'
#' @returns A data.table
#'
#' @examples
#' rain_version()
#'
#' @export
rain_version <- function(){
  cbind(`rain model version` = .rain_version,
        .data_summary)
}
