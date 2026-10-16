test_that("rain probabilities match the reference predictions", {
  rain_test <- getFromNamespace(".rain_test", "snail")
  rain_model <- getFromNamespace(".rain", "snail")

  predictors <- setdiff(
    names(rain_test),
    "p_expected"
  )

  p <- snail::predict_compact_glm(rain_model, rain_test[, ..predictors], type = "response")

  expect_type(p, "double")
  expect_length(p, nrow(rain_test))
  expect_true(all(is.finite(p)))
  expect_true(all(p >= 0 & p <= 1))

  expect_equal(
    unname(p),
    rain_test$p_expected,
    tolerance = 1e-8
  )
})
