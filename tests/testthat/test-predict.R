test_that("predict returns a complete predictive distribution", {

  fit <- bruno(
    event = c(1, 0, 1, 1, 0),
    mu0 = 0.50,
    tau = 10,
    future_n = 6
  )

  prediction <- predict(fit)

  expect_s3_class(prediction, "data.frame")

  expect_named(
    prediction,
    c("t", "probability")
  )

  expect_equal(
    prediction$t,
    0:6
  )

  expect_equal(
    sum(prediction$probability),
    1,
    tolerance = 1e-12
  )
})


test_that("predict uses fitted future_n by default", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 7
  )

  prediction <- predict(fit)

  expect_equal(
    max(prediction$t),
    7
  )

  expect_equal(
    attr(prediction, "future_n"),
    7
  )
})


test_that("predict can evaluate another future sample size", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 5
  )

  prediction <- predict(
    fit,
    future_n = 12
  )

  expect_equal(
    prediction$t,
    0:12
  )

  expect_equal(
    attr(prediction, "future_n"),
    12
  )
})


test_that("future_n equal to one reproduces next_probability", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.40,
    tau = 20
  )

  prediction <- predict(
    fit,
    future_n = 1
  )

  predicted_event_probability <-
    prediction$probability[
      prediction$t == 1
    ]

  expect_equal(
    predicted_event_probability,
    fit$results[[1L]]$next_probability,
    tolerance = 1e-12
  )
})


test_that("predict does not alter the fitted object", {

  fit <- bruno(
    event = c(1, 0, 1, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 4
  )

  original <- fit

  invisible(
    predict(
      fit,
      future_n = 8
    )
  )

  expect_identical(
    fit,
    original
  )
})


test_that("grouped predictions contain one distribution per class", {

  event <- c(1, 0, 1, 1, 0, 0)
  group <- c("A", "A", "A", "B", "B", "B")

  fit <- bruno(
    event = event,
    group = group,
    mu0 = 0.50,
    tau = 10,
    future_n = 4
  )

  prediction <- predict(fit)

  expect_named(
    prediction,
    c("group", "t", "probability")
  )

  expect_setequal(
    unique(prediction$group),
    c("A", "B")
  )

  for (g in c("A", "B")) {

    prediction_g <- prediction[
      prediction$group == g,
      ,
      drop = FALSE
    ]

    expect_equal(
      prediction_g$t,
      0:4
    )

    expect_equal(
      sum(prediction_g$probability),
      1,
      tolerance = 1e-12
    )
  }
})
