test_that("bruno returns the expected S3 object", {

  event <- c(1, 1, 0, 1, 0)

  fit <- bruno(
    event = event,
    mu0 = 0.50,
    tau = 10
  )

  expect_s3_class(fit, "bruno_fit")

  expect_named(
    fit,
    c(
      "results",
      "events",
      "future_n",
      "n_total",
      "n_analyzed",
      "n_excluded",
      "grouped",
      "call"
    )
  )

  expect_false(fit$grouped)

  expect_equal(fit$future_n, 1L)
  expect_equal(fit$n_total, 5L)
  expect_equal(fit$n_analyzed, 5L)
  expect_equal(fit$n_excluded, 0L)

  expect_length(fit$results, 1L)
})


test_that("ungrouped recurrence counts are correct", {

  event <- c(1, 1, 0, 1, 0)

  fit <- bruno(
    event = event,
    mu0 = 0.50,
    tau = 10
  )

  result <- fit$results[[1L]]

  expect_null(result$group)
  expect_equal(result$n, 5L)
  expect_equal(result$s, 3)
  expect_equal(result$observed_rate, 3 / 5)
})


test_that("initial and conditional parameters are stored correctly", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.40,
    tau = 20
  )

  result <- fit$results[[1L]]

  expect_equal(result$alpha, 8)
  expect_equal(result$beta, 12)

  expect_equal(result$conditional_alpha, 11)
  expect_equal(result$conditional_beta, 14)
})


test_that("next-case probability is the conditional beta mean", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.40,
    tau = 20
  )

  result <- fit$results[[1L]]

  expected <-
    result$conditional_alpha /
    (result$conditional_alpha + result$conditional_beta)

  expect_equal(
    result$next_probability,
    expected
  )
})


test_that("next-case probability satisfies the weighted identity", {

  event <- c(1, 1, 0, 1, 0)

  mu0 <- 0.40
  tau <- 20

  fit <- bruno(
    event = event,
    mu0 = mu0,
    tau = tau
  )

  result <- fit$results[[1L]]

  n <- length(event)
  s <- sum(event)

  weighted <-
    tau / (tau + n) * mu0 +
    n / (tau + n) * (s / n)

  expect_equal(
    result$next_probability,
    weighted
  )
})


test_that("future_n is retained in the fitted object", {

  fit <- bruno(
    event = c(1, 0, 1, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 12
  )

  expect_equal(fit$future_n, 12)
})


test_that("all-zero observed events remain valid", {

  fit <- bruno(
    event = rep(0, 5),
    mu0 = 0.50,
    tau = 10
  )

  result <- fit$results[[1L]]

  expect_equal(result$n, 5)
  expect_equal(result$s, 0)

  expect_equal(result$alpha, 5)
  expect_equal(result$beta, 5)

  expect_equal(result$conditional_alpha, 5)
  expect_equal(result$conditional_beta, 10)

  expect_true(
    result$next_probability > 0 &&
      result$next_probability < 1
  )
})


test_that("all-one observed events remain valid", {

  fit <- bruno(
    event = rep(1, 5),
    mu0 = 0.50,
    tau = 10
  )

  result <- fit$results[[1L]]

  expect_equal(result$n, 5)
  expect_equal(result$s, 5)

  expect_equal(result$conditional_alpha, 10)
  expect_equal(result$conditional_beta, 5)

  expect_true(
    result$next_probability > 0 &&
      result$next_probability < 1
  )
})
