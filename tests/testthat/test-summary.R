test_that("summary returns the expected summary class", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.50,
    tau = 10
  )

  out <- summary(fit)

  expect_s3_class(
    out,
    "summary_bruno_fit"
  )

  expect_named(
    out,
    c(
      "call",
      "level",
      "future_n",
      "results"
    )
  )

  expect_equal(
    out$level,
    0.95
  )
})


test_that("conditional interval is the central beta interval", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.40,
    tau = 20
  )

  level <- 0.90

  out <- summary(
    fit,
    level = level
  )

  result <- fit$results[[1L]]

  gamma <- 1 - level

  expected <- stats::qbeta(
    c(
      gamma / 2,
      1 - gamma / 2
    ),
    shape1 = result$conditional_alpha,
    shape2 = result$conditional_beta
  )

  expect_equal(
    unname(
      out$results[[1L]]$conditional_interval
    ),
    expected
  )
})


test_that("conditional interval endpoints are named lower and upper", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10
  )

  out <- summary(fit)

  expect_named(
    out$results[[1L]]$conditional_interval,
    c("lower", "upper")
  )
})


test_that("conditional interval lies on the probability scale", {

  fit <- bruno(
    event = c(1, 0, 1, 1),
    mu0 = 0.50,
    tau = 10
  )

  interval <-
    summary(fit)$results[[1L]]$conditional_interval

  expect_true(interval[["lower"]] >= 0)
  expect_true(interval[["upper"]] <= 1)

  expect_true(
    interval[["lower"]] <=
      interval[["upper"]]
  )
})


test_that("predictive interval is NULL for future_n equal to one", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 1
  )

  out <- summary(fit)

  expect_null(
    out$results[[1L]]$predictive_interval
  )
})


test_that("predictive interval uses beta-binomial equal-tail quantiles", {

  fit <- bruno(
    event = c(1, 1, 0, 1, 0),
    mu0 = 0.40,
    tau = 20,
    future_n = 10
  )

  level <- 0.90

  out <- summary(
    fit,
    level = level
  )

  distribution <-
    fit$results[[1L]]$predictive

  cumulative <-
    cumsum(distribution$probability)

  gamma <- 1 - level

  lower_index <-
    which(cumulative >= gamma / 2)[1L]

  upper_index <-
    which(
      cumulative >= 1 - gamma / 2
    )[1L]

  expected <- c(
    lower = distribution$t[lower_index],
    upper = distribution$t[upper_index]
  )

  expect_equal(
    out$results[[1L]]$predictive_interval,
    expected
  )
})


test_that("predictive interval endpoints belong to beta-binomial support", {

  fit <- bruno(
    event = c(1, 0, 1, 1),
    mu0 = 0.50,
    tau = 10,
    future_n = 8
  )

  interval <-
    summary(fit)$results[[1L]]$predictive_interval

  expect_true(
    interval[["lower"]] %in% 0:8
  )

  expect_true(
    interval[["upper"]] %in% 0:8
  )

  expect_true(
    interval[["lower"]] <=
      interval[["upper"]]
  )
})


test_that("summary level changes reporting intervals but not fitted quantities", {

  fit <- bruno(
    event = c(1, 0, 1, 1, 0),
    mu0 = 0.50,
    tau = 10,
    future_n = 10
  )

  out_90 <- summary(
    fit,
    level = 0.90
  )

  out_95 <- summary(
    fit,
    level = 0.95
  )

  expect_equal(
    out_90$results[[1L]]$next_probability,
    out_95$results[[1L]]$next_probability
  )

  expect_equal(
    out_90$results[[1L]]$conditional_alpha,
    out_95$results[[1L]]$conditional_alpha
  )

  expect_equal(
    out_90$results[[1L]]$conditional_beta,
    out_95$results[[1L]]$conditional_beta
  )
})


test_that("grouped summaries retain predictive-class identity", {

  fit <- bruno(
    event = c(1, 0, 1, 0),
    group = c("A", "A", "B", "B"),
    mu0 = 0.50,
    tau = 10,
    future_n = 5
  )

  out <- summary(fit)

  expect_named(
    out$results,
    c("A", "B")
  )

  expect_equal(
    out$results$A$group,
    "A"
  )

  expect_equal(
    out$results$B$group,
    "B"
  )
})
