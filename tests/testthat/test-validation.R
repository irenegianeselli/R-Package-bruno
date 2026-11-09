test_that("event is required", {

  expect_error(
    bruno(
      mu0 = 0.50,
      tau = 10
    ),
    "`event` must be supplied"
  )
})


test_that("mu0 is required", {

  expect_error(
    bruno(
      event = c(1, 0, 1),
      tau = 10
    ),
    "`mu0` must be supplied"
  )
})


test_that("tau is required", {

  expect_error(
    bruno(
      event = c(1, 0, 1),
      mu0 = 0.50
    ),
    "`tau` must be supplied"
  )
})


test_that("mu0 must lie strictly between zero and one", {

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0,
      tau = 10
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 1,
      tau = 10
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = -0.1,
      tau = 10
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 1.1,
      tau = 10
    ),
    "strictly between 0 and 1"
  )
})


test_that("mu0 must be finite and numeric", {

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = NA_real_,
      tau = 10
    ),
    "finite numeric values"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = Inf,
      tau = 10
    ),
    "finite numeric values"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = "0.5",
      tau = 10
    ),
    "finite numeric values"
  )
})


test_that("tau must be strictly positive", {

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0.50,
      tau = 0
    ),
    "strictly positive"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0.50,
      tau = -1
    ),
    "strictly positive"
  )
})


test_that("tau must be finite and numeric", {

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0.50,
      tau = NA_real_
    ),
    "finite numeric values"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0.50,
      tau = Inf
    ),
    "finite numeric values"
  )

  expect_error(
    bruno(
      event = c(1, 0),
      mu0 = 0.50,
      tau = "10"
    ),
    "finite numeric values"
  )
})


test_that("future_n must be a finite positive integer", {

  invalid <- list(
    0,
    -1,
    1.5,
    NA_real_,
    Inf
  )

  for (value in invalid) {

    expect_error(
      bruno(
        event = c(1, 0),
        mu0 = 0.50,
        tau = 10,
        future_n = value
      ),
      "positive integer"
    )
  }
})


test_that("event rejects values other than zero and one", {

  expect_error(
    bruno(
      event = c(0, 1, 2),
      mu0 = 0.50,
      tau = 10
    ),
    "only 0 and 1"
  )
})


test_that("event rejects character and factor classifications", {

  expect_error(
    bruno(
      event = c("yes", "no"),
      mu0 = 0.50,
      tau = 10
    ),
    "logical or numeric"
  )

  expect_error(
    bruno(
      event = factor(c("0", "1")),
      mu0 = 0.50,
      tau = 10
    ),
    "logical or numeric"
  )
})


test_that("event must contain at least one case", {

  expect_error(
    bruno(
      event = numeric(0),
      mu0 = 0.50,
      tau = 10
    ),
    "at least one case"
  )
})


test_that("an entirely missing event contains no analysable case", {

  expect_error(
    bruno(
      event = c(NA, NA, NA),
      mu0 = 0.50,
      tau = 10
    ),
    "No case contains an analysable event"
  )
})


test_that("pre and post must be supplied together", {

  dat <- data.frame(
    x = 1:3,
    y = 2:4
  )

  expect_error(
    bruno(
      data = dat,
      pre = x,
      event = post <= pre,
      mu0 = 0.50,
      tau = 10
    ),
    "both be supplied or both be omitted"
  )

  expect_error(
    bruno(
      data = dat,
      post = y,
      event = post <= pre,
      mu0 = 0.50,
      tau = 10
    ),
    "both be supplied or both be omitted"
  )
})


test_that("paired input requires data", {

  expect_error(
    bruno(
      pre = x,
      post = y,
      event = post <= pre,
      mu0 = 0.50,
      tau = 10
    ),
    "`data` must be supplied"
  )
})


test_that("data must be a data frame", {

  expect_error(
    bruno(
      data = matrix(1:6, ncol = 2),
      pre = x,
      post = y,
      event = post <= pre,
      mu0 = 0.50,
      tau = 10
    ),
    "`data` must be a data frame"
  )
})


test_that("column arguments must identify existing columns", {

  dat <- data.frame(
    pre_score = 1:3,
    post_score = 2:4
  )

  expect_error(
    bruno(
      data = dat,
      pre = missing_pre,
      post = post_score,
      event = post <= pre,
      mu0 = 0.50,
      tau = 10
    ),
    "was not found in `data`"
  )
})


test_that("id must uniquely identify cases", {

  expect_error(
    bruno(
      id = c("P1", "P1", "P2"),
      event = c(1, 0, 1),
      mu0 = 0.50,
      tau = 10
    ),
    "uniquely identify cases"
  )
})


test_that("id must not contain missing values", {

  expect_error(
    bruno(
      id = c("P1", NA, "P3"),
      event = c(1, 0, 1),
      mu0 = 0.50,
      tau = 10
    ),
    "must not contain missing values"
  )
})


test_that("id length must match number of cases", {

  expect_error(
    bruno(
      id = c("P1", "P2"),
      event = c(1, 0, 1),
      mu0 = 0.50,
      tau = 10
    ),
    "exactly one identifier for each case"
  )
})


test_that("group length must match number of cases", {

  expect_error(
    bruno(
      event = c(1, 0, 1),
      group = c("A", "B"),
      mu0 = 0.50,
      tau = 10
    ),
    "exactly one value for each case"
  )
})


test_that("group cannot be entirely missing", {

  expect_error(
    bruno(
      event = c(1, 0, 1),
      group = c(NA, NA, NA),
      mu0 = 0.50,
      tau = 10
    ),
    "at least one non-missing predictive class"
  )
})


test_that("ungrouped mu0 and tau must be scalar", {

  expect_error(
    bruno(
      event = c(1, 0, 1),
      mu0 = c(0.40, 0.60),
      tau = 10
    ),
    "`mu0` must be a single value"
  )

  expect_error(
    bruno(
      event = c(1, 0, 1),
      mu0 = 0.50,
      tau = c(5, 10)
    ),
    "`tau` must be a single value"
  )
})


test_that("multiple group-specific mu0 values require names", {

  expect_error(
    bruno(
      event = c(1, 0, 1, 0),
      group = c("A", "A", "B", "B"),
      mu0 = c(0.30, 0.70),
      tau = 10
    ),
    "must be a named vector"
  )
})


test_that("multiple group-specific tau values require names", {

  expect_error(
    bruno(
      event = c(1, 0, 1, 0),
      group = c("A", "A", "B", "B"),
      mu0 = 0.50,
      tau = c(5, 10)
    ),
    "must be a named vector"
  )
})


test_that("group-specific parameter names must match analysed classes exactly", {

  expect_error(
    bruno(
      event = c(1, 0, 1, 0),
      group = c("A", "A", "B", "B"),
      mu0 = c(A = 0.30, C = 0.70),
      tau = 10
    ),
    "must match the analysed predictive classes exactly"
  )

  expect_error(
    bruno(
      event = c(1, 0, 1, 0),
      group = c("A", "A", "B", "B"),
      mu0 = 0.50,
      tau = c(A = 5, C = 10)
    ),
    "must match the analysed predictive classes exactly"
  )
})


test_that("summary level must lie strictly between zero and one", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10
  )

  expect_error(
    summary(fit, level = 0),
    "strictly between 0 and 1"
  )

  expect_error(
    summary(fit, level = 1),
    "strictly between 0 and 1"
  )

  expect_error(
    summary(fit, level = NA_real_),
    "strictly between 0 and 1"
  )
})


test_that("predict validates a replacement future_n", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10
  )

  expect_error(
    predict(
      fit,
      future_n = 2.5
    ),
    "positive integer"
  )
})
