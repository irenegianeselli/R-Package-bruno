test_that("grouped analysis conditions separately by predictive class", {

  event <- c(
    1, 1, 0,
    1, 0, 0
  )

  group <- c(
    "A", "A", "A",
    "B", "B", "B"
  )

  fit <- bruno(
    event = event,
    group = group,
    mu0 = 0.50,
    tau = 10
  )

  expect_true(fit$grouped)

  expect_named(
    fit$results,
    c("A", "B")
  )

  expect_equal(
    fit$results$A$n,
    3
  )

  expect_equal(
    fit$results$A$s,
    2
  )

  expect_equal(
    fit$results$B$n,
    3
  )

  expect_equal(
    fit$results$B$s,
    1
  )
})


test_that("scalar mu0 and tau are applied to every predictive class", {

  fit <- bruno(
    event = c(1, 0, 1, 0),
    group = c("A", "A", "B", "B"),
    mu0 = 0.60,
    tau = 12
  )

  expect_equal(
    fit$results$A$mu0,
    0.60
  )

  expect_equal(
    fit$results$B$mu0,
    0.60
  )

  expect_equal(
    fit$results$A$tau,
    12
  )

  expect_equal(
    fit$results$B$tau,
    12
  )
})


test_that("named mu0 and tau are matched by predictive-class name", {

  fit <- bruno(
    event = c(1, 0, 1, 0),
    group = c("A", "A", "B", "B"),
    mu0 = c(B = 0.70, A = 0.30),
    tau = c(B = 15, A = 5)
  )

  expect_equal(
    fit$results$A$mu0,
    0.30
  )

  expect_equal(
    fit$results$A$tau,
    5
  )

  expect_equal(
    fit$results$B$mu0,
    0.70
  )

  expect_equal(
    fit$results$B$tau,
    15
  )
})


test_that("missing group membership excludes only the affected case", {

  event <- c(TRUE, TRUE, FALSE, TRUE)
  group <- c("A", NA, "A", "B")

  fit <- bruno(
    event = event,
    group = group,
    mu0 = 0.50,
    tau = 10
  )

  expect_equal(fit$n_total, 4)
  expect_equal(fit$n_analyzed, 3)
  expect_equal(fit$n_excluded, 1)

  expect_false(
    fit$events$included[2]
  )

  expect_equal(
    fit$results$A$n,
    2
  )

  expect_equal(
    fit$results$B$n,
    1
  )
})


test_that("predictive classes without analysable events are not fitted", {

  event <- c(TRUE, FALSE, NA, NA)
  group <- c("A", "A", "B", "B")

  fit <- bruno(
    event = event,
    group = group,
    mu0 = 0.50,
    tau = 10
  )

  expect_named(
    fit$results,
    "A"
  )

  expect_equal(
    fit$results$A$n,
    2
  )

  expect_false(
    "B" %in% names(fit$results)
  )
})


test_that("example data have the predetermined grouped counts", {

  data(
    bruno_example,
    package = "bruno"
  )

  fit <- bruno(
    data = bruno_example,
    id = id,
    pre = pre,
    post = post,
    event = post <= pre,
    mu0 = 0.50,
    tau = 10,
    group = group
  )

  expect_equal(fit$n_total, 12)
  expect_equal(fit$n_analyzed, 10)
  expect_equal(fit$n_excluded, 2)

  expect_equal(fit$results$A$n, 5)
  expect_equal(fit$results$A$s, 4)

  expect_equal(fit$results$B$n, 5)
  expect_equal(fit$results$B$s, 3)
})
