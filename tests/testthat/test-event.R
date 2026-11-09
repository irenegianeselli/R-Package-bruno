test_that("paired input evaluates the declared event correctly", {

  dat <- data.frame(
    id = c("P1", "P2", "P3", "P4"),
    before = c(3, 4, 5, 2),
    after = c(2, 4, 6, 1)
  )

  fit <- bruno(
    data = dat,
    id = id,
    pre = before,
    post = after,
    event = post <= pre,
    mu0 = 0.50,
    tau = 10
  )

  expect_equal(
    fit$events$event,
    c(TRUE, TRUE, FALSE, TRUE)
  )

  expect_equal(fit$results[[1L]]$s, 3)
  expect_equal(fit$results[[1L]]$n, 4)
})


test_that("pre and post aliases refer to selected columns", {

  dat <- data.frame(
    baseline_score = c(2, 5, 4),
    followup_score = c(1, 7, 2)
  )

  fit <- bruno(
    data = dat,
    pre = baseline_score,
    post = followup_score,
    event = abs(post - pre) >= 2,
    mu0 = 0.50,
    tau = 10
  )

  expect_equal(
    fit$events$event,
    c(FALSE, TRUE, TRUE)
  )
})


test_that("logical and binary direct events give identical results", {

  logical_event <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  numeric_event <- c(1, 1, 0, 1, 0)

  fit_logical <- bruno(
    event = logical_event,
    mu0 = 0.40,
    tau = 20
  )

  fit_numeric <- bruno(
    event = numeric_event,
    mu0 = 0.40,
    tau = 20
  )

  expect_equal(
    fit_logical$results,
    fit_numeric$results
  )

  expect_equal(
    fit_logical$events$event,
    fit_numeric$events$event
  )
})


test_that("direct and paired specifications of the same event agree", {

  dat <- data.frame(
    id = paste0("P", 1:5),
    pre = c(2, 4, 3, 5, 1),
    post = c(1, 5, 3, 4, 2)
  )

  declared_event <- with(
    dat,
    post <= pre
  )

  paired_fit <- bruno(
    data = dat,
    id = id,
    pre = pre,
    post = post,
    event = post <= pre,
    mu0 = 0.60,
    tau = 12,
    future_n = 6
  )

  direct_fit <- bruno(
    id = dat$id,
    event = declared_event,
    mu0 = 0.60,
    tau = 12,
    future_n = 6
  )

  paired_result <- paired_fit$results[[1L]]
  direct_result <- direct_fit$results[[1L]]

  expect_equal(paired_result$n, direct_result$n)
  expect_equal(paired_result$s, direct_result$s)
  expect_equal(
    paired_result$observed_rate,
    direct_result$observed_rate
  )
  expect_equal(
    paired_result$conditional_alpha,
    direct_result$conditional_alpha
  )
  expect_equal(
    paired_result$conditional_beta,
    direct_result$conditional_beta
  )
  expect_equal(
    paired_result$next_probability,
    direct_result$next_probability
  )
  expect_equal(
    paired_result$predictive,
    direct_result$predictive
  )
})


test_that("missing event values are excluded without imputation", {

  event <- c(TRUE, FALSE, NA, TRUE, NA)

  fit <- bruno(
    event = event,
    mu0 = 0.50,
    tau = 10
  )

  expect_equal(fit$n_total, 5)
  expect_equal(fit$n_analyzed, 3)
  expect_equal(fit$n_excluded, 2)

  expect_equal(
    fit$events$included,
    c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(fit$results[[1L]]$n, 3)
  expect_equal(fit$results[[1L]]$s, 2)
})


test_that("missing paired values remain visible in audit information", {

  dat <- data.frame(
    id = c("P1", "P2", "P3"),
    pre = c(2, 3, 4),
    post = c(1, NA, 5)
  )

  fit <- bruno(
    data = dat,
    id = id,
    pre = pre,
    post = post,
    event = post <= pre,
    mu0 = 0.50,
    tau = 10
  )

  expect_equal(
    fit$events$id,
    dat$id
  )

  expect_equal(
    fit$events$pre,
    dat$pre
  )

  expect_equal(
    fit$events$post,
    dat$post
  )

  expect_true(is.na(fit$events$event[2]))
  expect_false(fit$events$included[2])
})


test_that("row-based case index is created when id is absent", {

  fit <- bruno(
    event = c(1, 0, 1),
    mu0 = 0.50,
    tau = 10
  )

  expect_true("case" %in% names(fit$events))
  expect_false("id" %in% names(fit$events))

  expect_equal(
    fit$events$case,
    1:3
  )
})

test_that("example data have the predetermined ungrouped event counts", {

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
    tau = 10
  )

  expect_equal(fit$n_total, 12)
  expect_equal(fit$n_analyzed, 11)
  expect_equal(fit$n_excluded, 1)

  expect_equal(
    fit$results[[1L]]$n,
    11
  )

  expect_equal(
    fit$results[[1L]]$s,
    8
  )

  expect_equal(
    fit$results[[1L]]$observed_rate,
    8 / 11
  )
})
