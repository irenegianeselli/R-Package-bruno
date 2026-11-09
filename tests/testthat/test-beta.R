test_that("beta parameters reproduce mu0 and tau", {

  pars <- bruno:::beta_parameters(
    mu0 = 0.35,
    tau = 20,
    s = 4,
    n = 10
  )

  expect_equal(
    pars$alpha + pars$beta,
    20
  )

  expect_equal(
    pars$alpha /
      (pars$alpha + pars$beta),
    0.35
  )
})


test_that("initial beta parameters follow the specified parameterisation", {

  mu0 <- 0.30
  tau <- 25

  pars <- bruno:::beta_parameters(
    mu0 = mu0,
    tau = tau,
    s = 0,
    n = 1
  )

  expect_equal(
    pars$alpha,
    mu0 * tau
  )

  expect_equal(
    pars$beta,
    (1 - mu0) * tau
  )
})


test_that("conditioning updates alpha and beta exactly", {

  mu0 <- 0.40
  tau <- 20
  s <- 7
  n <- 12

  pars <- bruno:::beta_parameters(
    mu0 = mu0,
    tau = tau,
    s = s,
    n = n
  )

  alpha <- mu0 * tau
  beta <- (1 - mu0) * tau

  expect_equal(
    pars$conditional_alpha,
    alpha + s
  )

  expect_equal(
    pars$conditional_beta,
    beta + n - s
  )
})


test_that("conditional concentration equals tau plus n", {

  tau <- 17
  n <- 13

  pars <- bruno:::beta_parameters(
    mu0 = 0.45,
    tau = tau,
    s = 8,
    n = n
  )

  expect_equal(
    pars$conditional_alpha +
      pars$conditional_beta,
    tau + n
  )
})


test_that("beta-binomial probabilities sum to one", {

  distribution <- bruno:::beta_binomial(
    future_n = 20,
    alpha = 8.3,
    beta = 5.7
  )

  expect_equal(
    sum(distribution$probability),
    1,
    tolerance = 1e-12
  )
})


test_that("beta-binomial support is complete", {

  m <- 12

  distribution <- bruno:::beta_binomial(
    future_n = m,
    alpha = 4,
    beta = 7
  )

  expect_equal(
    distribution$t,
    0:m
  )

  expect_equal(
    nrow(distribution),
    m + 1L
  )
})


test_that("beta-binomial probabilities are finite and non-negative", {

  distribution <- bruno:::beta_binomial(
    future_n = 100,
    alpha = 2.5,
    beta = 8.5
  )

  expect_true(
    all(is.finite(distribution$probability))
  )

  expect_true(
    all(distribution$probability >= 0)
  )
})


test_that("beta-binomial implementation agrees with its analytic pmf", {

  m <- 8
  alpha <- 3.5
  beta <- 6.5

  distribution <- bruno:::beta_binomial(
    future_n = m,
    alpha = alpha,
    beta = beta
  )

  t <- 0:m

  expected <- exp(
    lchoose(m, t) +
      lbeta(
        alpha + t,
        beta + m - t
      ) -
      lbeta(alpha, beta)
  )

  expected <- expected / sum(expected)

  expect_equal(
    distribution$probability,
    expected,
    tolerance = 1e-12
  )
})


test_that("future sample size one reproduces Bernoulli prediction", {

  alpha <- 9
  beta <- 6

  distribution <- bruno:::beta_binomial(
    future_n = 1,
    alpha = alpha,
    beta = beta
  )

  expected_success <- alpha / (alpha + beta)

  expect_equal(
    distribution$probability[
      distribution$t == 1
    ],
    expected_success,
    tolerance = 1e-12
  )

  expect_equal(
    distribution$probability[
      distribution$t == 0
    ],
    1 - expected_success,
    tolerance = 1e-12
  )
})


test_that("extreme observed recurrence counts produce valid beta parameters", {

  no_events <- bruno:::beta_parameters(
    mu0 = 0.50,
    tau = 10,
    s = 0,
    n = 10
  )

  all_events <- bruno:::beta_parameters(
    mu0 = 0.50,
    tau = 10,
    s = 10,
    n = 10
  )

  expect_true(no_events$conditional_alpha > 0)
  expect_true(no_events$conditional_beta > 0)

  expect_true(all_events$conditional_alpha > 0)
  expect_true(all_events$conditional_beta > 0)
})
