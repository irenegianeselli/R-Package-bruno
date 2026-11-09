#' Compute the Beta-Binomial Predictive Distribution
#'
#' Internal mathematical utility for computing the predictive distribution
#' of the number of occurrences of the declared event in a prespecified
#' future sample.
#'
#' Given a future sample size \eqn{m} and positive conditional beta
#' parameters \eqn{\alpha^*} and \eqn{\beta^*}, the predictive probability
#' of exactly \eqn{t} future occurrences is
#'
#' \deqn{
#' P(T=t) =
#' {m \choose t}
#' \frac{
#' B(\alpha^*+t,\ \beta^*+m-t)
#' }{
#' B(\alpha^*,\ \beta^*)
#' },
#' \quad t=0,\ldots,m.
#' }
#'
#' The probability mass function is evaluated on the log scale. A common
#' log-scale shift is applied before exponentiation to reduce numerical
#' underflow, after which the probabilities are normalised to sum to one.
#' This numerical procedure evaluates the same beta-binomial probability
#' mass function and introduces no simulation.
#'
#' @param future_n Prespecified number of future cases. Must be a single
#'   finite positive integer.
#' @param alpha Positive conditional alpha parameter.
#' @param beta Positive conditional beta parameter.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{t}{
#'     Possible number of future event occurrences, from `0` to `future_n`.
#'   }
#'   \item{probability}{
#'     Predictive probability assigned to each value of `t`.
#'   }
#' }
#'
#' @noRd
beta_binomial <- function(future_n,
                          alpha,
                          beta) {

  validate_future_n(future_n)

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0) {
    stop(
      "`alpha` must be a single finite positive value.",
      call. = FALSE
    )
  }

  if (!is.numeric(beta) ||
      length(beta) != 1L ||
      is.na(beta) ||
      !is.finite(beta) ||
      beta <= 0) {
    stop(
      "`beta` must be a single finite positive value.",
      call. = FALSE
    )
  }

  t <- 0:future_n

  log_probability <-
    lchoose(future_n, t) +
    lbeta(
      alpha + t,
      beta + future_n - t
    ) -
    lbeta(alpha, beta)

  log_shift <- max(log_probability)

  probability <- exp(
    log_probability - log_shift
  )

  total <- sum(probability)

  if (!is.finite(total) || total <= 0) {
    stop(
      "The beta-binomial predictive probabilities could not be computed.",
      call. = FALSE
    )
  }

  probability <- probability / total

  data.frame(
    t = t,
    probability = probability
  )
}


#' Compute a Beta-Binomial Quantile
#'
#' Internal utility returning the smallest recurrence count \eqn{t} for
#' which the beta-binomial cumulative predictive probability is at least
#' `prob`.
#'
#' For predictive mass function \eqn{p(t)}, the quantile is defined as
#'
#' \deqn{
#' Q(p) =
#' \min\left\{
#' t:\sum_{j=0}^{t}p(j)\geq p
#' \right\}.
#' }
#'
#' @param distribution Beta-binomial predictive distribution returned by
#'   `beta_binomial()`.
#' @param prob Probability in the closed interval from `0` to `1`.
#'
#' @return Integer beta-binomial quantile.
#'
#' @noRd
beta_binomial_quantile <- function(distribution,
                                   prob) {

  if (!is.numeric(prob) ||
      length(prob) != 1L ||
      is.na(prob) ||
      !is.finite(prob) ||
      prob < 0 ||
      prob > 1) {
    stop(
      "`prob` must be a single finite value between 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.data.frame(distribution) ||
      !all(c("t", "probability") %in% names(distribution))) {
    stop(
      "`distribution` must be a beta-binomial predictive distribution.",
      call. = FALSE
    )
  }

  if (prob == 0) {
    return(distribution$t[[1L]])
  }

  cumulative <- cumsum(distribution$probability)

  index <- which(cumulative >= prob)[1L]

  if (is.na(index)) {
    index <- nrow(distribution)
  }

  distribution$t[[index]]
}


#' Compute a Central Beta-Binomial Predictive Interval
#'
#' Internal utility for obtaining the equal-tail predictive interval for the
#' future recurrence count from a beta-binomial predictive distribution.
#'
#' For reporting level \eqn{1-\gamma}, the interval endpoints are
#'
#' \deqn{
#' \left[
#' Q(\gamma/2),
#' Q(1-\gamma/2)
#' \right],
#' }
#'
#' where \eqn{Q} is the quantile function of the beta-binomial predictive
#' distribution.
#'
#' Because the predictive distribution is discrete, the probability
#' contained by the resulting interval need not equal the requested level
#' exactly and may exceed it.
#'
#' @param distribution Beta-binomial predictive distribution returned by
#'   `beta_binomial()`.
#' @param level Reporting probability strictly between `0` and `1`.
#'
#' @return A named integer vector with elements `lower` and `upper`.
#'
#' @noRd
beta_binomial_interval <- function(distribution,
                                   level) {

  validate_probability_level(level)

  gamma <- 1 - level

  lower <- beta_binomial_quantile(
    distribution = distribution,
    prob = gamma / 2
  )

  upper <- beta_binomial_quantile(
    distribution = distribution,
    prob = 1 - gamma / 2
  )

  c(
    lower = lower,
    upper = upper
  )
}
