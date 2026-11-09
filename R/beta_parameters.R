#' Compute Initial and Conditional Beta Parameters
#'
#' Internal mathematical utility for the beta specification used by
#' [bruno()].
#'
#' Given an initial event probability \eqn{\mu_0}, concentration
#' \eqn{\tau}, and observed recurrence information consisting of \eqn{s}
#' occurrences among \eqn{n} cases, the function computes
#'
#' \deqn{a = \mu_0 \tau}
#'
#' and
#'
#' \deqn{b = (1 - \mu_0)\tau,}
#'
#' followed by the conditional parameters
#'
#' \deqn{a^* = a + s}
#'
#' and
#'
#' \deqn{b^* = b + n - s.}
#'
#' @param mu0 Initial event probability. Must be a single finite numeric
#'   value strictly between `0` and `1`.
#' @param tau Concentration parameter of the initial beta specification.
#'   Must be a single finite positive numeric value.
#' @param s Observed number of event occurrences.
#' @param n Number of analysed observed cases.
#'
#' @return A list containing `alpha`, `beta`, `conditional_alpha`, and
#'   `conditional_beta`.
#'
#' @noRd
beta_parameters <- function(mu0,
                            tau,
                            s,
                            n) {

  if (!is.numeric(mu0) ||
      length(mu0) != 1L ||
      is.na(mu0) ||
      !is.finite(mu0) ||
      mu0 <= 0 ||
      mu0 >= 1) {
    stop(
      "`mu0` must be a single finite value strictly between 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.numeric(tau) ||
      length(tau) != 1L ||
      is.na(tau) ||
      !is.finite(tau) ||
      tau <= 0) {
    stop(
      "`tau` must be a single finite positive value.",
      call. = FALSE
    )
  }

  if (!is.numeric(n) ||
      length(n) != 1L ||
      is.na(n) ||
      !is.finite(n) ||
      n < 1 ||
      n != floor(n)) {
    stop(
      "`n` must be a single finite positive integer.",
      call. = FALSE
    )
  }

  if (!is.numeric(s) ||
      length(s) != 1L ||
      is.na(s) ||
      !is.finite(s) ||
      s < 0 ||
      s > n ||
      s != floor(s)) {
    stop(
      "`s` must be a single integer between 0 and `n`.",
      call. = FALSE
    )
  }

  alpha <- mu0 * tau
  beta <- (1 - mu0) * tau

  conditional_alpha <- alpha + s
  conditional_beta <- beta + n - s

  list(
    alpha = alpha,
    beta = beta,
    conditional_alpha = conditional_alpha,
    conditional_beta = conditional_beta
  )
}
