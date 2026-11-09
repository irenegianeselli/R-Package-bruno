#' Resolve the Initial Assessment Across Predictive Classes
#'
#' Internal utility for expanding the user-supplied initial event
#' probability and beta concentration parameter across analysed predictive
#' classes.
#'
#' Each parameter may be supplied either as a single value common to all
#' predictive classes or as a named numeric vector containing exactly one
#' value for each class. Validity of the supplied parameters and their names
#' is checked before expansion.
#'
#' @param mu0 Initial event probability or probabilities.
#' @param tau Concentration parameter or parameters of the initial beta
#'   specification.
#' @param groups Character vector containing the analysed predictive classes.
#'
#' @return A data frame with one row per predictive class and columns
#'   `group`, `mu0`, and `tau`.
#'
#' @noRd
resolve_initial_assessment <- function(mu0,
                                       tau,
                                       groups) {

  validate_initial_assessment(
    mu0 = mu0,
    tau = tau,
    groups = groups
  )

  groups <- as.character(groups)

  mu0_values <- if (length(mu0) == 1L) {
    rep(mu0, length(groups))
  } else {
    unname(mu0[groups])
  }

  tau_values <- if (length(tau) == 1L) {
    rep(tau, length(groups))
  } else {
    unname(tau[groups])
  }

  data.frame(
    group = groups,
    mu0 = mu0_values,
    tau = tau_values,
    stringsAsFactors = FALSE
  )
}
