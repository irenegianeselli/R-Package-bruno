#' Summarise a bruno Fit
#'
#' Summarises the observed recurrence information, the initial and
#' conditional beta parameters, the next-case predictive probability, and
#' mathematically defined probability intervals derived from the fitted
#' distributions.
#'
#' @param object An object of class `"bruno_fit"`.
#' @param level Reporting probability for the conditional and predictive
#'   intervals. Must be strictly between `0` and `1`. Defaults to `0.95`.
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' For conditional beta parameters \eqn{\alpha^*} and \eqn{\beta^*},
#'
#' \deqn{
#' x \mid A_{n,s}
#' \sim
#' \mathrm{Beta}(\alpha^*,\beta^*).
#' }
#'
#' Let the requested reporting level be \eqn{1-\gamma}. The central
#' equal-tail conditional probability interval is
#'
#' \deqn{
#' \left[
#' F^{-1}_{\mathrm{Beta}}(\gamma/2),
#' F^{-1}_{\mathrm{Beta}}(1-\gamma/2)
#' \right].
#' }
#'
#' This interval describes the dispersion of the conditional beta
#' assessment for \eqn{x}. It is not an interval around the predictive
#' probability for the next event.
#'
#' For a future sample of size greater than one, let \eqn{T} denote the
#' future recurrence count and let \eqn{Q_T} denote the quantile function
#' of its beta-binomial predictive distribution. The equal-tail predictive
#' interval is
#'
#' \deqn{
#' \left[
#' Q_T(\gamma/2),
#' Q_T(1-\gamma/2)
#' \right].
#' }
#'
#' Because \eqn{T} is discrete, the probability contained by the predictive
#' interval can exceed the requested reporting level.
#'
#' No simulation is used to compute either interval.
#'
#' @return An object of class `"summary_bruno_fit"` containing:
#' \describe{
#'   \item{call}{
#'     Matched call from the fitted object.
#'   }
#'   \item{level}{
#'     Reporting probability used for the intervals.
#'   }
#'   \item{future_n}{
#'     Future sample size stored in the fitted object.
#'   }
#'   \item{results}{
#'     A list containing one summary for the ungrouped analysis or one
#'     summary per predictive class. Each element contains the quantities
#'     stored in the fitted result together with `conditional_interval` and,
#'     when `future_n > 1`, `predictive_interval`.
#'   }
#' }
#'
#' @examples
#' dat <- data.frame(
#'   id = paste0("P", 1:6),
#'   pre = c(2, 3, 4, 2, 5, 3),
#'   post = c(1, 3, 5, 2, 3, 4)
#' )
#'
#' fit <- bruno(
#'   data = dat,
#'   id = id,
#'   pre = pre,
#'   post = post,
#'   event = post <= pre,
#'   mu0 = 0.50,
#'   tau = 10,
#'   future_n = 10
#' )
#'
#' summary(fit)
#'
#' summary(fit, level = 0.90)
#'
#' @method summary bruno_fit
#' @export
summary.bruno_fit <- function(object,
                                level = 0.95,
                                ...) {

  if (!inherits(object, "bruno_fit")) {
    stop(
      "`object` must inherit from class \"bruno_fit\".",
      call. = FALSE
    )
  }

  validate_probability_level(level)

  if (length(list(...)) > 0L) {
    stop(
      "Additional arguments supplied through `...` are not currently used.",
      call. = FALSE
    )
  }

  gamma <- 1 - level

  results <- lapply(
    object$results,
    function(result) {

      conditional_interval <- stats::qbeta(
        c(
          gamma / 2,
          1 - gamma / 2
        ),
        shape1 = result$conditional_alpha,
        shape2 = result$conditional_beta
      )

      names(conditional_interval) <- c(
        "lower",
        "upper"
      )

      predictive_interval <- NULL

      if (object$future_n > 1L) {

        predictive_interval <- beta_binomial_interval(
          distribution = result$predictive,
          level = level
        )
      }

      list(
        group = result$group,
        n = result$n,
        s = result$s,
        observed_rate = result$observed_rate,
        mu0 = result$mu0,
        tau = result$tau,
        alpha = result$alpha,
        beta = result$beta,
        conditional_alpha = result$conditional_alpha,
        conditional_beta = result$conditional_beta,
        next_probability = result$next_probability,
        conditional_interval = conditional_interval,
        predictive_interval = predictive_interval
      )
    }
  )

  if (object$grouped) {
    names(results) <- names(object$results)
  }

  out <- list(
    call = object$call,
    level = level,
    future_n = object$future_n,
    results = results
  )

  class(out) <- "summary_bruno_fit"

  out
}


#' Validate a Probability Reporting Level
#'
#' Checks that a reporting probability is a single finite numeric value
#' strictly between `0` and `1`.
#'
#' @param level Reporting probability.
#'
#' @return Invisibly returns `TRUE` when `level` is valid.
#'
#' @noRd
validate_probability_level <- function(level) {

  if (!is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level) ||
      level <= 0 ||
      level >= 1) {
    stop(
      "`level` must be a single finite value strictly between 0 and 1.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Print a bruno Summary
#'
#' Prints the principal quantities and probability intervals contained in
#' an object returned by `summary.bruno_fit()`.
#'
#' @param x An object of class `"summary_bruno_fit"`.
#' @param digits Number of significant digits used for printed numeric
#'   quantities.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @method print summary_bruno_fit
#' @export
print.summary_bruno_fit <- function(
    x,
    digits = max(3L, getOption("digits") - 3L),
    ...) {

  if (!inherits(x, "summary_bruno_fit")) {
    stop(
      "`x` must inherit from class \"summary_bruno_fit\".",
      call. = FALSE
    )
  }

  if (!is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      !is.finite(digits) ||
      digits < 1 ||
      digits != floor(digits)) {
    stop(
      "`digits` must be a single finite positive integer.",
      call. = FALSE
    )
  }

  if (length(list(...)) > 0L) {
    stop(
      "Additional arguments supplied through `...` are not currently used.",
      call. = FALSE
    )
  }

  percentage <- format(
    100 * x$level,
    trim = TRUE,
    scientific = FALSE
  )

  cat("Predictive recurrence of a measurement-defined event\n")
  cat("Interval level:", paste0(percentage, "%"), "\n\n")

  for (i in seq_along(x$results)) {

    result <- x$results[[i]]

    if (!is.null(result$group)) {
      cat(
        "Predictive class:",
        as.character(result$group),
        "\n"
      )
    }

    cat(
      "Observed cases:",
      result$n,
      "\n"
    )

    cat(
      "Event occurrences:",
      result$s,
      "\n"
    )

    cat(
      "Observed recurrence:",
      signif(result$observed_rate, digits),
      "\n"
    )

    cat(
      "mu0:",
      signif(result$mu0, digits),
      "\n"
    )

    cat(
      "tau:",
      signif(result$tau, digits),
      "\n"
    )

    cat(
      "Conditional beta:",
      paste0(
        "Beta(",
        signif(result$conditional_alpha, digits),
        ", ",
        signif(result$conditional_beta, digits),
        ")"
      ),
      "\n"
    )

    cat(
      "Next-case probability:",
      signif(result$next_probability, digits),
      "\n"
    )

    cat(
      "Conditional interval:",
      paste0(
        "[",
        signif(
          result$conditional_interval[["lower"]],
          digits
        ),
        ", ",
        signif(
          result$conditional_interval[["upper"]],
          digits
        ),
        "]"
      ),
      "\n"
    )

    if (!is.null(result$predictive_interval)) {

      cat(
        "Predictive interval for T:",
        paste0(
          "[",
          result$predictive_interval[["lower"]],
          ", ",
          result$predictive_interval[["upper"]],
          "]"
        ),
        "\n"
      )
    }

    if (i < length(x$results)) {
      cat("\n")
    }
  }

  invisible(x)
}
