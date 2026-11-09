#' Print a bruno Fit
#'
#' Prints the principal observed and predictive quantities stored in an
#' object returned by [bruno()].
#'
#' No additional probability calculation is performed by this method.
#'
#' @param x An object of class `"bruno_fit"`.
#' @param digits Number of significant digits used when printing numeric
#'   probabilities and parameters.
#' @param ... Additional arguments. Currently unused.
#'
#' @return `x`, invisibly.
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
#'   tau = 10
#' )
#'
#' print(fit)
#'
#' @method print bruno_fit
#' @export
print.bruno_fit <- function(x,
                              digits = max(3L, getOption("digits") - 3L),
                              ...) {

  if (!inherits(x, "bruno_fit")) {
    stop(
      "`x` must inherit from class \"bruno_fit\".",
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

  cat("Predictive recurrence of a measurement-defined event\n\n")

  if (x$grouped) {

    printed <- do.call(
      rbind,
      lapply(
        x$results,
        function(result) {
          data.frame(
            group = as.character(result$group),
            n = result$n,
            s = result$s,
            observed_rate = result$observed_rate,
            mu0 = result$mu0,
            tau = result$tau,
            next_probability = result$next_probability,
            stringsAsFactors = FALSE
          )
        }
      )
    )

  } else {

    result <- x$results[[1L]]

    printed <- data.frame(
      n = result$n,
      s = result$s,
      observed_rate = result$observed_rate,
      mu0 = result$mu0,
      tau = result$tau,
      next_probability = result$next_probability
    )
  }

  numeric_columns <- vapply(
    printed,
    is.numeric,
    logical(1)
  )

  printed[numeric_columns] <- lapply(
    printed[numeric_columns],
    signif,
    digits = digits
  )

  print(
    printed,
    row.names = FALSE
  )

  cat(
    "\nFuture sample size:",
    x$future_n,
    "\n"
  )

  if (x$n_excluded > 0L) {
    cat(
      "Excluded cases:",
      x$n_excluded,
      "of",
      x$n_total,
      "\n"
    )
  }

  invisible(x)
}
