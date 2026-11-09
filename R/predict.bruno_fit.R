#' Predict Future Event Recurrence
#'
#' Computes the beta-binomial predictive distribution of the number of
#' occurrences of the declared event in a future sample, using the
#' conditional beta assessment stored in a `"bruno_fit"` object.
#'
#' @param object An object of class `"bruno_fit"`.
#' @param future_n Optional positive integer giving the number of future
#'   cases for which the recurrence-count distribution is required. If
#'   omitted, the value originally supplied to [bruno()] is used.
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' For a fitted predictive class with conditional beta parameters
#' \eqn{\alpha^*} and \eqn{\beta^*}, and a future sample of size \eqn{m},
#' the number of future event occurrences \eqn{T} has predictive
#' distribution
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
#' No simulation is used.
#'
#' When `future_n = 1`, the probability associated with `t = 1` is equal
#' to the next-case predictive probability stored in the fitted object:
#'
#' \deqn{
#' P(T=1) =
#' \frac{\alpha^*}{\alpha^*+\beta^*}.
#' }
#'
#' @return A data frame containing the exact predictive distribution.
#'
#' For an ungrouped fit, the returned columns are:
#' \describe{
#'   \item{t}{
#'     Number of future event occurrences, from `0` to `future_n`.
#'   }
#'   \item{probability}{
#'     Predictive probability assigned to the corresponding recurrence
#'     count.
#'   }
#' }
#'
#' For a grouped fit, an additional `group` column identifies the predictive
#' class to which each distribution belongs.
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
#'   future_n = 5
#' )
#'
#' predict(fit)
#'
#' predict(fit, future_n = 10)
#'
#' @method predict bruno_fit
#' @export
predict.bruno_fit <- function(object,
                                future_n = object$future_n,
                                ...) {

  if (!inherits(object, "bruno_fit")) {
    stop(
      "`object` must inherit from class \"bruno_fit\".",
      call. = FALSE
    )
  }

  validate_future_n(future_n)

  if (length(list(...)) > 0L) {
    stop(
      "Additional arguments supplied through `...` are not currently used.",
      call. = FALSE
    )
  }

  predictions <- lapply(
    object$results,
    function(result) {

      distribution <- beta_binomial(
        future_n = future_n,
        alpha = result$conditional_alpha,
        beta = result$conditional_beta
      )

      if (!is.null(result$group)) {
        distribution <- data.frame(
          group = rep(
            as.character(result$group),
            nrow(distribution)
          ),
          distribution,
          stringsAsFactors = FALSE
        )
      }

      distribution
    }
  )

  if (object$grouped) {

    out <- do.call(
      rbind,
      predictions
    )

    rownames(out) <- NULL

  } else {

    out <- predictions[[1L]]
  }

  attr(out, "future_n") <- future_n

  out
}
