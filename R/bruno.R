#' Predict Recurrence of a Measurement-Defined Event
#'
#' `bruno()` assigns predictive probabilities to the recurrence of a
#' prespecified binary event under a stated judgement of exchangeability.
#' The event can be defined from paired pre-post measurements through a
#' user-specified logical expression or supplied directly as an already
#' classified binary event.
#'
#' @param data Optional data frame containing the observations. Required
#'   when `pre` and `post` are used to define the event. It may also be used
#'   with a directly specified event when case identifiers or predictive
#'   classes are stored in the same data frame.
#' @param id Optional unquoted column name identifying cases when `data`
#'   is supplied, or an optional identifier vector when `data` is omitted.
#'   Identifiers are retained for case-level classification and diagnostic
#'   reporting and do not enter the predictive calculation.
#' @param pre Optional unquoted column name identifying the pre-measurement.
#'   Used together with `post` when the event is defined from paired
#'   measurements.
#' @param post Optional unquoted column name identifying the post-measurement.
#'   Used together with `pre` when the event is defined from paired
#'   measurements.
#' @param event Definition of the binary event whose recurrence is to be
#'   predicted. When `pre` and `post` are supplied, `event` is evaluated
#'   with `pre` and `post` referring to the selected measurement columns.
#'   Otherwise, `event` is evaluated directly and must produce a logical
#'   vector or a numeric vector containing only `0` and `1`, apart from
#'   missing values.
#' @param mu0 Initial probability assigned to the event before the observed
#'   recurrence information is incorporated. Must be numeric, finite, and
#'   strictly between `0` and `1`. When `group` is supplied, `mu0` may be
#'   a single value common to all predictive classes or a named numeric
#'   vector containing exactly one value for each analysed class.
#' @param tau Positive finite concentration parameter of the initial beta
#'   specification. When `group` is supplied, `tau` may be a single value
#'   common to all predictive classes or a named numeric vector containing
#'   exactly one value for each analysed class.
#' @param group Optional unquoted column name identifying predictive classes
#'   when `data` is supplied, or an optional grouping vector when `data`
#'   is omitted. Prediction is performed separately within each analysed
#'   predictive class.
#' @param future_n Positive integer giving the prespecified number of cases
#'   in the future sample. Defaults to `1`.
#'
#' @details
#' Let \eqn{E_i} denote the binary event defined for case \eqn{i} from the
#' observed measurement outcome or outcomes. `bruno()` does not assign a
#' substantive interpretation such as improvement, deterioration, increase,
#' or decrease to the event. The event definition is supplied by the user.
#'
#' Suppose that the event occurs in \eqn{s} of \eqn{n} observed cases. The
#' observed proportion \eqn{s/n} describes the recurrence frequency in those
#' cases. It does not by itself determine the probability assigned to a
#' future occurrence.
#'
#' Let \eqn{E_1, E_2, \ldots} be judged exchangeable for the intended
#' prediction. For an indefinitely extendible exchangeable binary sequence,
#' de Finetti's representation theorem expresses the assigned joint
#' probabilities as a mixture of Bernoulli laws over a mixing distribution
#' on the unit interval. Exchangeability is a judgement concerning the
#' predictive relevance of the observed cases for future cases; it is not
#' estimated or tested by `bruno()`.
#'
#' `bruno()` adopts a beta distribution as an additional parametric
#' specification of the mixing distribution. The beta specification is not
#' implied by de Finetti's representation theorem.
#'
#' Before the observed recurrence information is incorporated, the user
#' supplies an initial event probability \eqn{\mu_0} and a concentration
#' parameter \eqn{\tau}. The corresponding beta parameters are
#'
#' \deqn{a = \mu_0 \tau}
#'
#' and
#'
#' \deqn{b = (1 - \mu_0)\tau.}
#'
#' Thus, \eqn{\mu_0 = a/(a+b)} and \eqn{\tau = a+b}. The parameter
#' \eqn{\tau} expresses the concentration of the initial beta assessment.
#' It is not a parameter supplied by de Finetti's representation theorem
#' and does not automatically represent a literal number of previous
#' observations.
#'
#' Let \eqn{A_{n,s}} denote the observed information that exactly \eqn{s}
#' of the first \eqn{n} events occurred. Conditioning gives
#'
#' \deqn{
#' x \mid A_{n,s} \sim \mathrm{Beta}(a+s,\ b+n-s).
#' }
#'
#' The predictive probability assigned to occurrence of the event for the
#' next case is
#'
#' \deqn{
#' P(E_{n+1} \mid A_{n,s}) =
#' \frac{a+s}{a+b+n}.
#' }
#'
#' Equivalently,
#'
#' \deqn{
#' P(E_{n+1} \mid A_{n,s}) =
#' \frac{\tau}{\tau+n}\mu_0 +
#' \frac{n}{\tau+n}\frac{s}{n}.
#' }
#'
#' For a prespecified future sample of size \eqn{m}, let \eqn{T} denote the
#' number of future occurrences of the same event. The predictive
#' distribution is beta-binomial:
#'
#' \deqn{
#' P(T=t \mid A_{n,s}) =
#' {m \choose t}
#' \frac{
#' B(a+s+t,\ b+n-s+m-t)
#' }{
#' B(a+s,\ b+n-s)
#' },
#' \quad t=0,\ldots,m,
#' }
#'
#' where \eqn{B} denotes the beta function.
#'
#' The conditional beta distribution permits direct calculation of central
#' probability intervals for \eqn{x}. These intervals describe the
#' dispersion of the conditional beta assessment and are distinct from the
#' predictive probability assigned to the next binary event.
#'
#' For a future sample with more than one case, the beta-binomial
#' distribution permits direct calculation of predictive intervals for the
#' future recurrence count \eqn{T}. Conditional and predictive intervals
#' are obtained analytically from their respective distributions. Their
#' reporting level is specified when the fitted object is summarised or
#' queried and is not fixed during fitting.
#'
#' When `group` is supplied, the recurrence information is conditioned and
#' prediction is performed separately within each analysed predictive class.
#' A common `mu0` and `tau` may be supplied for all classes, or class-specific
#' values may be supplied as named vectors. Group membership does not
#' establish exchangeability; the exchangeability judgement remains part
#' of the predictive specification.
#'
#' Cases for which the declared event cannot be determined do not contribute
#' to \eqn{n} or \eqn{s}. When predictive classes are supplied, cases with
#' missing class membership likewise do not contribute to a class-specific
#' calculation. No missing values are imputed. Case-level information is
#' retained so that analysed and excluded observations remain identifiable.
#'
#' The resulting probabilities concern recurrence of the declared
#' measurement-defined event. They do not by themselves represent latent
#' change, intervention efficacy, measurement validity, or a research
#' decision.
#'
#' @return An object of class `"bruno_fit"` containing:
#' \describe{
#'   \item{results}{
#'     A list containing one set of predictive results for the ungrouped
#'     analysis or one set for each analysed predictive class. Each set
#'     contains \eqn{n}, \eqn{s}, the observed recurrence proportion,
#'     `mu0`, `tau`, the initial and conditional beta parameters, the
#'     predictive probability for the next case, and the beta-binomial
#'     predictive distribution for the prespecified future sample.
#'   }
#'   \item{events}{
#'     Case-level audit data containing the supplied identifier or original
#'     row index, event classification, inclusion status, and the measurement
#'     and grouping information relevant to the classification.
#'   }
#'   \item{future_n}{
#'     Prespecified size of the future sample.
#'   }
#'   \item{n_total}{
#'     Number of cases presented for classification.
#'   }
#'   \item{n_analyzed}{
#'     Number of cases contributing to the recurrence calculation.
#'   }
#'   \item{n_excluded}{
#'     Number of cases excluded from the recurrence calculation.
#'   }
#'   \item{grouped}{
#'     Logical value indicating whether predictive classes were supplied.
#'   }
#'   \item{call}{
#'     Matched function call.
#'   }
#' }
#'
#' @references
#' de Finetti, B. (1937). La prévision: Ses lois logiques, ses sources
#' subjectives. *Annales de l'Institut Henri Poincaré*, *7*(1), 1--68.
#'
#' @examples
#' dat <- data.frame(
#'   id = paste0("P", 1:6),
#'   score_pre = c(2, 3, 4, 2, 5, 3),
#'   score_post = c(1, 3, 5, 2, 3, 4)
#' )
#'
#' fit <- bruno(
#'   data = dat,
#'   id = id,
#'   pre = score_pre,
#'   post = score_post,
#'   event = post <= pre,
#'   mu0 = 0.50,
#'   tau = 10
#' )
#'
#' fit
#'
#' event_status <- c(1, 1, 0, 1, 1, 0)
#' participant <- paste0("P", 1:6)
#'
#' fit_direct <- bruno(
#'   id = participant,
#'   event = event_status,
#'   mu0 = 0.50,
#'   tau = 10
#' )
#'
#' fit_direct
#'
#' @export
bruno <- function(data = NULL,
                    id = NULL,
                    pre = NULL,
                    post = NULL,
                    event,
                    mu0,
                    tau,
                    group = NULL,
                    future_n = 1L) {

  call <- match.call()

  event_missing <- missing(event)
  mu0_missing <- missing(mu0)
  tau_missing <- missing(tau)

  id_expr <- substitute(id)
  pre_expr <- substitute(pre)
  post_expr <- substitute(post)
  event_expr <- if (event_missing) NULL else substitute(event)
  group_expr <- substitute(group)

  data_supplied <- !is.null(data)
  id_supplied <- !identical(id_expr, quote(NULL))
  pre_supplied <- !identical(pre_expr, quote(NULL))
  post_supplied <- !identical(post_expr, quote(NULL))
  group_supplied <- !identical(group_expr, quote(NULL))

  mu0_value <- if (mu0_missing) NULL else mu0
  tau_value <- if (tau_missing) NULL else tau

  validate_bruno_input(
    data = data,
    data_supplied = data_supplied,
    id_expr = id_expr,
    id_supplied = id_supplied,
    pre_expr = pre_expr,
    pre_supplied = pre_supplied,
    post_expr = post_expr,
    post_supplied = post_supplied,
    event_missing = event_missing,
    mu0 = mu0_value,
    mu0_missing = mu0_missing,
    tau = tau_value,
    tau_missing = tau_missing,
    group_expr = group_expr,
    group_supplied = group_supplied,
    future_n = future_n
  )

  prepared <- prepare_bruno_data(
    data = data,
    data_supplied = data_supplied,
    id_expr = id_expr,
    id_supplied = id_supplied,
    pre_expr = pre_expr,
    pre_supplied = pre_supplied,
    post_expr = post_expr,
    post_supplied = post_supplied,
    event_expr = event_expr,
    group_expr = group_expr,
    group_supplied = group_supplied,
    envir = parent.frame()
  )

  if (prepared$grouped) {

    validate_initial_assessment(
      mu0 = mu0_value,
      tau = tau_value,
      groups = prepared$group_levels
    )

    initial <- resolve_initial_assessment(
      mu0 = mu0_value,
      tau = tau_value,
      groups = prepared$group_levels
    )

    results <- lapply(
      prepared$group_levels,
      function(g) {

        idx <- prepared$included &
          !is.na(prepared$group) &
          prepared$group == g

        event_g <- prepared$event[idx]

        n <- length(event_g)
        s <- sum(event_g)

        assessment_g <- initial[
          initial$group == g,
          ,
          drop = FALSE
        ]

        pars <- beta_parameters(
          mu0 = assessment_g$mu0,
          tau = assessment_g$tau,
          s = s,
          n = n
        )

        predictive <- beta_binomial(
          future_n = future_n,
          alpha = pars$conditional_alpha,
          beta = pars$conditional_beta
        )

        next_probability <-
          pars$conditional_alpha /
          (pars$conditional_alpha + pars$conditional_beta)

        list(
          group = g,
          n = n,
          s = s,
          observed_rate = s / n,
          mu0 = assessment_g$mu0,
          tau = assessment_g$tau,
          alpha = pars$alpha,
          beta = pars$beta,
          conditional_alpha = pars$conditional_alpha,
          conditional_beta = pars$conditional_beta,
          next_probability = next_probability,
          predictive = predictive
        )
      }
    )

    names(results) <- prepared$group_levels

  } else {

    validate_initial_assessment(
      mu0 = mu0_value,
      tau = tau_value
    )

    event_used <- prepared$event[prepared$included]

    n <- length(event_used)
    s <- sum(event_used)

    pars <- beta_parameters(
      mu0 = mu0_value,
      tau = tau_value,
      s = s,
      n = n
    )

    predictive <- beta_binomial(
      future_n = future_n,
      alpha = pars$conditional_alpha,
      beta = pars$conditional_beta
    )

    next_probability <-
      pars$conditional_alpha /
      (pars$conditional_alpha + pars$conditional_beta)

    results <- list(
      list(
        group = NULL,
        n = n,
        s = s,
        observed_rate = s / n,
        mu0 = mu0_value,
        tau = tau_value,
        alpha = pars$alpha,
        beta = pars$beta,
        conditional_alpha = pars$conditional_alpha,
        conditional_beta = pars$conditional_beta,
        next_probability = next_probability,
        predictive = predictive
      )
    )
  }

  out <- list(
    results = results,
    events = prepared$events,
    future_n = future_n,
    n_total = prepared$n_total,
    n_analyzed = prepared$n_analyzed,
    n_excluded = prepared$n_excluded,
    grouped = prepared$grouped,
    call = call
  )

  class(out) <- "bruno_fit"

  out
}

