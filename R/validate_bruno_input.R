#' Validate Inputs for bruno
#'
#' Internal validation utility for [bruno()].
#'
#' The function checks:
#' - presence of required arguments;
#' - compatibility of the two supported input modes;
#' - data-frame and column specifications;
#' - domains of the initial assessment parameters;
#' - validity of the prespecified future sample size.
#'
#' Event values, identifiers, grouping values, and correspondence between
#' class-specific initial assessments and analysed predictive classes are
#' validated after the relevant vectors have been obtained.
#'
#' @param data Object supplied to `data`.
#' @param data_supplied Logical value indicating whether `data` was supplied.
#' @param id_expr Unevaluated expression supplied to `id`.
#' @param id_supplied Logical value indicating whether `id` was supplied.
#' @param pre_expr Unevaluated expression supplied to `pre`.
#' @param pre_supplied Logical value indicating whether `pre` was supplied.
#' @param post_expr Unevaluated expression supplied to `post`.
#' @param post_supplied Logical value indicating whether `post` was supplied.
#' @param event_missing Logical value indicating whether `event` was omitted.
#' @param mu0 Initial event probability supplied by the user, or `NULL` when
#'   omitted.
#' @param mu0_missing Logical value indicating whether `mu0` was omitted.
#' @param tau Concentration parameter supplied by the user, or `NULL` when
#'   omitted.
#' @param tau_missing Logical value indicating whether `tau` was omitted.
#' @param group_expr Unevaluated expression supplied to `group`.
#' @param group_supplied Logical value indicating whether `group` was supplied.
#' @param future_n Prespecified future sample size.
#'
#' @return Invisibly returns `TRUE` when the structural input is valid.
#'
#' @noRd
validate_bruno_input <- function(data,
                                   data_supplied,
                                   id_expr,
                                   id_supplied,
                                   pre_expr,
                                   pre_supplied,
                                   post_expr,
                                   post_supplied,
                                   event_missing,
                                   mu0,
                                   mu0_missing,
                                   tau,
                                   tau_missing,
                                   group_expr,
                                   group_supplied,
                                   future_n) {

  if (event_missing) {
    stop("`event` must be supplied.", call. = FALSE)
  }

  if (mu0_missing) {
    stop("`mu0` must be supplied.", call. = FALSE)
  }

  if (tau_missing) {
    stop("`tau` must be supplied.", call. = FALSE)
  }

  if (data_supplied && !is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (xor(pre_supplied, post_supplied)) {
    stop(
      "`pre` and `post` must either both be supplied or both be omitted.",
      call. = FALSE
    )
  }

  paired_input <- pre_supplied && post_supplied

  if (paired_input && !data_supplied) {
    stop(
      "`data` must be supplied when `pre` and `post` are used.",
      call. = FALSE
    )
  }

  if (data_supplied && paired_input) {

    validate_column_expression(
      expr = pre_expr,
      data = data,
      argument = "pre"
    )

    validate_column_expression(
      expr = post_expr,
      data = data,
      argument = "post"
    )
  }

  if (data_supplied && id_supplied) {
    validate_column_expression(
      expr = id_expr,
      data = data,
      argument = "id"
    )
  }

  if (data_supplied && group_supplied) {
    validate_column_expression(
      expr = group_expr,
      data = data,
      argument = "group"
    )
  }

  validate_mu0(mu0)
  validate_tau(tau)
  validate_future_n(future_n)

  invisible(TRUE)
}


#' Validate a Column Reference
#'
#' Checks that an argument identifying a variable in `data` is an unquoted
#' column name and that the corresponding column exists.
#'
#' @param expr Unevaluated column expression.
#' @param data Data frame containing the referenced column.
#' @param argument Character string naming the argument being checked.
#'
#' @return Invisibly returns `TRUE` when the column reference is valid.
#'
#' @noRd
validate_column_expression <- function(expr,
                                       data,
                                       argument) {

  if (!is.symbol(expr)) {
    stop(
      sprintf(
        "`%s` must be an unquoted column name when `data` is supplied.",
        argument
      ),
      call. = FALSE
    )
  }

  column <- as.character(expr)

  if (!column %in% names(data)) {
    stop(
      sprintf(
        "Column `%s` supplied to `%s` was not found in `data`.",
        column,
        argument
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate an Event Vector
#'
#' Checks that an evaluated event definition is a one-dimensional logical
#' vector or a numeric vector containing only `0` and `1`, apart from
#' missing values.
#'
#' Missing values are permitted because cases for which the declared event
#' cannot be determined are retained in the audit information but excluded
#' from recurrence calculations.
#'
#' @param event Evaluated event vector.
#'
#' @return Invisibly returns `TRUE` when `event` is valid.
#'
#' @noRd
validate_event_vector <- function(event) {

  if (is.null(event)) {
    stop("`event` must not be NULL.", call. = FALSE)
  }

  if (!is.atomic(event) ||
      is.object(event) && !is.factor(event) && !is.logical(event) &&
      !is.numeric(event) ||
      !is.null(dim(event))) {
    stop(
      "`event` must evaluate to a one-dimensional logical or binary vector.",
      call. = FALSE
    )
  }

  if (length(event) == 0L) {
    stop(
      "`event` must contain at least one case.",
      call. = FALSE
    )
  }

  if (is.logical(event)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(event)) {
    stop(
      "`event` must be logical or numeric with values 0 and 1.",
      call. = FALSE
    )
  }

  observed <- event[!is.na(event)]

  if (length(observed) > 0L &&
      any(!is.finite(observed))) {
    stop(
      "`event` must not contain non-finite values.",
      call. = FALSE
    )
  }

  if (length(observed) > 0L &&
      any(!observed %in% c(0, 1))) {
    stop(
      "`event` must contain only 0 and 1, apart from missing values.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate the Number of Event Values
#'
#' Checks that an evaluated event vector has the expected number of cases.
#'
#' @param event Evaluated event vector.
#' @param n Expected number of cases.
#'
#' @return Invisibly returns `TRUE` when the lengths agree.
#'
#' @noRd
validate_event_length <- function(event,
                                  n) {

  if (length(event) != n) {
    stop(
      "`event` must define exactly one value for each case.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate Case Identifiers
#'
#' Checks that case identifiers are one-dimensional, have the expected
#' length, contain no missing values, and uniquely identify cases.
#'
#' Identifiers are retained for traceability and do not enter the
#' predictive calculation.
#'
#' @param id Identifier vector.
#' @param n Expected number of cases.
#'
#' @return Invisibly returns `TRUE` when `id` is valid.
#'
#' @noRd
validate_id_vector <- function(id,
                               n) {

  if (!is.atomic(id) || !is.null(dim(id))) {
    stop(
      "`id` must be a one-dimensional vector.",
      call. = FALSE
    )
  }

  if (length(id) != n) {
    stop(
      "`id` must contain exactly one identifier for each case.",
      call. = FALSE
    )
  }

  if (anyNA(id)) {
    stop(
      "`id` must not contain missing values.",
      call. = FALSE
    )
  }

  if (anyDuplicated(id)) {
    stop(
      "`id` must uniquely identify cases.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate Predictive-Class Membership
#'
#' Checks that a grouping vector is one-dimensional, has the expected
#' length, and contains at least one observed predictive-class label.
#'
#' Missing class labels are permitted so that the corresponding cases can
#' remain visible in the audit information. Such cases do not contribute
#' to class-specific recurrence calculations.
#'
#' @param group Grouping vector.
#' @param n Expected number of cases.
#'
#' @return Invisibly returns `TRUE` when `group` is valid.
#'
#' @noRd
validate_group_vector <- function(group,
                                  n) {

  if (!is.atomic(group) || !is.null(dim(group))) {
    stop(
      "`group` must be a one-dimensional vector.",
      call. = FALSE
    )
  }

  if (length(group) != n) {
    stop(
      "`group` must contain exactly one value for each case.",
      call. = FALSE
    )
  }

  if (all(is.na(group))) {
    stop(
      "`group` must contain at least one non-missing predictive class.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate the Initial Event Probability
#'
#' Checks that `mu0` contains finite numeric values strictly between `0`
#' and `1`.
#'
#' @param mu0 Initial event probability or probabilities.
#'
#' @return Invisibly returns `TRUE` when `mu0` is valid.
#'
#' @noRd
validate_mu0 <- function(mu0) {

  if (!is.numeric(mu0) ||
      length(mu0) == 0L ||
      anyNA(mu0) ||
      any(!is.finite(mu0))) {
    stop(
      "`mu0` must contain finite numeric values.",
      call. = FALSE
    )
  }

  if (any(mu0 <= 0 | mu0 >= 1)) {
    stop(
      "All values of `mu0` must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate the Beta Concentration Parameter
#'
#' Checks that `tau` contains finite positive numeric values.
#'
#' @param tau Concentration parameter or parameters of the initial beta
#'   specification.
#'
#' @return Invisibly returns `TRUE` when `tau` is valid.
#'
#' @noRd
validate_tau <- function(tau) {

  if (!is.numeric(tau) ||
      length(tau) == 0L ||
      anyNA(tau) ||
      any(!is.finite(tau))) {
    stop(
      "`tau` must contain finite numeric values.",
      call. = FALSE
    )
  }

  if (any(tau <= 0)) {
    stop(
      "All values of `tau` must be strictly positive.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate the Initial Assessment
#'
#' Checks the domains of `mu0` and `tau` and their correspondence with the
#' analysed predictive classes.
#'
#' Without grouping, `mu0` and `tau` must each contain one value. With
#' grouping, each may contain either one common value or one named value for
#' every analysed predictive class.
#'
#' @param mu0 Initial event probability or probabilities.
#' @param tau Concentration parameter or parameters.
#' @param groups Optional character vector containing analysed predictive
#'   classes.
#'
#' @return Invisibly returns `TRUE` when the initial assessment is valid.
#'
#' @noRd
validate_initial_assessment <- function(mu0,
                                        tau,
                                        groups = NULL) {

  validate_mu0(mu0)
  validate_tau(tau)

  if (is.null(groups)) {

    if (length(mu0) != 1L) {
      stop(
        "`mu0` must be a single value when `group` is not supplied.",
        call. = FALSE
      )
    }

    if (length(tau) != 1L) {
      stop(
        "`tau` must be a single value when `group` is not supplied.",
        call. = FALSE
      )
    }

    return(invisible(TRUE))
  }

  if (length(groups) == 0L) {
    stop(
      "No predictive class contains an analysable event.",
      call. = FALSE
    )
  }

  if (anyNA(groups) || anyDuplicated(groups)) {
    stop(
      "Analysed predictive-class labels must be non-missing and unique.",
      call. = FALSE
    )
  }

  validate_group_parameter(
    x = mu0,
    groups = groups,
    argument = "mu0"
  )

  validate_group_parameter(
    x = tau,
    groups = groups,
    argument = "tau"
  )

  invisible(TRUE)
}


#' Validate a Predictive-Class Parameter
#'
#' Checks that a class-specific initial-assessment parameter is either a
#' scalar common to all classes or a named vector whose names correspond
#' exactly to the analysed predictive classes.
#'
#' @param x Numeric parameter vector.
#' @param groups Character vector containing analysed predictive classes.
#' @param argument Character string naming the parameter being checked.
#'
#' @return Invisibly returns `TRUE` when the parameter is valid.
#'
#' @noRd
validate_group_parameter <- function(x,
                                     groups,
                                     argument) {

  if (length(x) == 1L) {
    return(invisible(TRUE))
  }

  nm <- names(x)

  if (is.null(nm) ||
      length(nm) != length(x) ||
      anyNA(nm) ||
      any(nm == "")) {
    stop(
      sprintf(
        paste0(
          "When `%s` contains multiple values, it must be a named ",
          "vector with one value for each analysed predictive class."
        ),
        argument
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(nm)) {
    stop(
      sprintf(
        "Names of `%s` must be unique.",
        argument
      ),
      call. = FALSE
    )
  }

  if (length(x) != length(groups) ||
      !setequal(nm, groups)) {
    stop(
      sprintf(
        "Names of `%s` must match the analysed predictive classes exactly.",
        argument
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate the Future Sample Size
#'
#' Checks that `future_n` is a single finite positive integer.
#'
#' @param future_n Prespecified number of future cases.
#'
#' @return Invisibly returns `TRUE` when `future_n` is valid.
#'
#' @noRd
validate_future_n <- function(future_n) {

  if (!is.numeric(future_n) ||
      length(future_n) != 1L ||
      is.na(future_n) ||
      !is.finite(future_n) ||
      future_n < 1 ||
      future_n != floor(future_n)) {
    stop(
      "`future_n` must be a single finite positive integer.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
