#' Prepare Data for bruno
#'
#' Internal preprocessing utility for [bruno()].
#'
#' The function:
#' - evaluates the declared event exactly once;
#' - validates the resulting binary classification;
#' - preserves supplied case identifiers or creates a row-based case index;
#' - retains paired measurements when they were used to define the event;
#' - retains predictive-class membership when supplied;
#' - identifies the cases contributing to recurrence calculations;
#' - preserves excluded cases in a case-level audit data frame.
#'
#' No probabilistic calculation is performed by this function.
#'
#' @param data Optional data frame supplied to [bruno()].
#' @param data_supplied Logical value indicating whether `data` was supplied.
#' @param id_expr Unevaluated expression supplied to `id`.
#' @param id_supplied Logical value indicating whether `id` was supplied.
#' @param pre_expr Unevaluated expression supplied to `pre`.
#' @param pre_supplied Logical value indicating whether `pre` was supplied.
#' @param post_expr Unevaluated expression supplied to `post`.
#' @param post_supplied Logical value indicating whether `post` was supplied.
#' @param event_expr Unevaluated expression defining or supplying the event.
#' @param group_expr Unevaluated expression supplied to `group`.
#' @param group_supplied Logical value indicating whether `group` was supplied.
#' @param envir Calling environment in which directly supplied vectors and
#'   objects referenced by the event expression are evaluated.
#'
#' @return A list containing:
#' \describe{
#'   \item{event}{
#'     Logical event vector for all presented cases, including `NA` where
#'     the event could not be determined.
#'   }
#'   \item{included}{
#'     Logical vector identifying cases contributing to the recurrence
#'     calculation.
#'   }
#'   \item{group}{
#'     Character vector containing predictive-class membership, or `NULL`
#'     when no classes were supplied.
#'   }
#'   \item{group_levels}{
#'     Character vector containing predictive classes represented among
#'     analysed cases, or `NULL` for an ungrouped analysis.
#'   }
#'   \item{grouped}{
#'     Logical value indicating whether predictive classes were supplied.
#'   }
#'   \item{events}{
#'     Case-level audit data frame.
#'   }
#'   \item{n_total}{
#'     Number of cases presented for classification.
#'   }
#'   \item{n_analyzed}{
#'     Number of cases contributing to recurrence calculations.
#'   }
#'   \item{n_excluded}{
#'     Number of excluded cases.
#'   }
#' }
#'
#' @noRd
prepare_bruno_data <- function(data,
                                 data_supplied,
                                 id_expr,
                                 id_supplied,
                                 pre_expr,
                                 pre_supplied,
                                 post_expr,
                                 post_supplied,
                                 event_expr,
                                 group_expr,
                                 group_supplied,
                                 envir) {

  paired_input <- pre_supplied && post_supplied

  pre_values <- NULL
  post_values <- NULL

  if (paired_input) {

    pre_col <- as.character(pre_expr)
    post_col <- as.character(post_expr)

    pre_values <- data[[pre_col]]
    post_values <- data[[post_col]]

    event_environment <- list2env(
      list(
        pre = pre_values,
        post = post_values
      ),
      parent = envir
    )

    event_raw <- eval(
      event_expr,
      envir = event_environment
    )

    n_cases <- nrow(data)

  } else if (data_supplied) {

    event_raw <- eval(
      event_expr,
      envir = data,
      enclos = envir
    )

    n_cases <- nrow(data)

  } else {

    event_raw <- eval(
      event_expr,
      envir = envir
    )

    n_cases <- length(event_raw)
  }

  validate_event_vector(event_raw)
  validate_event_length(event_raw, n_cases)

  event <- if (is.logical(event_raw)) {
    event_raw
  } else {
    as.logical(event_raw)
  }

  if (id_supplied) {

    if (data_supplied) {
      id <- data[[as.character(id_expr)]]
    } else {
      id <- eval(id_expr, envir = envir)
    }

    validate_id_vector(
      id = id,
      n = n_cases
    )

  } else {

    id <- seq_len(n_cases)
  }

  group <- NULL

  if (group_supplied) {

    if (data_supplied) {
      group_raw <- data[[as.character(group_expr)]]
    } else {
      group_raw <- eval(group_expr, envir = envir)
    }

    validate_group_vector(
      group = group_raw,
      n = n_cases
    )

    group <- as.character(group_raw)
  }

  included <- !is.na(event)

  if (group_supplied) {
    included <- included & !is.na(group)
  }

  if (!any(included)) {
    stop(
      "No case contains an analysable event.",
      call. = FALSE
    )
  }

  if (group_supplied) {
    group_levels <- unique(group[included])
  } else {
    group_levels <- NULL
  }

  if (id_supplied) {

    events <- data.frame(
      id = id,
      stringsAsFactors = FALSE
    )

  } else {

    events <- data.frame(
      case = id
    )
  }

  if (paired_input) {

    events$pre <- pre_values
    events$post <- post_values
  }

  if (group_supplied) {
    events$group <- group
  }

  events$event <- event
  events$included <- included

  n_total <- n_cases
  n_analyzed <- sum(included)
  n_excluded <- n_total - n_analyzed

  list(
    event = event,
    included = included,
    group = group,
    group_levels = group_levels,
    grouped = group_supplied,
    events = events,
    n_total = n_total,
    n_analyzed = n_analyzed,
    n_excluded = n_excluded
  )
}

