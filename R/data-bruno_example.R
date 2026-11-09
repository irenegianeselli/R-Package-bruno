#' Constructed Example Data for bruno
#'
#' A small constructed data set for illustrating event classification,
#' missing-data handling, predictive classes, and case-level traceability
#' with [bruno()].
#'
#' The data are artificial and do not represent observations from an
#' empirical psychological, educational, clinical, or intervention study.
#' The measurement values and predictive-class labels have no substantive
#' interpretation beyond their use in package examples and tests.
#'
#' @format A data frame with 12 rows and 4 variables:
#' \describe{
#'   \item{id}{
#'     Character case identifier. Values are unique and non-missing.
#'   }
#'   \item{pre}{
#'     Integer pre-measurement value.
#'   }
#'   \item{post}{
#'     Integer post-measurement value. One value is missing so that an event
#'     depending on both `pre` and `post` can be non-determinable for that
#'     case.
#'   }
#'   \item{group}{
#'     Character predictive-class label. Two non-missing classes, `"A"` and
#'     `"B"`, are represented. One value is missing so that exclusion due to
#'     unavailable predictive-class membership can be illustrated separately
#'     from exclusion due to an indeterminate event.
#'   }
#' }
#'
#' @details
#' No event classification is stored in the data set. The event must be
#' declared explicitly by the user when [bruno()] is called.
#'
#' For example, the declaration
#'
#' `event = post <= pre`
#'
#' defines one possible binary measurement event for illustration. Under
#' that declaration, 8 events occur among 11 cases for which the event is
#' determinable in the ungrouped analysis.
#'
#' When `group` is also supplied, predictive class `"A"` contains 4 event
#' occurrences among 5 analysable cases and predictive class `"B"` contains
#' 3 event occurrences among 5 analysable cases. The case with missing
#' `group` is retained in the case-level audit information but does not
#' contribute to a class-specific recurrence calculation.
#'
#' These counts are consequences of the illustrative event declaration
#' above and are not intrinsic properties or substantive labels of the
#' measurement values.
#'
#' @examples
#' data(bruno_example)
#'
#' bruno_example
#'
#' fit <- bruno(
#'   data = bruno_example,
#'   id = id,
#'   pre = pre,
#'   post = post,
#'   event = post <= pre,
#'   mu0 = 0.50,
#'   tau = 10
#' )
#'
#' fit
#'
#' fit_grouped <- bruno(
#'   data = bruno_example,
#'   id = id,
#'   pre = pre,
#'   post = post,
#'   event = post <= pre,
#'   mu0 = 0.50,
#'   tau = 10,
#'   group = group
#' )
#'
#' fit_grouped
#'
#' @source Constructed solely for examples and software tests in `bruno`;
#'   no empirical observations are included.
#'
#' @name bruno_example
#' @docType data
#' @keywords datasets
NULL
