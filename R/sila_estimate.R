#' Generate subject-level estimates of time to threshold based on the modeled
#' value vs. time function
#'
#' @param sila_res output of illa, an object of class "sila"
#' @inheritParams illa
#' @param align_event One of: "all", "first", "last", "mean". "all" uses all
#' visits per subject to determine their time shifts. "first" uses the first
#' visit per subject only. "last" uses the last visit per subject only. "mean"
#' uses the mean value and mean age within each subject to estimate the shift.
#' @param adtime_limits a vector with two elements specifying the min and max
#' allowed values for adjusted time
#'
#' @return A tibble containing the following columns:
#' * `subid`: subject identifier
#' * `age`: age in years at observation
#' * `val`: observed biomarker value
#' * `shift`: estimated time shift
#' * `adtime`: estimated adjusted time, given by age + shift
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' set.seed(42)
#' df <- tibble::tibble(
#'   subid = c(
#'     1, 1, 1,
#'     2, 2, 2, 2, 2, 2
#'   ),
#'   age = c(
#'     seq(from = 50, to = 70, length.out = 3),
#'     seq(from = 50, to = 70, length.out = 6)
#'   ),
#'   val = c(
#'     2, 4, 6,
#'     2, 4, 6, 8, 10, 12
#'   ) + stats::rnorm(3 + 6, mean = 0, sd = .1)
#' )
#' res <- illa(df, val0 = 2)
#' sila_estimate(res)
sila_estimate <- function(
    sila_res, df = NULL, align_event = "all", adtime_limits = c(-50, 50)) {
  if (is.null(df)) {
    df <- sila_res$df
  }

  if (sila_res$is_decreasing) {
    adtime_limits <- c(max(adtime_limits), min(adtime_limits))
  } else {
    adtime_limits <- c(min(adtime_limits), max(adtime_limits))
  }
  adtimes <- seq(adtime_limits[1], adtime_limits[2], length.out = 1000)
  vals <- sila_res$adtime_to_val(adtimes)

  tout <- df %>%
    dplyr::arrange(.data$subid, .data$age) %>%
    tidyr::nest(.by = subid, .key = "data") %>%
    dplyr::mutate(shift = NA_real_)

  val_to_adtime_quick <- stats::approxfun(vals, adtimes, rule = 2)
  adtime_to_val_quick <- stats::approxfun(adtimes, vals, rule = 2) # nolint

  align_event_namespace <- switch(align_event,
    first = "dplyr",
    last = "dplyr",
    mean = "base",
    "unknown"
  )

  for (rowid in seq_len(nrow(tout))) {
    .x <- tout$data[[rowid]]
    if (max(.x$val) < vals[2]) {
      tout$shift[rowid] <- adtime_limits[1] -
        ifelse(sila_res$is_decreasing, max(.x$age), min(.x$age))
    } else if (min(.x$val) > vals[length(vals) - 1]) {
      tout$shift[rowid] <- adtime_limits[2] -
        ifelse(sila_res$is_decreasing, min(.x$age), max(.x$age))
    } else {
      if (align_event %in% c("first", "last", "mean", "median")) {
        tout$shift[rowid] <- val_to_adtime_quick(
          utils::getFromNamespace(align_event, align_event_namespace)(.x$val)
        ) - utils::getFromNamespace(align_event, align_event_namespace)(.x$age)
      } else {
        tout$shift[rowid] <- tryCatch(
          {
            stats::coef(
              stats::nls(
                val ~ adtime_to_val_quick(age + shift),
                data = .x,
                start = list(
                  shift = mean(val_to_adtime_quick(.x$val) - .x$age)
                ),
                control = stats::nls.control(
                  maxiter = 1000, tol = 0.04, minFactor = 1e-16
                ),
                algorithm = "port",
                lower = min(adtime_limits) - min(.x$age),
                upper = max(adtime_limits) - max(.x$age)
              )
            )["shift"]
          },
          error = function(cond) {
            stats::coef(
              nls2::nls2(
                val ~ adtime_to_val_quick(age + shift),
                data = .x,
                start = list(
                  shift = pmin(
                    pmax(
                      range(val_to_adtime_quick(.x$val) - .x$age),
                      min(adtime_limits) - min(.x$age)
                    ),
                    max(adtime_limits) - max(.x$age)
                  )
                ),
                control = stats::nls.control(maxiter = 100),
                algorithm = "grid-search"
              )
            )["shift"]
          }
        )
      }
    }
  }

  tout <- tout %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::mutate(
      adtime = .data$age + .data$shift
    )

  tout
}
