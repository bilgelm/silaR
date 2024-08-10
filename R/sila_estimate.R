#' Generate subject-level estimates of time to threshold based on the modeled
#' value vs. time function
#'
#' @param tsila Tibble output of SILA containing the modeled value vs. time
#'              function
#' @param df A tibble with the following columns: subid, age, val
#' @param align_event Which observation(s) to use within a subject to estimate
#'                    subject-level age of onset and duration of positivity.
#'                    Can be "first" or "last" (default).
#' @param extrap_years Data within extrap_years is used to extrapolate SILA
#'                     estimates when observations fall beyond the modeled
#'                     range. Linear regression is applied to the modeled
#'                     relationship between value and time. Default is 3.
#' @param truncate_aget0 Boolean (default is TRUE) specifying if subject-level
#'                       estimates should be truncated such that the lowest
#'                       duration estimate is set to the earliest modeled time
#'                       point on the SILA curve.
#'
#' @return A tibble containing the following columns:
#'         - subid: subject identifier
#'         - age: age in years at observation
#'         - val: observed biomarker value
#'         - minage: minimum observed age of subject
#'         - maxage: maximum observed age of subject
#'         - valt0: time point defining zero time
#'         - ageref: age at the reference observation
#'         - dtageref: time from observation to reference
#'         - estval: SILA-estimated value at observation
#'         - estage0: SILA-estimated age the subject will cross the threshold
#'         - estdtt0: SILA-estimated time from threshold
#'         - estresid: Estimated biomarker residual
#'         - estpos: Boolean indicating if SILA estimate is above threshold
#'         - aevent: Same as align_event
#'         - extrapyrs: Same as extrap_years
#'         - truncated: Boolean indicating if estaget0 and estdtt0 are truncated
#' @importFrom dplyr %>%
#' @importFrom tidyr nest unnest
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
#' illa_res <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
#' sila_estimate(illa_res$tout, df)
#' sila_estimate(illa_res$tout, df, align_event = "first")
sila_estimate <- function(
    tsila, df, align_event = "last", extrap_years = 3, truncate_aget0 = TRUE) {
  # Create extrapolated model
  md1 <- stats::lm(
    adtime ~ val,
    tsila %>%
      dplyr::filter(adtime > max(adtime) - extrap_years)
  )
  md2 <- stats::lm(
    adtime ~ val,
    tsila %>%
      dplyr::filter(adtime < min(adtime) + extrap_years)
  )

  rng <- diff(range(df$val))
  minval <- min(df$val) - rng
  maxval <- max(df$val) + rng
  left_adtime <- stats::predict(md2, newdata = tibble(val = minval))
  right_adtime <- stats::predict(md1, newdata = tibble(val = maxval))
  extended_adtime <- c(left_adtime, tsila$adtime, right_adtime)
  extended_val <- c(minval, tsila$val, maxval)

  val_to_adtime <- stats::approxfun(extended_val, extended_adtime)

  # if adtime is outside of the (extended) bounds of the observed data, truncate
  # the estimated value at the bounds of the (extended) observed data values
  adtime_to_val <- stats::approxfun(extended_adtime, extended_val, rule = 2)

  valt0 <- tsila$val[tsila$adtime == 0]
  tout <- df %>%
    dplyr::arrange(subid, age) %>%
    dplyr::group_by(subid) %>%
    dplyr::mutate(
      minage = min(age),
      maxage = max(age),
      valt0 = valt0
    ) %>%
    dplyr::ungroup() %>%
    nest(.by = subid, .key = "data")

  if (align_event %in% c("first", "last")) {
    tout <- tout %>%
      dplyr::mutate(
        ageref = map_dbl(
          data, ~ getFromNamespace(align_event, "dplyr")(.x$age)
        ),
        chronref = map_dbl(
          data,
          ~ val_to_adtime(getFromNamespace(align_event, "dplyr")(.x$val))
        )
      )
  } else {
    tout <- tout %>%
      dplyr::mutate(
        ageref = map_dbl(data, ~ mean(.x$age)),
        chronref = map_dbl(
          data,
          ~ stats::coef(
            stats::nls(
              val ~ adtime_to_val(age + shift),
              data = .x,
              start = list(
                shift = val_to_adtime(mean(.x$val)) - mean(.x$age)
              ),
              control = stats::nls.control(
                maxiter = 1000, tol = 0.04, minFactor = 1e-16
              )
            )
          )["shift"]
        ) + ageref
      )
  }
  tout <- tout %>%
    unnest(cols = c(data)) %>%
    dplyr::mutate(
      dtageref = age - ageref,
      estdtt0 = chronref + dtageref,
      estval = adtime_to_val(estdtt0),
      estaget0 = ageref - chronref,
      estresid = val - estval,
      estpos = 1 * (estval >= valt0),
      aevent = align_event,
      extrapyrs = extrap_years
    )

  # This is an option to restrict the estaget0 such that at a person's oldest
  # age they cannot have a estaget0 less than minimum ILLA value time to A+
  if (truncate_aget0) {
    tout <- tout %>%
      dplyr::arrange(subid, age) %>%
      dplyr::group_by(subid) %>%
      dplyr::mutate(
        dtshift = min(tsila$adtime) - dplyr::last(estdtt0),
        truncated = 1 * (dtshift > 0),
        estaget0 = estaget0 - max(0, dtshift),
        estdtt0 = estdtt0 + max(0, dtshift)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dtshift)
  } else {
    tout$realigned <- 0
  }

  tout %>%
    dplyr::select(-chronref) %>%
    dplyr::relocate(dtageref, .after = ageref) %>%
    dplyr::relocate(estdtt0, .after = estaget0)
}
