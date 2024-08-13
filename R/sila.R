#' Sampled Iterative Local Approximation with smoothing
#'
#' @inheritParams illa
#' @param sk List of smoothing kernels
#'
#' @return A list containing two `tibble`s:
#' * `tsila` specifies the value vs. time curve:
#'   * `val` is the SILA-modeled
#' value
#'   * `time` is the SILA-modeled time resulting from numeric integration
#' (not anchored)
#'   * `adtime` is the anchored time such that `t = 0` corresponds to
#' `val0`
#'   * `mrate` is the mean sampled rate through the value
#'   * `sdrate` is the
#' mean sampled rate through the value
#'   * `nsubs` is the number of subjects with
#' observations that intersect the modeled value
#'   * `sdval` is an approximation of
#' the standard deviation of the value (calculated by propagating the rate
#' error)
#'   * `ci95` is an approximation of the 95% confidence interval of the
#' value (likely underestimated since it does not account for temporal
#' covariance across observations)
#' * `tdrs`
#' @importFrom dplyr %>%
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
#' sila_res <- sila(df, dt = 2, val0 = 2, maxi = 100)
sila <- function(df, dt, val0, maxi, sk = NULL) {
  t <- df %>%
    dplyr::arrange(subid, age) %>%
    dplyr::group_by(subid) %>%
    dplyr::mutate(
      ns = dplyr::n(), # for each subject get no. longitudinal observations
      idx = 1:dplyr::n() # for each subject create ordered observation numbers
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ns > 1) # remove cases without longitudinal data

  # Set up variables for A+/- ids and residual weighting
  # Emphasis will be on backwards prediction. Residuals weighted such that A+
  # and A- have equal say despite possible imbalance in the data.
  tmp <- t %>%
    dplyr::slice_max(age, n = 1, by = subid, with_ties = FALSE) %>%
    dplyr::pull(val)
  # ratio of positive:negative cases used to weight residuals when estimating
  # smoothing kernel
  resnorm <- sum(tmp >= val0) / sum(tmp < val0)
  idpos <- t$val > val0 # indices for biomarker negative cases
  idneg <- t$val <= val0 # indices for biomarker positive cases

  if (is.null(sk)) {
    sk <- seq(from = 0, to = 0.5, by = 0.05)
  }
  # identify the best smoothing kernel
  min_ssq <- .Machine$double.xmax
  tsila <- tdrs <- best_sk <- NA
  for (i in seq_along(sk)) {
    illa_res <- illa(df, dt, val0, maxi, sk[i])
    temp <- sila_estimate(
      illa_res$tout, df,
      align_event = "last", truncate_aget0 = FALSE
    )
    ssq <- sum(temp$estresid[idpos]^2) +
      ifelse(is.finite(resnorm), resnorm * sum(temp$estresid[idneg]^2), 0)
    if (ssq < min_ssq) {
      min_ssq <- ssq
      tsila <- illa_res$tout
      tdrs <- illa_res$tdrs
      best_sk <- sk[i]
    }
  }
  tdrs$skern <- best_sk

  list(tsila = tsila, tdrs = tdrs)
}
