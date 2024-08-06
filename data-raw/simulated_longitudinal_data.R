#' Simulate longitudinal data to illustrate use of SILA
#'
#' A simple model that assumes slope = 0 for non-accumulators and a slope = 5/yr
#' above the positivity threshold of 21.
#'
#' @param n_neg number of participants with biomarker below threshold
#' @param n_pos number of participants with biomarker above threshold
#' @param n_vis number of longitudinal visits per participant
#'
#' @return a tibble with subid, age, and val
#' @export
#'
#' @examples
simulate_data <- function(n_neg = 130, n_pos = 70, n_vis = 3) {
  set.seed(42)

  # set up the initial subjects
  h <- TRUE
  while (h) {
    age_base_neg <- 65 + 15 * stats::runif(n_neg)
    age_base_pos <- 65 + 15 * stats::runif(n_pos)
    h <- stats::t.test(age_base_neg, age_base_pos)$p.value < 0.05
  }

  # simulate data for negative cases
  subid_neg <- 1:n_neg
  mneg <- 0
  sdneg <- 7
  vals_neg <- stats::rnorm(n_vis * n_neg, mneg, sdneg)

  subage_neg <- subval_neg <- rep(NA_real_, n_vis * n_neg)
  for (i in seq_along(subid_neg)) {
    idx_hi <- n_vis * i
    idx_low <- idx_hi - n_vis + 1
    subage_neg[idx_low:idx_hi] <- c(
      age_base_neg[i],
      age_base_neg[i] + stats::runif(n_vis - 1) - .5 + 2 * 1:(n_vis - 1)
    )
    subval_neg[idx_low:idx_hi] <- vals_neg[idx_low:idx_hi]
  }

  # simulate data for positive cases
  subid_pos <- n_neg + 1:n_pos
  val_base_pos <- 100 * stats::runif(n_pos) + 5
  sd_pos <- stats::rnorm(n_vis * n_pos, 0, sdneg)

  subage_pos <- subval_pos <- rep(NA_real_, n_vis * n_pos)
  for (i in seq_along(subid_pos)) {
    idx_hi <- n_vis * i
    idx_low <- idx_hi - n_vis + 1
    subage_pos[idx_low:idx_hi] <- c(
      age_base_pos[i],
      age_base_pos[i] + stats::runif(n_vis - 1) - .5 + 2 * 1:(n_vis - 1)
    )
    subtime <- subage_pos[idx_low:idx_hi] - age_base_pos[i]
    subval_pos[idx_low:idx_hi] <- subtime * 5 +
      val_base_pos[i] + sd_pos[idx_low:idx_hi]
  }

  # merge simulated positive and negative data into a table
  df <- tibble::tibble(
    subid = c(rep(subid_neg, each = n_vis), rep(subid_pos, each = n_vis)),
    age = c(subage_neg, subage_pos),
    val = c(subval_neg, subval_pos)
  )

  df
}

simulated_longitudinal_data <- simulate_data()

usethis::use_data(simulated_longitudinal_data, overwrite = TRUE)
