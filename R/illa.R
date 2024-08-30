#' Estimate value vs. time curve from longitudinal data
#'
#' @param df A `tibble` with the following columns: `subid`, `age`, `val`
#' * `subid` is a numeric subject identifier
#' * `age` is age in years at each observation (cannot be repeated within
#' subject)
#' * `value` is the biomarker value to be modeled
#' @param val0 Anchor biomarker value that corresponds to `t = 0`
#'
#' @return `illa` returns an object of class "sila" containg:
#' * `val0` Anchor biomarker value that corresponds to `t = 0`
#' * `adtime_to_val` function representing g : val = g(adtime)
#' * `val_to_rate` function representing f : rate = f(val)
#' * `tdrs` a `tibble` with sampled rates at query points
#' * `is_decreasing` TRUE if biomarker value globally decreases with age
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom purrr map_dbl
#' @importFrom tibble tibble
#' @importFrom deSolve ode
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
#' illa(df, val0 = 2)
illa <- function(df, val0) {
  # basic checks
  stopifnot(
    inherits(df, "data.frame"), # df should be a tibble or data.frame
    all(c("subid", "age", "val") %in% colnames(df)), # required columns
    !any(is.na(df)), # missingness is not allowed
    !any( # age should not be duplicated within subject
      df %>%
        dplyr::group_by(.data$subid) %>%
        dplyr::mutate(dup_age = any(duplicated(.data$age))) %>%
        dplyr::pull(.data$dup_age)
    ),
    is.numeric(val0) & is.finite(val0) # val0 should be a finite number
  )

  is_decreasing <- check_decreasing(df)
  if (is_decreasing) {
    res_negatedval <- illa(df %>% dplyr::mutate(val = -.data$val), -val0)
    res <- structure(
      list(
        df = df,
        val0 = val0,
        adtime_to_val = function(x) -res_negatedval$adtime_to_val(x),
        val_to_rate = function(x) -res_negatedval$val_to_rate(-x),
        tdrs = res_negatedval$tdrs %>%
          dplyr::mutate(val = -.data$val, rate = -.data$rate),
        is_decreasing = is_decreasing
      ),
      class = "sila"
    )
    return(res)
  }

  t <- df %>%
    dplyr::arrange(.data$subid, .data$age) %>%
    dplyr::group_nest(.data$subid, .key = "data") %>%
    dplyr::mutate(
      min = map_dbl(.data$data, ~ min(.x$val)),
      max = map_dbl(.data$data, ~ max(.x$val)),
      nvis = map_dbl(.data$data, ~ nrow(.x)),
      lmfit = purrr::map(.data$data, ~ stats::lm(val ~ age, data = .x)),
      mrate = map_dbl(.data$lmfit, ~ stats::coef(.x)[["age"]]),
      mrate_se = map_dbl(
        .data$lmfit, ~ summary(.x)$coefficients["age", "Std. Error"]
      )
    ) %>%
    dplyr::select(-data)

  tmod <- t %>%
    dplyr::filter(.data$nvis > 1 & .data$mrate < 100)
  # TODO: this should be abs(mrate); need warnings that there are steep changes

  n_qval <- 150 # TODO: number of query values should be a function parameter
  # TODO: can query values be specified as
  # unique(c(t$min, t$max)) or a subset thereof?

  qval <- seq(
    from = min(df$val),
    to = max(df$val),
    length.out = n_qval + 1
  )

  tdrs <- tibble(val = qval) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # TODO: should comparisons include = ?
      ids = list((tmod$min < .data$val) & (tmod$max > .data$val)),
      tot = sum(.data$ids),
      rates = list(tmod$mrate[.data$ids]),
      vals = list(rep(.data$val, .data$tot)),
      # TODO: should weighting use SE of each rate estimate instead of nvis?
      # but there is a problem: SE is 0 for two pts
      rate = stats::weighted.mean(.data$rates, tmod$nvis[.data$ids]),
      ratestd = stats::sd(.data$rates), # TODO: sd should also be weighted
      npos = sum(.data$rates > 0),
      ci = 1.96 * .data$ratestd / sqrt(.data$tot)
    ) %>%
    dplyr::select(-ids) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rates, -vals) %>%
    dplyr::filter(!is.na(.data$rate))

  # NOTE: monotonically decreasing biomarkers need to be negated before this
  fit <- mgcv::gam(
    rate ~ s(val, bs = "cs"),
    data = tdrs,
    weights = tdrs$tot,
    family = stats::gaussian(link = "log") # this ensures positive smooth fun.
  )

  # ODE to solve: dy(t)/dt = f(y(t))
  # we want to solve for y(t), where t is the aligned time, i.e., chronicity
  # which is given by integral from 0 to t f(y(T)) dT
  # subject to initial condition y(t=0) = val0

  maxval <- max(tdrs$val)
  fitted_rate_maxval <- mgcv::predict.gam(
    fit,
    type = "response", newdata = tibble(val = max(tdrs$val), tot = 1)
  )

  minval <- min(tdrs$val)
  fitted_rate_minval <- mgcv::predict.gam(
    fit,
    type = "response", newdata = tibble(val = min(tdrs$val), tot = 1)
  )

  val_to_rate <- function(x) {
    below_minval <- x < minval
    above_maxval <- x > maxval
    idx <- !is.na(x) & (!below_minval) & (!above_maxval)
    res <- rep(NA, length(x))
    if (any(idx)) {
      res[idx] <- mgcv::predict.gam(
        fit,
        type = "response", newdata = tibble(val = x[idx], tot = 1)
      )
    }
    if (any(below_minval)) res[below_minval] <- fitted_rate_minval
    if (any(above_maxval)) res[above_maxval] <- fitted_rate_maxval
    res
  }

  f <- function(t, state, parameters) {
    # the derivative function to integrate
    list(val_to_rate(state[["val"]]))
  }

  adtime_to_val <- function(x) {
    # need to split pos and neg (neg must be decreasing order)
    x_pos <- sort(unique(x[x >= 0]))
    x_neg <- sort(unique(x[x < 0]), decreasing = TRUE)

    if (length(x_pos) > 0) {
      # the first time value must be 0 for the initial condition
      # integration forward in time
      x_pos_aug <- c(0, x_pos)
      y_pos <- ode(y = c(val = val0), times = x_pos_aug, func = f)[-1, "val"]
    } else {
      y_pos <- numeric(0)
    }

    if (length(x_neg) > 0) {
      # the first time value must be 0 for the initial condition
      # integration backward in time
      x_neg_aug <- c(0, x_neg)
      y_neg <- ode(y = c(val = val0), times = x_neg_aug, func = f)[-1, "val"]
    } else {
      y_neg <- numeric(0)
    }

    x_all <- c(x_neg, x_pos)
    y_all <- c(y_neg, y_pos)
    unname(y_all[match(x, x_all)])
  }

  structure(
    list(
      df = df,
      val0 = val0,
      adtime_to_val = adtime_to_val,
      val_to_rate = val_to_rate,
      tdrs = tdrs,
      is_decreasing = is_decreasing
    ),
    class = "sila"
  )
}

#' Determine if biomarker decreases with age and perform monotonicity checks
#'
#' @inheritParams illa
#'
#' @return TRUE if biomarker decreases with age overall
check_decreasing <- function(df) {
  # try to understand if we are dealing with a measure that increases or
  # decreases with age
  fit_lmer <- lme4::lmer(val ~ age + (1 | subid), data = df)
  decreasing_with_age <- lme4::fixef(fit_lmer)[["age"]] < 0

  # attempt to assess monotonicity
  try(
    {
      fit_gam <- mgcv::gam(val ~ s(age) + s(subid, bs = "re"), data = df)

      eps <- 1e-7
      newdf <- tibble(
        subid = 0,
        age = seq(min(df$age), max(df$age), length.out = 100),
        val = 0
      )
      x0 <- stats::predict(fit_gam, newdf, type = "lpmatrix")

      newdfeps_p <- newdf
      newdfeps_p$age <- newdfeps_p$age + eps

      x1 <- stats::predict(fit_gam, newdfeps_p, type = "lpmatrix")

      # finite difference approximation of first derivative
      # the design matrix
      xp <- (x1 - x0) / eps

      # first derivative
      fd_d1 <- xp %*% stats::coef(fit_gam)

      # check first derivative of the age smooth
      # if it has both -/+ values, display warning about monotonicity assumption
      most_slopes_neg <- mean(fd_d1 < 0) > 0.5

      if (decreasing_with_age != most_slopes_neg) {
        warning(
          paste(
            "Globally, the biomarker seems to be",
            ifelse(decreasing_with_age, "decreasing", "increasing"),
            "with age, but locally, most slopes with respect to age are",
            ifelse(most_slopes_neg, "negative.", "positive."),
            "SILA will assume a monotonically",
            ifelse(decreasing_with_age, "decreasing", "increasing"),
            "biomarker. However, it is likely that this biomarker is",
            "inappropriate to model with SILA due to the violation of the",
            "monotonicity assumption. Check results."
          ),
          call. = FALSE
        )
      } else if (any(fd_d1 < 0) & any(fd_d1 > 0)) {
        warning(
          paste(
            "Globally, the biomarker seems to be",
            ifelse(decreasing_with_age, "decreasing", "increasing"),
            "with age, but locally, it has some",
            ifelse(most_slopes_neg, "positive", "negative"),
            "slopes. The monotonicity assumption of SILA may be violated.",
            "Check results."
          ),
          call. = FALSE
        )
      }
    },
    silent = TRUE
  )

  decreasing_with_age
}
