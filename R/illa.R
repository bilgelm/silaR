#' Estimate value vs. time curve from longitudinal data
#'
#' @param df A tibble with the following columns: subid, age, val
#' @param dt Time difference between queries
#' @param val0 Anchor value
#' @param maxi Maximum number of iterations
#' @param skern Span of the smoothing kernel
#'
#' @return A list containing two tibbles
#' @importFrom dplyr %>%
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @importFrom tibble tibble
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
#' illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
#' illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0.2)
illa <- function(df, dt, val0, maxi, skern) {
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
    dplyr::select(-.data$data)

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

  tdrs <- tibble(
    val = qval,
    skern = skern
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # TODO: should comparisons include = ?
      ids = list((tmod$min < .data$val) & (tmod$max > .data$val)),
      tot = sum(.data$ids),
      rates = list(tmod$mrate[.data$ids]),
      vals = list(rep(.data$val, .data$tot)),
      # TODO: should weighting use SE of each rate estimate instead of nvis?
      rate = stats::weighted.mean(rates, tmod$nvis[.data$ids]),
      ratestd = stats::sd(.data$rates), # TODO: sd should also be weighted
      npos = sum(.data$rates > 0),
      ci = 1.96 * .data$ratestd / sqrt(.data$tot)
    ) %>%
    dplyr::select(-.data$ids)

  rates <- unlist(tdrs$rates)
  vals <- unlist(tdrs$vals)

  tdrs <- tdrs %>%
    dplyr::select(-.data$rates, -.data$vals) %>%
    dplyr::filter(.data$tot >= 2)

  if (skern > 0) {
    fit <- stats::loess(rates ~ vals,
      degree = 2, span = skern,
      family = "symmetric",
      control = stats::loess.control(iterations = 5)
    )
    tdrs <- tdrs %>%
      dplyr::select(-.data$rate) %>%
      dplyr::left_join(
        tibble(val = vals, rate = stats::predict(fit)) %>%
          dplyr::distinct()
      )
    # TODO: ratestd and ci should also be updated
  }

  med_rate <- stats::median(tdrs$rate)

  # Perform iterative model
  # set initial conditions
  # use iterative process to go forward and backward through discretely
  # sampled rate data

  # TODO: can the integration be rewritten using cumsum or pracma::cumtrapz?

  # forward in time
  qval_cur <- mean(tdrs$val)
  valf <- c()
  rf <- c()
  sdf <- c()
  nf <- c()
  nif <- 0

  while ((qval_cur < max(tdrs$val)) && (nif < maxi)) {
    if ((med_rate < 0) && (qval_cur < min(tdrs$val))) break

    id <- which.min(abs(tdrs$val - qval_cur))

    # TODO: tdrs can be inspected up front for these and trimmed
    if ((tdrs$rate[id] <= 0) && (med_rate > 0)) break
    if ((tdrs$rate[id] >= 0) && (med_rate < 0)) break

    valf <- c(valf, qval_cur)
    rf <- c(rf, tdrs$rate[id])
    sdf <- c(sdf, tdrs$ratestd[id])
    nf <- c(nf, tdrs$tot[id])
    qval_cur <- tdrs$rate[id] * dt + qval_cur

    nif <- nif + 1
  }
  tf <- seq(0, by = dt, length.out = nif)

  # backward in time
  qval_cur <- mean(tdrs$val)
  valb <- c()
  rb <- c()
  sdb <- c()
  nb <- c()
  nib <- 0
  while ((qval_cur > min(qval)) && (nib < maxi)) {
    if ((med_rate < 0) && (qval_cur > max(tdrs$val))) break

    id <- which.min(abs(tdrs$val - qval_cur))

    # TODO: tdrs can be inspected up front for these and trimmed
    if ((tdrs$rate[id] < 0) && (med_rate > 0)) break
    if ((tdrs$rate[id] > 0) && (med_rate < 0)) break

    valb <- c(valb, qval_cur)
    rb <- c(rb, tdrs$rate[id])
    sdb <- c(sdb, tdrs$ratestd[id])
    nb <- c(nb, tdrs$tot[id])
    qval_cur <- -tdrs$rate[id] * dt + qval_cur

    nib <- nib + 1
  }
  tb <- -seq(0, by = dt, length.out = nib)

  tout <- tibble(
    val = c(rev(valb[-1]), valf),
    time = c(rev(tb[-1]), tf),
    mrate = c(rev(rb[-1]), rf),
    sdrate = c(rev(sdb[-1]), sdf),
    nsubs = c(rev(nb[-1]), nf)
  ) %>%
    dplyr::mutate(
      sdval = sqrt(
        (.data$mrate * dt * .data$sdrate / .data$mrate)^2 + (.05 * .data$val)^2
      ),
      ci95 = 1.96 * .data$sdval / sqrt(.data$nsubs)
    )
  id0 <- which.min(abs(tout$val - val0))
  tout$adtime <- tout$time - tout$time[id0]

  list(
    tout = tout %>% dplyr::relocate(.data$adtime, .after = .data$time),
    tdrs = tdrs %>%
      dplyr::select(val, rate, ratestd, npos, tot, ci, skern)
  )
}
