#' Estimate value vs. time curve from longitudinal data.
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
#' @export
illa <- function(df, dt, val0, maxi, skern) {
  t <- df %>%
    dplyr::arrange(.data$subid, .data$age) %>%
    dplyr::group_nest(.data$subid, .key = "data") %>%
    dplyr::mutate(
      min = map_dbl(.data$data, ~ min(.x$val)),
      max = map_dbl(.data$data, ~ max(.x$val)),
      val = map_dbl(.data$data, ~ dplyr::first(.x$val)),
      nvis = map_dbl(.data$data, ~ nrow(.x)),
      mrate = map_dbl(.data$data, ~ coef(lm(val ~ age, data = .x))[["age"]])
    ) %>%
    dplyr::select(-.data$data)

  tmod <- t %>%
    dplyr::filter(.data$nvis > 1 & .data$mrate < 100)
  qval <- seq(
    from = min(tmod$val),
    to = max(tmod$val),
    length.out = 151
  )

  tdrs <- dplyr::tibble(
    val = qval,
    skern = skern
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ids = list((tmod$min < .data$val) & (tmod$max > .data$val)),
      tot = sum(ids),
      rates = list(tmod$mrate[ids]),
      vals = list(rep(.data$val, .data$tot)),
      rate = stats::weighted.mean(rates, tmod$nvis[ids]),
      ratestd = stats::sd(rates),
      npos = sum(rates > 0),
      ci = 1.96 * .data$ratestd / sqrt(.data$tot)
    ) %>%
    dplyr::select(-ids)

  rates <- unlist(tdrs$rates)
  vals <- unlist(tdrs$vals)

  tdrs <- tdrs %>%
    dplyr::select(-rates, -vals) %>%
    dplyr::filter(.data$tot >= 2)

  if (skern > 0) {
    fit <- stats::loess(rates ~ vals, span = skern)
    srate <- stats::predict(fit)
    ids <- !duplicated(vals)
    vals <- unique(vals)
    srate <- srate[ids]
    tdrs$rate <- srate[match(tdrs$val, vals)] # check this
  }

  med_rate <- stats::median(tdrs$rate)

  # Perform iterative model
  # set initial conditions
  # use iterative process to go forward and backward through discretely
  # sampled rate data

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
    if (tdrs$tot[id] < 2) break
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
    if (tdrs$tot[id] < 2) break
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

  tout <- dplyr::tibble(
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

  list(tout = tout, tdrs = tdrs)
}
