library(tidyverse)

illa <- function(df, dt, val0, maxi, skern) {
  # df should be a tibble with the following columns: subid, age, val
  t <- df %>%
    arrange(subid, age) %>%
    group_nest(subid) %>%
    mutate(
      min = map_dbl(data, ~ min(.x$val)),
      max = map_dbl(data, ~ max(.x$val)),
      val = map_dbl(data, ~ first(.x$val)),
      nvis = map_dbl(data, ~ nrow(.x)),
      mrate = map_dbl(data, ~ coef(lm(val ~ age, data = .x))[["age"]])
    ) %>%
    select(-data)

  tmod <- t %>%
    filter(.data$nvis > 1 & .data$mrate < 100)
  qval <- seq(
    from = min(tmod$val),
    to = max(tmod$val),
    length.out = 151
  )

  tdrs <- tibble(
    val = qval,
    skern = skern
  ) %>%
    rowwise() %>%
    mutate(
      ids = list((tmod$min < .data$val) & (tmod$max > .data$val)),
      tot = sum(ids),
      rates = list(tmod$mrate[ids]),
      vals = list(rep(.data$val, .data$tot)),
      rate = weighted.mean(rates, tmod$nvis[ids]),
      ratestd = sd(rates),
      npos = sum(rates > 0),
      ci = 1.96 * .data$ratestd / sqrt(.data$tot)
    ) %>%
    select(-ids)

  rates <- unlist(tdrs$rates)
  vals <- unlist(tdrs$vals)

  tdrs <- tdrs %>%
    select(-rates, -vals) %>%
    filter(.data$tot >= 2)

  if (skern > 0) {
    fit <- loess(rates ~ vals, span = skern)
    srate <- predict(fit)
    ids <- !duplicated(vals)
    vals <- unique(vals)
    srate <- srate[ids]
    tdrs$rate <- srate[match(tdrs$val, vals)] # check this
  }

  med_rate <- median(tdrs$rate)

  # Perform iterative model
  # set inital conditions
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

  tout <- tibble(
    val = c(rev(valb[-1]), valf),
    time = c(rev(tb[-1]), tf),
    mrate = c(rev(rb[-1]), rf),
    sdrate = c(rev(sdb[-1]), sdf),
    nsubs = c(rev(nb[-1]), nf)
  ) %>%
    mutate(
      sdval = sqrt(
        (.data$mrate * dt * .data$sdrate / .data$mrate)^2 + (.05 * .data$val)^2
      ),
      ci95 = 1.96 * .data$sdval / sqrt(.data$nsubs)
    )
  id0 <- which.min(abs(tout$val - val0))
  tout$adtime <- tout$time - tout$time[id0]

  list(tout = tout, tdrs = tdrs)
}
