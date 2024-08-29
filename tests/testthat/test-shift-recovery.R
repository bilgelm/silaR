test_that("SILA can recover shifts in simulated data", {
  set.seed(42)

  # simulate data with ground truth
  # we use a sigmoid function
  val_fun <- function(adtime) 1 / (1 + exp(-adtime))
  rate_fun <- function(adtime) exp(-adtime) * val_fun(adtime)^2

  # randomly pick a baseline adtime per person
  n_subjects <- 500
  n_vis <- 3
  base_adtime <- runif(n_subjects, -5, 5)
  adtime <- rep(base_adtime, each = n_vis) + rep(c(0, 2, 4), n_subjects)
  noise <- rnorm(n_subjects * n_vis, sd = .2)

  # if align_event = "first": noise[seq(1, n_subjects * n_vis, by = n_vis)] <- 0

  val <- val_fun(adtime) + noise

  # randomly pick a time shift per person
  shift <- runif(n_subjects, 50, 70)
  age <- adtime + rep(shift, each = n_vis)

  df <- tibble::tibble(
    subid = rep(1:n_subjects, each = n_vis),
    age = age,
    val = val
  )

  df_true <- df
  df_true$adtime <- adtime

  expect_no_error(
    res <- illa(df, val0 = 0.5)
  )

  expect_no_error(
    resfit <- sila_estimate(
      res,
      align_event = "all",
      adtime_limits = c(-10, 10)
    )
  )

  # RMSE of estimated disease age
  # note that this is the same as the RMSE of the estimated shifts
  # ie sqrt(mean((resfit %>% filter(dtageref==0) %>% pull(estaget0) - shift)^2))
  print("RMSE")
  print(sqrt(mean((resfit$adtime - df_true$adtime)^2)))
  expect_lt(sqrt(mean((resfit$adtime - df_true$adtime)^2)), 1.3)
})
