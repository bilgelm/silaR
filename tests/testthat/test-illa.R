test_that("ILLA without smoothing runs without error", {
  set.seed(42)
  df <- tibble::tibble(
    subid = c(
      1, 1, 1,
      2, 2, 2, 2, 2, 2
    ),
    age = c(
      seq(from = 50, to = 70, length.out = 3),
      seq(from = 50, to = 70, length.out = 6)
    ),
    val = c(
      2, 4, 6,
      2, 4, 6, 8, 10, 12
    ) + stats::rnorm(3 + 6, mean = 0, sd = .1)
  )

  expect_no_error(
    illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
  )
})
