test_that("ILLA without smoothing runs without error", {
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
      2.14, 3.94, 6.04,
      2.06, 4.04, 5.99, 8.15, 9.99, 12.2
    )
  )

  expect_no_error(
    res <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
  )
})
