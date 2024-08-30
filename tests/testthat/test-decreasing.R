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
      6.04, 3.94, 2.14,
      12.2, 9.99, 8.15, 5.99, 4.04, 2.06
    )
  )

  val0 <- 2

  expect_no_error(
    res <- illa(df, val0)
  )

  expect_equal(res$adtime_to_val(0), val0)
})
