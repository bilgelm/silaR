test_that("SILA estimate runs without error", {
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

  res <- illa(df, val0 = 2)

  expect_no_error(
    res_lastalign <- sila_estimate(res)
  )
})
