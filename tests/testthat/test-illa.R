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
    res0 <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
  )

  expect_no_error(
    res05 <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0.5)
  )

  need_to_cleanup_matlab <- FALSE
  if (!testthat:::on_ci()) {
    # if not on CI, run the Matlab test
    # in order for this to work with devtools::check(),
    # the directory SILA-AD-Biomarker should be in your Matlab path
    matlab_cmd <- paste(
      "source ~/.zshrc;", # TODO: need a better solution here
      "matlab -nodisplay -nojvm -nosplash -nodesktop -r",
      '"run(\'test_illa.m\'); exit;"'
    )
    system(matlab_cmd)
    need_to_cleanup_matlab <- TRUE

    matlab_res_dir <- "."
  } else {
    matlab_res_dir <- file.path("..", "..", "..", "..", "matlab-results")
  }

  # compare R output to MATLAB output

  # no smoothing
  tout0 <- readr::read_csv(
    file.path(matlab_res_dir, "test_illa_matlab_tout_skern0.csv")
  )
  expect_equal(res0$tout, tout0)

  tdrs0 <- readr::read_csv(
    file.path(matlab_res_dir, "test_illa_matlab_tdrs_skern0.csv")
  )
  expect_equal(res0$tdrs, tdrs0)

  # with smoothing
  tout05 <- readr::read_csv(
    file.path(matlab_res_dir, "test_illa_matlab_tout_skern0.5.csv")
  )
  expect_equal(res05$tout, tout05, tolerance = 1e-3)

  tdrs05 <- readr::read_csv(
    file.path(matlab_res_dir, "test_illa_matlab_tdrs_skern0.5.csv")
  )
  expect_equal(res05$tdrs, tdrs05, tolerance = 2e-3)

  if (need_to_cleanup_matlab) {
    # remove matlab output files
    system("rm test_illa_matlab_t*.csv")
  }
})
