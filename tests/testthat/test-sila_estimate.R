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

  illa_res <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)

  expect_no_error(
    res_lastalign <- sila_estimate(illa_res$tout, df)
  )

  expect_no_error(
    res_firstalign <- sila_estimate(illa_res$tout, df, align_event = "first")
  )

  expect_no_error(
    res_allalign <- sila_estimate(illa_res$tout, df, align_event = "all")
  )

  need_to_cleanup_matlab <- FALSE
  if (!testthat:::on_ci()) {
    # if not on CI, run the Matlab test
    # in order for this to work with devtools::check(),
    # the directory SILA-AD-Biomarker should be in your Matlab path
    matlab_cmd <- paste(
      "source ~/.zshrc;", # TODO: need a better solution here
      "matlab -nodisplay -nojvm -nosplash -nodesktop -r",
      '"run(\'test_sila_estimate.m\'); exit;"'
    )
    system(matlab_cmd)
    need_to_cleanup_matlab <- TRUE

    matlab_res_dir <- "."
  } else {
    matlab_res_dir <- file.path("..", "..", "..", "..", "matlab-results")
  }

  # compare R output to MATLAB output

  # align last
  tout_last <- readr::read_csv(
    file.path(matlab_res_dir, "test_sila_estimate_matlab_aevent_last.csv")
  )
  expect_equal(res_lastalign, tout_last)

  # align first
  tout_first <- readr::read_csv(
    file.path(matlab_res_dir, "test_sila_estimate_matlab_aevent_first.csv")
  )
  expect_equal(res_firstalign, tout_first)

  if (need_to_cleanup_matlab) {
    # remove matlab output files
    system("rm test_sila_estimate_matlab_aevent_*.csv")
  }
})
