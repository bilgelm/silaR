test_that("SILA runs", {
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
    res <- sila(df, dt = 2, val0 = 2, maxi = 100)
  )

  need_to_cleanup_matlab <- FALSE
  if (!testthat:::on_ci()) {
    # if not on CI, run the Matlab test
    # in order for this to work with devtools::check(),
    # the directory SILA-AD-Biomarker should be in your Matlab path
    matlab_cmd <- paste(
      "source ~/.zshrc;", # TODO: need a better solution here
      "matlab -nodisplay -nojvm -nosplash -nodesktop -r",
      '"run(\'test_sila.m\'); exit;"'
    )
    system(matlab_cmd)
    need_to_cleanup_matlab <- TRUE

    matlab_res_dir <- "."
  } else {
    matlab_res_dir <- file.path("..", "..", "..", "..", "matlab-results")
  }

  # compare R output to MATLAB output

  tsila <- readr::read_csv(
    file.path(matlab_res_dir, "test_sila_matlab_tsila.csv")
  )
  expect_equal(res$tsila, tsila)

  tdrs <- readr::read_csv(
    file.path(matlab_res_dir, "test_sila_matlab_tdrs.csv")
  )
  expect_equal(res$tdrs, tdrs)
})
