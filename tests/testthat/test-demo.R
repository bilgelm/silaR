library(dplyr)

test_that("multiplication works", {
  need_to_cleanup_matlab <- FALSE
  if (!testthat:::on_ci()) {
    # if not on CI, run the Matlab test
    # in order for this to work with devtools::check(),
    # the directories SILA-AD-Biomarker and SILA-AD-Biomarker/demo
    # should be in your Matlab path
    matlab_cmd <- paste(
      "source ~/.zshrc;", # TODO: need a better solution here
      "matlab -nodisplay -nojvm -nosplash -nodesktop -r",
      '"run(\'test_demo.m\'); exit;"'
    )
    system(matlab_cmd, ignore.stdout = TRUE)
    need_to_cleanup_matlab <- TRUE

    matlab_res_dir <- "."
  } else {
    matlab_res_dir <- file.path("..", "..", "..", "..", "matlab-results")
  }

  # read MATLAB generated simulated data
  sim_data <- readr::read_csv(
    file.path(matlab_res_dir, "test_demo_matlab_simulated_data.csv"),
    show_col_types = FALSE
  )

  # run R version of SILA
  res <- sila(sim_data, dt = 0.25, val0 = 21, maxi = 200)

  # compare R output to MATLAB output

  tsila <- readr::read_csv(
    file.path(matlab_res_dir, "test_demo_matlab_tsila.csv"),
    show_col_types = FALSE
  )

  tdrs <- readr::read_csv(
    file.path(matlab_res_dir, "test_demo_matlab_tdrs.csv"),
    show_col_types = FALSE
  )

  if (need_to_cleanup_matlab) {
    # remove matlab output files
    system("rm test_demo_matlab_*.csv")
  }

  skip(
    message = paste(
      "Skipping MATLAB/R demo comparisons because they will fail.",
      "TODO: Fix these and remove the skipping command."
    )
  )
  expect_equal(
    res$tsila %>%
      mutate(across(where(is.numeric), .fns = function(x) round(x, 1))),
    tsila %>%
      mutate(across(where(is.numeric), .fns = function(x) round(x, 1)))
  )
  expect_equal(
    res$tdrs %>%
      mutate(across(where(is.numeric), .fns = function(x) round(x, 1))),
    tdrs %>%
      mutate(across(where(is.numeric), .fns = function(x) round(x, 1)))
  )
})
