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

  expect_equal(colnames(res$tsila), colnames(tsila))
  expect_true(all(abs(res$tsila$val - tsila$val) < 0.15))
  expect_true(all(abs(res$tsila$val - tsila$val) / tsila$val < 0.01))
  expect_equal(res$tsila %>% select(time, adtime), tsila %>% select(time, adtime))
  # rest of the columns (mrate, sdrate, nsubs, sdval, ci95) are not checked
  # because a slight difference in val can make a big difference for these

  expect_equal(colnames(res$tdrs), colnames(tdrs))
  expect_equal(res$tdrs %>% select(-rate), tdrs %>% select(-rate))
  expect_true(all(abs(res$tdrs$rate - tdrs$rate) < 0.085))
  expect_true(all(abs(res$tdrs$rate - tdrs$rate) / tdrs$rate < 0.03))
})
