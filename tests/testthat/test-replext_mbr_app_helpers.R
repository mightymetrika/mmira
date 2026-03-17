testthat::test_that("mbr_app_text_to_vector parses common input styles", {
  testthat::expect_equal(mbr_app_text_to_vector("10:12"), 10:12)
  testthat::expect_equal(mbr_app_text_to_vector("10, 20, 30"), c(10, 20, 30))
})

testthat::test_that("mbr_append_input_params appends app metadata", {
  x <- data.frame(setting = "single_centre", method = "MBR", n = 10)
  input <- list(
    setting = "single",
    methods = c("MBR", "CR"),
    n_simulations = 5,
    n_values = "10, 20",
    imbalance_threshold = 2,
    lambda_values = "15:30",
    n_centres = 10,
    max_n_per_centre = 50
  )

  out <- mbr_append_input_params(x, input)

  testthat::expect_true("run_code" %in% names(out))
  testthat::expect_true("app_methods" %in% names(out))
  testthat::expect_equal(out$app_methods[1], "MBR, CR")
  testthat::expect_equal(out$app_n_values[1], "10, 20")
  testthat::expect_false("app_seed" %in% names(out))
  testthat::expect_true(nzchar(out$run_code[1]))
})

testthat::test_that("mbr_app_run_simulation dispatches to the single-centre replext", {
  input <- list(
    setting = "single",
    n_values = "10, 20",
    methods = c("MBR", "PBR"),
    n_simulations = 5,
    imbalance_threshold = 2,
    lambda_values = "15:16",
    n_centres = 3,
    max_n_per_centre = 20
  )

  out <- mbr_app_run_simulation(input)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(out$setting == "single_centre"))
})
