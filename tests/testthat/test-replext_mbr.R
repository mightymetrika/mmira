testthat::test_that("simulate_mbr_once respects the MBR(2) prefix imbalance bound", {
  set.seed(123)
  out <- simulate_mbr_once(n = 50, method = "MBR")

  imbalance_path <- abs(cumsum(ifelse(out$allocation == "A", 1, -1)))

  expect_length(out$allocation, 50)
  expect_true(all(imbalance_path <= 2))
})

testthat::test_that("simulate_mbr_once returns the expected pieces", {
  set.seed(123)
  out <- simulate_mbr_once(n = 20)

  expect_true(is.list(out))
  expect_length(out$allocation, 20)
  expect_true(all(c(
    "allocation",
    "final_imbalance",
    "mean_suballocation_imbalance",
    "prop_suballocation_imbalance",
    "correct_guess_probability"
  ) %in% names(out)))
})

testthat::test_that("simulate_mbr_once can use a larger PBR block size", {
  set.seed(123)
  out <- simulate_mbr_once(n = 4, method = "PBR", pbr_block_size = 4)

  expect_length(out$allocation, 4)
  expect_equal(out$final_imbalance, 0)
})

testthat::test_that("replext_mbr_single returns one row per method-by-n condition", {
  set.seed(123)
  out <- replext_mbr_single(
    n_values = c(10, 20),
    methods = c("MBR", "PBR"),
    n_simulations = 5,
    pbr_block_size = 4
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
  expect_true(all(c(
    "setting",
    "method",
    "n",
    "n_simulations",
    "imbalance_threshold",
    "pbr_block_size",
    "mean_final_imbalance",
    "mean_suballocation_imbalance",
    "prop_suballocation_imbalance",
    "mean_correct_guess_probability",
    "se_correct_guess_probability"
  ) %in% names(out)))
  expect_true(all(out$pbr_block_size == 4))
})

testthat::test_that("replext_mbr_multi returns one row per method-by-lambda condition", {
  set.seed(123)
  out <- replext_mbr_multi(
    lambda_values = 15:16,
    methods = c("MBR", "PBR"),
    n_simulations = 5,
    n_centres = 3,
    max_n_per_centre = 20,
    pbr_block_size = 4
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
  expect_true(all(c(
    "setting",
    "method",
    "lambda",
    "n_centres",
    "max_n_per_centre",
    "n_simulations",
    "pbr_block_size",
    "mean_n_recruited_per_centre",
    "mean_final_imbalance",
    "mean_correct_guess_probability",
    "se_correct_guess_probability"
  ) %in% names(out)))
  expect_true(all(out$pbr_block_size == 4))
})
