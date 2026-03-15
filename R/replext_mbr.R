#' Simulate one merged block randomisation run
#'
#' This function generates one allocation sequence under a selected
#' randomisation method and computes the paper-style balance and predictability
#' metrics used in Van der Pas (2019).
#'
#' @param n Integer. Final allocation length.
#' @param method Character. One of `"MBR"`, `"PBR"`, or `"CR"`.
#' @param ratio Integer allocation ratio. Defaults to `c(1, 1)`.
#' @param labels Character labels for each treatment arm. Defaults to
#'   `c("A", "B")`.
#' @param imbalance_threshold Integer. Threshold used for the suballocation
#'   imbalance metric. Defaults to `2` to match the paper's main setting.
#'
#' @return A named list with the allocation and summary metrics.
#'
#' @references
#' Van der Pas, S. L. (2019). Merged block randomisation: A novel
#' randomisation procedure for small clinical trials. *Clinical Trials*, 16(3),
#' 246-252. <doi:10.1177/1740774519827957>
#'
#' @examples
#' simulate_mbr_once(n = 20)
#'
#' @export
simulate_mbr_once <- function(n,
                              method = c("MBR", "PBR", "CR"),
                              ratio = c(1, 1),
                              labels = c("A", "B"),
                              imbalance_threshold = 2) {
  method <- match.arg(method)

  allocation <- mbr_generate_allocation(
    n = n,
    method = method,
    ratio = ratio,
    labels = labels
  )

  list(
    allocation = allocation,
    final_imbalance = mbr_imbalance(allocation = allocation, labels = labels),
    mean_suballocation_imbalance = mean(mbr_prefix_imbalance(allocation = allocation, labels = labels)),
    prop_suballocation_imbalance = mbr_suballocation_imbalance_prop(
      allocation = allocation,
      imbalance_threshold = imbalance_threshold,
      labels = labels
    ),
    correct_guess_probability = mbr_correct_guess_probability(
      allocation = allocation,
      labels = labels
    )
  )
}

#' Replicate and extend the single-centre merged block simulation
#'
#' This function reproduces the single-centre simulation structure from
#' Van der Pas (2019): two-arm 1:1 allocation, sample sizes from 10 to 100,
#' and repeated evaluation of balance and predictability.
#'
#' @param n_values Integer vector of final sample sizes.
#' @param methods Character vector of methods to compare.
#' @param n_simulations Integer. Number of simulation runs per condition.
#' @param ratio Integer allocation ratio. Defaults to `c(1, 1)`.
#' @param labels Character labels for each treatment arm.
#' @param imbalance_threshold Integer. Threshold used for the suballocation
#'   imbalance metric.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, prints simple progress messages.
#'
#' @return A data frame with one row per method-by-sample-size condition.
#'
#' @references
#' Van der Pas, S. L. (2019). Merged block randomisation: A novel
#' randomisation procedure for small clinical trials. *Clinical Trials*, 16(3),
#' 246-252. <doi:10.1177/1740774519827957>
#'
#' @examples
#' replext_mbr_single(n_values = c(10, 20), n_simulations = 10, seed = 123)
#'
#' @export
replext_mbr_single <- function(n_values = 10:100,
                               methods = c("MBR", "PBR", "CR"),
                               n_simulations = 1000,
                               ratio = c(1, 1),
                               labels = c("A", "B"),
                               imbalance_threshold = 2,
                               seed = NULL,
                               verbose = FALSE) {
  mbr_check_paper_metric_inputs(ratio = ratio, labels = labels)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  results <- data.frame()

  for (method in methods) {
    for (n_i in n_values) {
      if (verbose) {
        message("Running ", method, " at n = ", n_i)
      }

      sim_list <- vector(mode = "list", length = n_simulations)

      for (i in seq_len(n_simulations)) {
        sim_list[[i]] <- simulate_mbr_once(
          n = n_i,
          method = method,
          ratio = ratio,
          labels = labels,
          imbalance_threshold = imbalance_threshold
        )
      }

      results_i <- data.frame(
        setting = "single_centre",
        method = method,
        n = n_i,
        n_simulations = n_simulations,
        imbalance_threshold = imbalance_threshold,
        mean_final_imbalance = mean(vapply(sim_list, function(x) x$final_imbalance, numeric(1))),
        mean_suballocation_imbalance = mean(vapply(sim_list, function(x) x$mean_suballocation_imbalance, numeric(1))),
        prop_suballocation_imbalance = mean(vapply(sim_list, function(x) x$prop_suballocation_imbalance, numeric(1))),
        mean_correct_guess_probability = mean(vapply(sim_list, function(x) x$correct_guess_probability, numeric(1))),
        se_correct_guess_probability = stats::sd(vapply(sim_list, function(x) x$correct_guess_probability, numeric(1))) / sqrt(n_simulations),
        stringsAsFactors = FALSE
      )

      results <- rbind(results, results_i)
    }
  }

  rownames(results) <- NULL
  results
}

#' Replicate and extend the multicentre merged block simulation
#'
#' This function reproduces the multicentre simulation structure from
#' Van der Pas (2019): 10 centres, independent centre-specific randomisation
#' lists, Poisson recruitment per centre, and pooled final imbalance.
#'
#' @param lambda_values Integer vector of Poisson means used for centre-level
#'   recruitment.
#' @param methods Character vector of methods to compare.
#' @param n_simulations Integer. Number of simulation runs per condition.
#' @param n_centres Integer. Number of centres/strata.
#' @param max_n_per_centre Integer. Maximum allocation list length per centre.
#' @param ratio Integer allocation ratio. Defaults to `c(1, 1)`.
#' @param labels Character labels for each treatment arm.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, prints simple progress messages.
#'
#' @return A data frame with one row per method-by-lambda condition.
#'
#' @references
#' Van der Pas, S. L. (2019). Merged block randomisation: A novel
#' randomisation procedure for small clinical trials. *Clinical Trials*, 16(3),
#' 246-252. <doi:10.1177/1740774519827957>
#'
#' @examples
#' replext_mbr_multi(lambda_values = 15:16, n_simulations = 10, seed = 123)
#'
#' @export
replext_mbr_multi <- function(lambda_values = 15:30,
                              methods = c("MBR", "PBR", "CR"),
                              n_simulations = 1000,
                              n_centres = 10,
                              max_n_per_centre = 50,
                              ratio = c(1, 1),
                              labels = c("A", "B"),
                              seed = NULL,
                              verbose = FALSE) {
  mbr_check_paper_metric_inputs(ratio = ratio, labels = labels)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  results <- data.frame()

  for (method in methods) {
    for (lambda_i in lambda_values) {
      if (verbose) {
        message("Running ", method, " at lambda = ", lambda_i)
      }

      pooled_imbalance <- numeric(n_simulations)
      mean_cgp <- numeric(n_simulations)
      mean_n_recruited <- numeric(n_simulations)

      for (i in seq_len(n_simulations)) {
        n_recruited <- pmin(stats::rpois(n_centres, lambda = lambda_i), max_n_per_centre)
        full_allocations <- mbr_generate_allocations(
          n = rep(max_n_per_centre, n_centres),
          method = method,
          ratio = ratio,
          labels = labels
        )

        centre_allocations <- vector(mode = "list", length = n_centres)
        centre_cgp <- numeric(n_centres)

        for (centre in seq_len(n_centres)) {
          if (n_recruited[centre] == 0L) {
            centre_allocations[[centre]] <- character(0)
            centre_cgp[centre] <- NA_real_
          } else {
            centre_allocations[[centre]] <- full_allocations[[centre]][seq_len(n_recruited[centre])]
            centre_cgp[centre] <- mbr_correct_guess_probability(
              allocation = centre_allocations[[centre]],
              labels = labels
            )
          }
        }

        pooled_allocation <- unlist(centre_allocations)

        if (length(pooled_allocation) == 0L) {
          pooled_imbalance[i] <- NA_real_
        } else {
          pooled_imbalance[i] <- mbr_imbalance(
            allocation = pooled_allocation,
            labels = labels
          )
        }

        mean_cgp[i] <- mean(centre_cgp, na.rm = TRUE)
        mean_n_recruited[i] <- mean(n_recruited)
      }

      results_i <- data.frame(
        setting = "multicentre",
        method = method,
        lambda = lambda_i,
        n_centres = n_centres,
        max_n_per_centre = max_n_per_centre,
        n_simulations = n_simulations,
        mean_n_recruited_per_centre = mean(mean_n_recruited, na.rm = TRUE),
        mean_final_imbalance = mean(pooled_imbalance, na.rm = TRUE),
        mean_correct_guess_probability = mean(mean_cgp, na.rm = TRUE),
        se_correct_guess_probability = stats::sd(mean_cgp, na.rm = TRUE) / sqrt(sum(!is.na(mean_cgp))),
        stringsAsFactors = FALSE
      )

      results <- rbind(results, results_i)
    }
  }

  rownames(results) <- NULL
  results
}
