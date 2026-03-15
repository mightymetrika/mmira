# Internal helpers for merged block randomisation simulations

#' Validate ratio and labels for allocation generators
#'
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A list containing validated `ratio` and `labels`.
#'
#' @keywords internal
mbr_validate_ratio_labels <- function(ratio, labels = NULL) {
  if (is.null(labels)) {
    labels <- as.character(seq_along(ratio))
  }

  if (length(ratio) != length(labels)) {
    stop("length(ratio) must match length(labels)")
  }

  if (any(ratio <= 0)) {
    stop("All ratio values must be positive")
  }

  list(
    ratio = as.integer(ratio),
    labels = as.character(labels)
  )
}

#' Build one permuted block
#'
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A character vector containing one randomly permuted block.
#'
#' @keywords internal
mbr_permuted_block <- function(ratio, labels) {
  block <- rep(labels, times = ratio)
  sample(block, size = length(block), replace = FALSE)
}

#' Generate a permuted block randomisation allocation
#'
#' @param n Final allocation length.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A character vector of length `n`.
#'
#' @keywords internal
mbr_pbr_allocation <- function(n, ratio = c(1, 1), labels = NULL) {
  checked <- mbr_validate_ratio_labels(ratio = ratio, labels = labels)
  ratio <- checked$ratio
  labels <- checked$labels

  block_length <- sum(ratio)
  n_blocks <- ceiling(n / block_length)

  out <- character(n_blocks * block_length)
  start <- 1L

  for (i in seq_len(n_blocks)) {
    block_i <- mbr_permuted_block(ratio = ratio, labels = labels)
    end <- start + block_length - 1L
    out[start:end] <- block_i
    start <- end + 1L
  }

  out[seq_len(n)]
}

#' Generate a merged block randomisation allocation
#'
#' @param n Final allocation length.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A character vector of length `n`.
#'
#' @keywords internal
mbr_allocation <- function(n, ratio = c(1, 1), labels = NULL) {
  checked <- mbr_validate_ratio_labels(ratio = ratio, labels = labels)

  as.character(
    mergedblocks::mergedblocks(
      n = n,
      ratio = checked$ratio,
      labels = checked$labels
    )
  )
}

#' Generate merged block randomisation allocations for multiple centres
#'
#' @param n Integer vector of allocation lengths.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A list with one allocation vector per centre.
#'
#' @keywords internal
mbr_multi_allocation <- function(n, ratio = c(1, 1), labels = NULL) {
  checked <- mbr_validate_ratio_labels(ratio = ratio, labels = labels)

  out <- mergedblocks::mergedblocksmulti(
    K = length(n),
    n = n,
    ratio = checked$ratio,
    labels = checked$labels
  )

  lapply(seq_along(n), function(i) {
    as.character(stats::na.omit(out[[i]]))
  })
}

#' Generate a complete randomisation allocation
#'
#' @param n Final allocation length.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A character vector of length `n`.
#'
#' @keywords internal
mbr_complete_randomisation <- function(n, ratio = c(1, 1), labels = NULL) {
  checked <- mbr_validate_ratio_labels(ratio = ratio, labels = labels)
  ratio <- checked$ratio
  labels <- checked$labels

  sample(labels,
         size = n,
         replace = TRUE,
         prob = ratio / sum(ratio))
}

#' Generate one allocation for a named method
#'
#' @param n Final allocation length.
#' @param method Randomisation method.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A character vector of length `n`.
#'
#' @keywords internal
mbr_generate_allocation <- function(n,
                                    method = c("MBR", "PBR", "CR"),
                                    ratio = c(1, 1),
                                    labels = NULL) {
  method <- match.arg(method)

  if (method == "MBR") {
    return(mbr_allocation(n = n, ratio = ratio, labels = labels))
  }

  if (method == "PBR") {
    return(mbr_pbr_allocation(n = n, ratio = ratio, labels = labels))
  }

  if (method == "CR") {
    return(mbr_complete_randomisation(n = n, ratio = ratio, labels = labels))
  }

  stop("Unsupported method")
}

#' Generate multiple allocations for a named method
#'
#' @param n Integer vector of allocation lengths.
#' @param method Randomisation method.
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return A list with one allocation vector per centre.
#'
#' @keywords internal
mbr_generate_allocations <- function(n,
                                     method = c("MBR", "PBR", "CR"),
                                     ratio = c(1, 1),
                                     labels = NULL) {
  method <- match.arg(method)

  if (method == "MBR") {
    return(mbr_multi_allocation(n = n, ratio = ratio, labels = labels))
  }

  lapply(n, function(n_i) {
    mbr_generate_allocation(
      n = n_i,
      method = method,
      ratio = ratio,
      labels = labels
    )
  })
}

#' Check that paper-style metrics are being used in a two-arm 1:1 setting
#'
#' @param ratio Integer allocation ratio.
#' @param labels Character labels for each treatment arm.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal
mbr_check_paper_metric_inputs <- function(ratio, labels) {
  checked <- mbr_validate_ratio_labels(ratio = ratio, labels = labels)
  ratio <- checked$ratio
  labels <- checked$labels

  if (length(labels) != 2L) {
    stop("Current paper-style metrics are implemented for two treatment arms only")
  }

  if (!identical(unname(ratio), c(1L, 1L))) {
    stop("Current paper-style metrics are implemented for 1:1 allocation only")
  }

  invisible(TRUE)
}

#' Compute absolute imbalance for a two-arm allocation prefix
#'
#' @param allocation Character allocation vector.
#' @param labels Character labels for the two treatment arms.
#'
#' @return Integer absolute imbalance.
#'
#' @keywords internal
mbr_imbalance <- function(allocation, labels = NULL) {
  if (is.null(labels)) {
    labels <- sort(unique(allocation))
  }

  mbr_check_paper_metric_inputs(ratio = c(1, 1), labels = labels)

  counts <- table(factor(allocation, levels = labels))
  abs(unname(counts[1] - counts[2]))
}

#' Compute prefix-wise imbalance values
#'
#' @param allocation Character allocation vector.
#' @param labels Character labels for the two treatment arms.
#'
#' @return Integer vector of prefix-wise imbalance values.
#'
#' @keywords internal
mbr_prefix_imbalance <- function(allocation, labels = NULL) {
  if (is.null(labels)) {
    labels <- sort(unique(allocation))
  }

  vapply(
    X = seq_along(allocation),
    FUN = function(i) {
      mbr_imbalance(allocation = allocation[seq_len(i)], labels = labels)
    },
    FUN.VALUE = integer(1)
  )
}

#' Compute the proportion of prefixes at or above an imbalance threshold
#'
#' @param allocation Character allocation vector.
#' @param imbalance_threshold Imbalance threshold.
#' @param labels Character labels for the two treatment arms.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
mbr_suballocation_imbalance_prop <- function(allocation,
                                             imbalance_threshold = 2,
                                             labels = NULL) {
  prefix_imbalance <- mbr_prefix_imbalance(allocation = allocation, labels = labels)
  mean(prefix_imbalance >= imbalance_threshold)
}

#' Compute Blackwell-Hodges correct guess probability
#'
#' @param allocation Character allocation vector.
#' @param labels Character labels for the two treatment arms.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
mbr_correct_guess_probability <- function(allocation, labels = NULL) {
  if (is.null(labels)) {
    labels <- sort(unique(allocation))
  }

  mbr_check_paper_metric_inputs(ratio = c(1, 1), labels = labels)

  guess_score <- numeric(length(allocation))

  for (i in seq_along(allocation)) {
    if (i == 1L) {
      guess_score[i] <- 0.5
    } else {
      counts <- table(factor(allocation[seq_len(i - 1L)], levels = labels))

      if (counts[1] == counts[2]) {
        guess_score[i] <- 0.5
      } else {
        predicted <- labels[which.min(counts)]
        guess_score[i] <- as.numeric(allocation[i] == predicted)
      }
    }
  }

  mean(guess_score)
}
