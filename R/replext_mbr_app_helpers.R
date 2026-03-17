# Internal helpers for the merged block randomisation Shiny app

#' Convert text input to a vector
#'
#' @param text_input Character string.
#'
#' @return A vector parsed from the text input.
#'
#' @keywords internal
mbr_app_text_to_vector <- function(text_input) {
  if (!grepl("^[^,]+$", text_input) && !grepl("[()]", text_input)) {
    text_input <- paste0("c(", text_input, ")")
  }

  eval(parse(text = text_input))
}

#' Append app input parameters to simulation results
#'
#' @param df A data frame of simulation results.
#' @param input A list-like object containing app inputs.
#'
#' @return A data frame with appended app parameters.
#'
#' @keywords internal
mbr_append_input_params <- function(df, input) {
  out <- df

  out$run_code <- mmints::generateRunCode()
  out$app_setting <- input$setting
  out$app_methods <- paste(input$methods, collapse = ", ")
  out$app_n_simulations <- input$n_simulations

  if (identical(input$setting, "single")) {
    out$app_n_values <- input$n_values
    out$app_imbalance_threshold <- input$imbalance_threshold
  }

  if (identical(input$setting, "multi")) {
    out$app_lambda_values <- input$lambda_values
    out$app_n_centres <- input$n_centres
    out$app_max_n_per_centre <- input$max_n_per_centre
  }

  out
}

#' Run the app-selected simulation
#'
#' @param input A list-like object containing app inputs.
#'
#' @return A data frame of simulation results.
#'
#' @keywords internal
mbr_app_run_simulation <- function(input) {
  if (identical(input$setting, "single")) {
    return(
      replext_mbr_single(
        n_values = as.integer(mbr_app_text_to_vector(input$n_values)),
        methods = input$methods,
        n_simulations = input$n_simulations,
        ratio = c(1, 1),
        labels = c("A", "B"),
        imbalance_threshold = input$imbalance_threshold,
        verbose = FALSE
      )
    )
  }

  replext_mbr_multi(
    lambda_values = as.integer(mbr_app_text_to_vector(input$lambda_values)),
    methods = input$methods,
    n_simulations = input$n_simulations,
    n_centres = input$n_centres,
    max_n_per_centre = input$max_n_per_centre,
    ratio = c(1, 1),
    labels = c("A", "B"),
    verbose = FALSE
  )
}

#' Get the metric choices for the current setting
#'
#' @param setting Character string.
#'
#' @return A named character vector.
#'
#' @keywords internal
mbr_app_metric_choices <- function(setting) {
  if (identical(setting, "single_centre")) {
    return(c(
      "Mean final imbalance" = "mean_final_imbalance",
      "Mean suballocation imbalance" = "mean_suballocation_imbalance",
      "Proportion of suballocations with imbalance at or above threshold" = "prop_suballocation_imbalance",
      "Mean correct guess probability" = "mean_correct_guess_probability",
      "SE correct guess probability" = "se_correct_guess_probability"
    ))
  }

  c(
    "Mean recruited per centre" = "mean_n_recruited_per_centre",
    "Mean final imbalance" = "mean_final_imbalance",
    "Mean correct guess probability" = "mean_correct_guess_probability",
    "SE correct guess probability" = "se_correct_guess_probability"
  )
}

#' Create a plot for simulation results
#'
#' @param df A data frame of simulation results.
#' @param metric Character string naming the metric column to plot.
#'
#' @return A ggplot object.
#'
#' @keywords internal
mbr_app_plot <- function(df, metric) {
  x_var <- if (identical(df$setting[1], "single_centre")) "n" else "lambda"
  x_label <- if (identical(x_var, "n")) "Final sample size" else "Poisson recruitment mean"
  y_label <- gsub("_", " ", metric)
  y_label <- paste0(toupper(substr(y_label, 1, 1)), substr(y_label, 2, nchar(y_label)))

  ggplot2::ggplot(
    df,
    ggplot2::aes_string(x = x_var, y = metric, color = "method", group = "method")
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      color = "Method"
    ) +
    ggplot2::theme_minimal()
}
