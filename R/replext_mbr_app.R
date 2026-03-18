#' Replication and extension app for merged block randomisation
#'
#' This Shiny app provides a first interface for the merged block
#' randomisation REPLEXT. Users can select simulation settings, run the
#' current `mmira` simulation functions, and view summary tables and plots.
#'
#' @return A Shiny app object.
#'
#' @details
#' The default inputs are set to mirror the main simulation structures from
#' Van der Pas (2019):
#'
#' \itemize{
#'   \item single-centre runs with `n = 10:100`
#'   \item multicentre runs with `lambda = 15:30`, `10` centres, and a maximum
#'   of `50` participants per centre
#'   \item `1000` simulation runs per condition
#'   \item permuted block randomisation default block size of `4`
#' }
#'
#' The app is intentionally simple so it can later be extended with the
#' `mmints` PostgreSQL workflow.
#'
#' @references
#' Van der Pas, S. L. (2019). Merged block randomisation: A novel
#' randomisation procedure for small clinical trials. *Clinical Trials*, 16(3),
#' 246-252. <doi:10.1177/1740774519827957>
#'
#' @examples
#' if (interactive()) {
#'   replext_mbr_app()
#' }
#'
#' @export
replext_mbr_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Merged Block Randomisation REPLEXT"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::helpText(
          "Defaults are set to mirror the main Van der Pas (2019) simulation structure.",
          "For a quicker check, reduce the number of simulation runs."
        ),
        shiny::selectInput(
          inputId = "setting",
          label = "Simulation setting",
          choices = c(
            "Single-centre" = "single",
            "Multicentre" = "multi"
          ),
          selected = "single"
        ),
        shiny::checkboxGroupInput(
          inputId = "methods",
          label = "Methods",
          choices = c(
            "Merged block randomisation (MBR)" = "MBR",
            "Permuted block randomisation (PBR)" = "PBR",
            "Complete randomisation (CR)" = "CR"
          ),
          selected = c("MBR", "PBR", "CR")
        ),
        shiny::numericInput(
          inputId = "pbr_block_size",
          label = "PBR block size (used only for PBR)",
          value = 4,
          min = 1,
          step = 1
        ),
        shiny::numericInput(
          inputId = "n_simulations",
          label = "Number of simulation runs",
          value = 1000,
          min = 1,
          step = 1
        ),
        shiny::conditionalPanel(
          condition = "input.setting == 'single'",
          shiny::textInput(
            inputId = "n_values",
            label = "Final sample sizes",
            value = "10:100"
          ),
          shiny::numericInput(
            inputId = "imbalance_threshold",
            label = "Suballocation imbalance threshold",
            value = 2,
            min = 0,
            step = 1
          )
        ),
        shiny::conditionalPanel(
          condition = "input.setting == 'multi'",
          shiny::textInput(
            inputId = "lambda_values",
            label = "Poisson recruitment means",
            value = "15:30"
          ),
          shiny::numericInput(
            inputId = "n_centres",
            label = "Number of centres",
            value = 10,
            min = 1,
            step = 1
          ),
          shiny::numericInput(
            inputId = "max_n_per_centre",
            label = "Maximum n per centre",
            value = 50,
            min = 1,
            step = 1
          )
        ),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::br(),
        shiny::br(),
        shiny::downloadButton("downloadBtn", "Download Results"),
        shiny::br(),
        shiny::br(),
        mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::br(),
        shiny::uiOutput("plot_controls"),
        shiny::plotOutput("resultsPlot"),
        shiny::br(),
        mmints::citationUI("citations")$output
      )
    )
  )

  server <- function(input, output, session) {
    results <- shiny::reactiveVal(data.frame())
    results_exp <- shiny::reactiveVal(data.frame())

    citations <- list(
      "Merged block randomisation paper:" =
        "van der Pas, S. L. (2019). Merged block randomisation: A novel randomisation procedure for small clinical trials. Clinical trials (London, England), 16(3), 246-252. https://doi.org/10.1177/1740774519827957",
      "Software implementing merged block randomisation:" = function() {
        mmints::format_citation(utils::citation("mergedblocks"))
      }
    )

    mmints::citationServer("citations", citations)

    shiny::observeEvent(input$runSim, {
      shiny::req(length(input$methods) > 0)

      sim_results <- mbr_app_run_simulation(input)
      rownames(sim_results) <- NULL

      sim_results_exp <- mbr_append_input_params(sim_results, input)

      results(sim_results_exp)
      results_exp(sim_results_exp)
    })

    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 10, scrollX = TRUE))

    output$plot_controls <- shiny::renderUI({
      if (nrow(results()) == 0) {
        return(NULL)
      }

      metric_choices <- mbr_app_metric_choices(results()$setting[1])

      shiny::selectInput(
        inputId = "selected_metric",
        label = "Metric to plot",
        choices = metric_choices,
        selected = unname(metric_choices[1])
      )
    })

    output$resultsPlot <- shiny::renderPlot({
      shiny::req(nrow(results()) > 0)
      shiny::req(input$selected_metric)

      mbr_app_plot(results(), metric = input$selected_metric)
    })

    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("mmira_replext_", Sys.Date(), ".csv")
      },
      content = function(file) {
        shiny::req(nrow(results_exp()) > 0)
        utils::write.csv(results_exp(), file, row.names = FALSE)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
