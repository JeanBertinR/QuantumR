#' @title Implement a shiny web app to compare h2o and Spark supervised machine learning models for classification tasks
#'
#' @description This function creates in one line of code a shareable web app to compare supervised classification model performances
#'
#' @param data dataset containing one or more explanatory variables and one categorical variable to predict.
#'    The dataset must be a data.frame or a data.table and can contain time-based column on Date or POSIXct format
#'
#' @param y the categorical output variable to predict (must correspond to one data column)
#'
#' @param framework the machine learning framework chosen to train and test models (either h2o or Spark). h2o by default
#'
#' @param share_app a logical value indicating whether the app must be shared on local LAN
#'
#' @param port a four-digit number corresponding to the port the application should listen to. This parameter is necessary only  if share_app option is set to TRUE
#'
#' @return NULL
#'
#' @examples
#'\dontrun{
#' library(shinyML)
#' shinyML_classification(data = iris,y = "Species",framework = "h2o")
#'}
#' @import shiny argonDash argonR dygraphs data.table ggplot2 shinycssloaders sparklyr
#' @importFrom dplyr %>% select mutate group_by ungroup summarise arrange rename select_if row_number sample_frac anti_join n
#' @importFrom tidyr gather everything
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom h2o h2o.init as.h2o h2o.deeplearning h2o.varimp h2o.predict h2o.gbm h2o.naiveBayes h2o.randomForest h2o.automl h2o.clusterStatus
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly layout add_trace hide_legend
#' @importFrom shinyWidgets materialSwitch switchInput sendSweetAlert knobInput awesomeCheckbox actionBttn prettyCheckboxGroup
#' @importFrom shinyjs useShinyjs hideElement
#' @importFrom stats predict reorder cor acf
#' @importFrom lubridate is.Date is.POSIXct
#' @importFrom graphics par
#' @author Jean Bertin, \email{jean.bertin@mines-paris.org}
#' @export

library(shiny)
library(qsimulatR)

#QuantumVisual <- function() {
  # User interface
  ui <- fluidPage(

    titlePanel("Let´s visualize your Quantum computation"),

    sidebarLayout(
      sidebarPanel(
        numericInput("n_qubits", "Number of qubits:", min = 1, max = 10, value = 1),
        actionButton("simulate", "Simulate"),
        downloadButton("downloadData", "Download results")
      ),

      mainPanel(
        tableOutput("results")
      )
    )
  )

  # Server
  server <- function(input, output) {

    # Simulation function
    simulate_quantum_computation <- eventReactive(input$simulate, {
      n_qubits <- input$n_qubits

      # Creating a quantum circuit
      qc <- qstate(nbits = n_qubits)
      qc <- H(qc, 1:n_qubits)
      qc <- phase_shift_gate(qc, pi/4, 1:n_qubits)
      qc <- controlled_not_gate(qc, 1, 2)
      qc <- measure(qc, 1:n_qubits)

      # Simulating the quantum circuit
      counts <- simulate(qc, n_shots = 1000)

      # Results of the simulation
      results <- data.frame(state = names(counts), count = unname(counts))
      results$count <- round(results$count/sum(results$count), 3)*100

      results
    })

    # Displaying the results
    output$results <- renderTable({
      simulate_quantum_computation()
    })

    # Downloading the results
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("qsimulatR_simulation_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simulate_quantum_computation(), file, row.names = FALSE)
      }
    )
  }

  # Assembly UI and SERVER sides inside shinyApp
  app <- shiny::shinyApp(ui,server)

  runApp(app,host = "0.0.0.0",quiet = TRUE,launch.browser = TRUE)
#}

# Running the QuantumVisual app
QuantumVisual()


