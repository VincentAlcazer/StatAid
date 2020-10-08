#' Model_surv_multi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Model_surv_multi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = "Multivariate_analysis",
      tabPanel(
        "Table",
        column(
          10,
          h4("Analysis informations"),
          htmlOutput(ns("analysis_info"))
        ),
        column(
          10,
          DT::DTOutput(ns("Table_reg"))
        ),
        column(
          2,
          absolutePanel(
            width = 200, right = 20, draggable = T,
            style = "opacity: 0.85",
            wellPanel(
              selectInput(ns("time_var"),
                label = ("Time column (numeric)"),
                multiple = F, selected = NULL,
                ""
              ),
              selectInput(ns("y_var"),
                label = ("Status column (0-1 or dead-alive)"),
                multiple = F, selected = NULL,
                ""
              ),
              selectInput(ns("x_var"),
                label = ("X Variable(s)"),
                multiple = T, selected = "All",
                choices = c("All")
              ),
              radioButtons(ns("model"),
                label = ("Model"),
                choices = list("Cox" = "cox"),
                selected = "cox"
              ),
              actionButton(ns("Run_analysis"), "Run Analysis"),
              p(),
              downloadButton(ns("download"), "Download table (.tsv)")
            )
          ) # Absolutepanel
        )
      ), # Column & tabpanel

      tabPanel(
        "Graph",
        column(
          10,
          plotOutput(ns("Graph_reg"))
        ),
        column(2)
      ) # TabPanel
    ) # Tabset panel
  ) # Taglist
}

#' Model_surv_multi Server Function
#'
#' @noRd
mod_Model_surv_multi_server <- function(input, output, session, r) {
  ns <- session$ns

  data <- reactive(r$test$data)


  ## Parameters


  observe({
    updateSelectInput(
      session,
      "time_var",
      choices = c(names(data()[sapply(data(), class) %in% c("numeric", "integer")]))
    )
  })

  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c(setdiff(names(data()[sapply(data(), class) %in% c("numeric", "integer")]), c(input$time_var)))
    )
    updateSelectInput(
      session,
      "x_var",
      choices = c(names(data()[!names(data()) %in% c("Patient_id", "Whole_cohort")]))
    )
  })


  ## DF

  regression_table_df <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({
      data_sort <- select(data(), input$y_var, one_of(input$x_var), one_of(input$time_var))

      df <- regression_table_multi_cox(data = data_sort, y_var = input$y_var, time = input$time_var)

      return(df)
    })
  })

  ## Output


  output$analysis_info <- renderText({
    paste(paste0("<b>Methods :</b> Multivariate analysis is performed using a cox regression model. 
                 
                Variables are manually selected by the user 
                (e.g. Variables with a p-value < 0.10 in univariate analysis have been included in the multivariate model.)
                 
                 P-values are adjusted with the FDR (Benjamini-Hochberg) method. <br>
                 
                 <b>NB :</b> Your multivariate model is currently based on ", length(input$x_var), " variables. As a rule-of-thumb, you would
                 need at least ", 10 * length(input$x_var), " patients/samples to have enough power to perform it.
                 "))
  })


  output$Table_reg <- DT::renderDT(
    regression_table_df()[, -7],
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    server = T,
    rownames = FALSE,
    options = list(
      lengthChange = TRUE,
      pageLength = 30,
      columnDefs = list(list(className = "dt-left", targets = "_all"))
    )
  )


  output$Graph_reg <- renderPlot(
    regression_table_df() %>%
      ggplot(aes(x = multiv_graph, y = HR, color = `X Variables`)) +
      geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high), size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      coord_flip() +
      default_theme +
      labs(title = "Multivariate analysis - Cox model", y = "Hazard Ratio [95%CI]", x = "") +
      theme(legend.position = "right")
  )
  
  # Download table
  output$download <- downloadHandler(
    filename = function() {
      paste("Multivariate_cox_table.tsv")
    },
    content = function(file) {
      write.table(regression_table_df()[, -7], file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
}

## To be copied in the UI
# mod_Model_surv_multi_ui("Model_surv_multi_ui_1")

## To be copied in the server
# callModule(mod_Model_surv_multi_server, "Model_surv_multi_ui_1")
