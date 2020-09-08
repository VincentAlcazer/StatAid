#' Model_num UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Model_num_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = "Univariate_analysis",
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
              selectInput(ns("y_var"),
                label = ("Y variable (outcome/to predict)"),
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
                choices = list("Linear" = "lm"),
                selected = "lm"
              ),
              actionButton(ns("Run_analysis"), "Run Analysis")
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

#' Model_num Server Function
#'
#' @noRd
mod_Model_num_server <- function(input, output, session, r) {
  ns <- session$ns


  data <- reactive(r$test$data)


  ## Parameters

  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c(names(data()[sapply(data(), class) %in% c("numeric", "integer")])[-1])
    )
    updateSelectInput(
      session,
      "x_var",
      choices = c("All", names(data()[!names(data()) %in% c("Patient_id", "Whole_cohort")])),
      selected = "All"
    )
  })


  ## DF

  regression_table_df <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({
      if (input$x_var == "All") {
        data_sort <- data()
      } else {
        data_sort <- select(data(), input$y_var, one_of(input$x_var))
      }

      df <- regression_table(data = data_sort, y_var = input$y_var)

      return(df)
    })
  })

  ## Output


  output$analysis_info <- renderText({
    paste(paste0("<b>Methods :</b> Univariate analysis is performed using a linear regression model. 
                 P-values are adjusted with the FDR (Benjamini-Hochberg) method. "))
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
      ggplot(aes(x = multiv_graph, y = `Beta Coeff.`, color = `X Variables`)) +
      geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
      coord_flip() +
      default_theme +
      labs(title = "Univariate analysis - Linear model", y = "Beta coefficient [95%CI]", x = "") +
      theme(legend.position = "right")
  )
}

## To be copied in the UI
# mod_Model_num_ui("Model_num_ui_1")

## To be copied in the server
# callModule(mod_Model_num_server, "Model_num_ui_1")
