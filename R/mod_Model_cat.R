#' Model_cat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Model_cat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = "Univariate_analysis",
      tabPanel(
        "Table",
        column(
          10,
          h4("Analysis informations"),
          htmlOutput(ns("y_var_print"))
        ),
        column(
          2,
          absolutePanel(
            width = 200, right = 20, draggable = T,
            style = "opacity: 0.85; z-index: 10",
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
                choices = list("Binomial" = "binomial"),
                selected = "binomial"
              ),
              actionButton(ns("Run_analysis"), "Run analysis"),
              p(),
              downloadButton(ns("download"), "Download table (.tsv)")
            )
          ) # Absolutepanel
        ),
        column(
          10,
          DT::DTOutput(ns("Table_reg"))
        ),
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
  )
}

#' Model_cat Server Function
#'
#' @noRd
mod_Model_cat_server <- function(input, output, session, r) {
  ns <- session$ns



  data <- reactive(r$test$data)


  ## Parameters

  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c(names(data()[sapply(data(), class) %in% c("factor", "character")])[-1])
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
      
      if (input$x_var[1] == "All"){
        data_sort <- data()
      } else {
        data_sort <- select(data(), input$y_var, one_of(input$x_var))
      }
      
      
      df <- regression_table(data = data_sort, y_var = input$y_var, family = input$model)

      return(df)
    })
  })

  ## Output

  output$y_var_print <- renderText({
    paste(paste0("<b>Y variable :</b> ", as.character(input$y_var), " (Levels = ", paste(levels(unlist(select(data(), input$y_var))), collapse = " / "), ")"),
      paste0("<br> <b> Odds Ratio :</b> comparing to reference level (= ", as.character(levels(unlist(select(data(), input$y_var)))[1]), ")"),
      "<br> <i> Note that for categorical variables, each level of the variables is compared to the baseline level 
      (which is the first level by alphabetical order).</i>",
      sep = "\n"
    )
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
      ggplot(aes(x = reorder(multiv_graph,desc(multiv_graph)), y = `Odds Ratio`, color = `X Variables`)) +
      geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high), size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      coord_flip() +
      default_theme +
      labs(title = "Univariate analysis - Logistic model", y = "Odds Ratio [95%CI]", x = "") +
      theme(legend.position = "right")
  )
  
  # Download table
  output$download <- downloadHandler(
    filename = function() {
      paste("Univariate_logit_table.tsv")
    },
    content = function(file) {
      write.table(regression_table_df()[, -7], file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
}

## To be copied in the UI
# mod_Model_cat_ui("Model_cat_ui_1")

## To be copied in the server
# callModule(mod_Model_cat_server, "Model_cat_ui_1")
