#' ROC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ROC_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        11,
        p(strong("Data format:"), "The response variable (y-var) should be a two-level categorical variable: TRUE-FALSE, 1-0 or Yes-No. 
          The x-variables should be numeric variables only (ordinal categorical variable should be encoded with numbers only). 
          2 different mode are proposed: raw values (ROC curve/parameters are computed on the raw x-var values) 
          and logit predictions (ROC curve/parameters are computed using Y predictions from a univariate logistic model based on the selected x-var). "),
        p("TP: True Positive, FP: False Positive, TN: True Negative,FN: False Negative, 
      ACC: Overall accuracy of classification, 
      SENS: Sensitivity, SPEC: Specificity, PPV: Positive predictive value, NPV: Positive predictive value."),
      ),

      
      column(8,
             offset = 1,
             box(width = 11, plotOutput(ns("graph")))
      ),
      column(
      2,
      absolutePanel(
        width = 200, right = 20, draggable = T,
        style = "opacity: 0.85",
        wellPanel(
          selectInput(ns("y_var"),
                      label = ("Y variable: Binary (Yes/No, TRUE/FALSE)"), 
                      multiple = F, selected = NULL,
                      ""
          ),
          selectInput(ns("x_var"),
                      label = ("X Variable (explanatory/predictive) - Numeric only"),
                      multiple = T, selected = NULL,
                      choices = c("")
          ),
          radioButtons(ns("value"),
                       label = ("Values"),
                       choices = list("Raw values"="raw",
                                      "Logit predictions"="logit"),
                       selected = "raw"
          ),
          
          actionButton(ns("Run_analysis"), "Run analysis"),
          p(),
          downloadButton(ns("download"), "Download table (.tsv)")
        )
      ) # Absolutepanel
    ), #column
    br(),
    column(11, DT::DTOutput(ns("res_table")))
    )#fluidrow
  )
}
    
#' ROC Server Function
#'
#' @noRd 
mod_ROC_server <- function(input, output, session,r){
  ns <- session$ns
  
  ## DF
  
  data <- reactive(r$test$data)
  
  ## Parameters update
  
  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c(names(data()[sapply(data(), class) %in% c("character", "factor")]))
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "x_var",
      choices = c(setdiff(names(data()[sapply(data(), class) %in% c("numeric", "integer")]), input$y_var))
    )
  })
  
  ## ROC list 
  
  roc_list <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    req(data())
    isolate({
      ROC_calc(data(), y_var = input$y_var, x_var = input$x_var, value = input$value)
     
    })

  })
  
  ## Graph
  
  output$graph <- renderPlot({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({

      roc_list()$plot

    })
  })
  

  ## Table 
  
  param_df <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    req(roc_list())
    isolate({
      roc_list()$param_df
    })
  })
  

  output$res_table <- DT::renderDT(
    param_df(),
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
  
  # Download table
  output$download <- downloadHandler(
    filename = function() {
      paste("ROC_table.tsv")
    },
    content = function(file) {
      
      write.table(param_df(), file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
    
  
  
 
}
    
## To be copied in the UI
# mod_ROC_ui("ROC_ui_1")
    
## To be copied in the server
# callModule(mod_ROC_server, "ROC_ui_1")
 
