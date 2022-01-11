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
          The x-variables should be numeric variables only (ordinal categorical variable should be encoded with numbers only).")
      ),
      
      column(8,
             offset = 1,
             box(width = 11, plotOutput(ns("graph"))),
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
          
          actionButton(ns("Run_analysis"), "Run analysis")
        )
      ) # Absolutepanel
    ) #column
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
  
  
  ## Graph
  
  output$graph <- renderPlot({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({

      ROC <- ROC_curves(data(), y_var = input$y_var, x_var = input$x_var)
      
      ROC

    })
  })
  

    
    
  
  
 
}
    
## To be copied in the UI
# mod_ROC_ui("ROC_ui_1")
    
## To be copied in the server
# callModule(mod_ROC_server, "ROC_ui_1")
 
