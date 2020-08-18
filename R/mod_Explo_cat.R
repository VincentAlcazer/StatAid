#' Explo_cat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Explo_cat_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(10,
           box(title="Barchart (count)",
               plotOutput(ns("Barchart_count")), collapsible=T),
           box(title="Barchart (percent)",
               plotOutput(ns("Barchart_percent")), collapsible=T),
           box(title=("Summary table: count, NA"), 
               DT::DTOutput(ns("Summary_na_table")), collapsible = T)
    ),
    column(2,
           absolutePanel(width = 200, right = 20, draggable = T,
                         style = "opacity: 0.85",
                         wellPanel(
                           selectInput(ns("Group"), label =("Groups"),
                                       multiple = F,selected = NULL,
                                       ""),
                           selectInput(ns("Variable"), label =("Variable (categorical)"),
                                       multiple = F,selected = NULL,
                                       ""),
                           checkboxInput(ns("Include_na"), 
                                         label="Include NA in groups",value=F),
                           checkboxInput(ns("Include_na_var"), 
                                         label="Include NA in variable",value=F),
                           selectInput(ns("Legend_ext"), label =("External legend"),
                                       choices=c("No"= "none",
                                                 "Top"= "top",
                                                 "Right"=  "right",
                                                 "Left"=  "left",
                                                 "Bottom"=   "bottom"),
                                       multiple = F,selected = "bottom"),
                           selectInput(ns("Legend_x"), label =("X axis legend"),
                                       choices=c("Hide","Normal",paste0("45",intToUtf8(0176))),
                                       multiple = F,selected = "Normal")
                           
                         )
           )#Absolutepanel
    )#Column
    
    
  )
}
    
#' Explo_cat Server Function
#'
#' @noRd 
mod_Explo_cat_server <- function(input, output, session, r){
  ns <- session$ns
 
  data <- reactive(r$test$data)

  ## Parameters
  observe({
    updateSelectInput(
      session,
      "Group",
      choices=c(names(data()[-1][sapply(data()[,-1], class) %in% c("factor","character")])))

  })
  
  observe({
    updateSelectInput(
      session,
      "Variable",
      choices=setdiff(c(names(data()[-1][sapply(data()[,-1], class) %in% c("factor","character")])),input$Group))
  })
  
  
  legend.x = reactive({
    if(input$Legend_x == "Hide"){
      leg=element_blank()
    }
    if(input$Legend_x == "Normal"){
      leg = element_text(size=12, color="black")
    }
    if(input$Legend_x == paste0("45",intToUtf8(0176))){
      leg = element_text(size=12, color="black", angle=45, vjust=1, hjust=1)
    }
    return(leg)
    
  }) 
  
  
  
  ## DF
  data_explo = reactive({
    # --- Filter out NA in variable
    if(input$Include_na == F){
      df <- data() %>% filter_at(.vars=input$Group, all_vars(is.na(.) == F))
    } else {
      df <- data()
    }
    
    if(input$Include_na_var == F){
      df <- df %>% filter_at(.vars=input$Variable, all_vars(is.na(.) == F))
    }
    
    
    return(df)
    
  })  
  
  
  ## Plots & tables
  
  output$Barchart_count <- renderPlot({
    
    # --- Barchart with count
    data_explo() %>%
      ggplot(aes_string(x = input$Variable, fill = input$Group)) +
      geom_bar(stat = "count", position = "dodge", color = "black", size = 0.75) +
      labs(y = "Count") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })
  
  output$Barchart_percent <- renderPlot({
    
    # --- Barchart with percent
    data_explo() %>%
      group_by_at(c(input$Group,input$Variable)) %>%
      summarise(count = n()) %>%
      ggplot(aes_string(x= input$Group, y = "count", fill = input$Variable)) +
      geom_bar(stat = "identity", position = "fill", color = "black", size = 0.75) +
      labs(y = "Percent") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })

  output$Summary_na_table <- DT::renderDT(
    
    data_explo() %>% 
      group_by_at(input$Group) %>%
      summarise(Count = n(),
                "NA" = sum(is.na(input$Variable))), # data
    class = "display nowrap compact", # style
    server = F,rownames = FALSE,
    options = list(lengthChange = TRUE)
  )
  
  
}
    
## To be copied in the UI
# mod_Explo_cat_ui("Explo_cat_ui_1")
    
## To be copied in the server
# callModule(mod_Explo_cat_server, "Explo_cat_ui_1")
 
