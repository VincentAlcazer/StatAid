#' Explo_distri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Explo_distri_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(10,
          p("Red-labelled data points are diverging from more than 3 standard-derivations from the mean.
            "),
           box(title="Boxplot (Median, Interquartile Range)",
               plotOutput(ns("Boxplot")), collapsible=T),
           box(title="Barplot (Mean, standard-derivation)", 
               plotOutput(ns("Barchart")), collapsible=T),
           box(title="Histogram", 
               plotOutput(ns("Histogram")), collapsible=T),
           box(title="Density", 
               plotOutput(ns("Density")), collapsible=T),
           box(title=("Summary table: count, NA & Shapiro test"), 
               DT::DTOutput(ns("Summary_na_table")), collapsible = T)
    ),
    column(2,
           absolutePanel(width = 200, right = 20, draggable = T,
                         style = "opacity: 0.85",
                         wellPanel(
                           selectInput(ns("Group"), label =("Groups"),
                                       multiple = F,selected = NULL,
                                       ""),
                           selectInput(ns("Variable"), label =("Variable (numerical)"),
                                       multiple = F,selected = NULL,
                                       ""),
                           checkboxInput(ns("Include_na"), 
                                         label="Include NA in groups",value=F),
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
    
#' Explo_distri Server Function
#'
#' @noRd 
mod_Explo_distri_server <- function(input, output, session, r){
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
      choices=setdiff(c(names(data()[-1][sapply(data()[,-1], class) %in% c("numeric","double","integer")])),input$Group))
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
    # --- Z-scors & outliers detection
    #group=as.character(input$group)
    temp_df <- as.data.frame(data())

    df <- data.frame(Patient_id = temp_df$Patient_id,
                     Group=temp_df[,input$Group],
                     variable = temp_df[,input$Variable],
                     Zscores = abs(scale(temp_df[,input$Variable])))  %>%
      mutate(Outlier = Zscores > 3, label = "")
    
    df$label[df$Outlier == T & is.na(df$Outlier) == F] <- as.character(df$Patient_id[df$Outlier == T & is.na(df$Outlier) == F])
    
    if(input$Include_na == F){
      df <- df %>% filter(is.na(Group) == F)
    }
    
    return(df)
    
  })  
  
  
  
  ## Plots & tables
  
  output$Boxplot <- renderPlot({
    
    # --- Boxplot & scatterplot
    data_explo() %>%
      ggplot(aes(x=Group, y=variable, fill = Group )) +
      geom_boxplot(outlier.shape = NA,size=0.75) +
      geom_point(position = position_jitterdodge(0.2)) +
      geom_text_repel(aes(label=label),size=5, colour="red") +
      labs( y= paste0(input$Variable), x = "", fill = "Group") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })
  
  output$Barchart <- renderPlot({
    
    data_explo() %>%
      ggbarplot(x="Group", y="variable", fill = "Group", size=0.75, width=1,
                add = c("mean_sd", "jitter"), error.plot = "upper_errorbar") +
      geom_text_repel(aes(label=label),size=5, colour="red") +
      labs( y= paste0(input$Variable), x = "", fill = "Group") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })
  
  output$Histogram <- renderPlot({
    
    data_explo() %>%
      ggplot(aes(x = variable, fill = Group)) +
      geom_histogram(position="identity", color = "black", alpha = 0.5, size = 0.75) +
      #annotate("text",  x=Inf, y = Inf, label = PValueSW, vjust=2, hjust=1.1, size=5,fontface="bold") +
      labs(x = paste0(input$Variable), 
           y = "Count (n)") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })
  
  output$Density <- renderPlot({
    
    data_explo() %>%
      ggplot(aes(x = variable, fill = Group)) +
      geom_density(position="identity", color = "black", alpha = 0.5, size = 0.75) +
      #annotate("text",  x=Inf, y = Inf, label = PValueSW, vjust=2, hjust=1.1, size=5,fontface="bold") +
      labs(x = paste0(input$Variable), 
           y = "Density") +
      default_theme +
      theme(axis.text.x = legend.x(),
            legend.position = input$Legend_ext)
    
  })
  
  
  
  output$Summary_na_table <- DT::renderDT(
    
    data_explo() %>% 
      group_by(Group) %>%
      summarise(Count = n(),
                "NA" = sum(is.na(variable)),
                "Shapiro-Wilk pval" = ifelse(Count>3, format.pval(shapiro.test(variable)$p.value,digits=3, eps = 0.001),"NA")), # data
    class = "display nowrap compact", # style
    server = F,rownames = FALSE,
    options = list(lengthChange = TRUE)
  )
  
  
}
    
## To be copied in the UI
# mod_Explo_distri_ui("Explo_distri_ui_1")
    
## To be copied in the server
# callModule(mod_Explo_distri_server, "Explo_distri_ui_1")
 
