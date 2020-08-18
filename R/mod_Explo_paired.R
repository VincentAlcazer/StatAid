#' Explo_paired UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Explo_paired_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(11,
      p(strong("Data format:"), "each row correspond to an observation (sample/patient), each selected variable/column to a different timepoint of the same variable 
        (e.g. Concentration_day0, concentration_day15, concentration_day30...)"),
      htmlOutput(ns("analysis_info")),
      ),

          column(8,offset=1, 
                                         box(width=11, plotOutput(ns("Graph_comp"))),
                           ),
        column(2,
                                  absolutePanel(width = 200, right = 20, draggable = T,
                                                style = "opacity: 1; z-index: 10 !important ;",
                                                wellPanel(strong("GRAPH TYPE"),
                                               selectInput(ns("Variable"), label =("Variable timepoints (numerical)"),
                                                              multiple = T,selected = NULL,
                                                              ""),
                                               selectInput(ns("Group"), label =("Groups"),
                                                           multiple = F,selected = "None",
                                                           "None"),
                                               
                                               selectInput(ns("Graphtype"), label =("Graph type"),
                                                           multiple = F,selected = "Mean_lines",
                                                           c("Scatter with lines"= "Mean_lines",
                                                             "Boxplot")),
                                               radioButtons(ns("Error"), "Error bars (Barchart)",
                                                            choices = c("Hide" = "hide",
                                                                        "Standard derivation" = "sd",
                                                                        "Standard error" = "se",
                                                                        "95% CI" = "IC95"),
                                                            selected = "IC95"),
                                               radioButtons(ns("Stat"), "Statistics",
                                                            choices = c("Hide" = "no",
                                                                        "Parametrics" = "param",
                                                                        "Non parametrics" = "non_param"),
                                                            selected = "no"),
                                               checkboxInput(ns("Show_points"), 
                                                             label="Show data points",value=F),
                                               checkboxInput(ns("Show_lines"), 
                                                             label="Show mean lines",value=T),
                                               checkboxInput(ns("Show_indiv_lines"), 
                                                             label="Show individual lines",value=F),
                                               
                                               sliderInput(ns("line_opacity"), label = "Individual lines opacity", min = 0, 
                                                           max = 1, value = 0.5,step=0.1),
                                               
                                               
                                               
                                               
                                                 actionButton(ns("Run_analysis"),"Run")
                                                  
                                                )
                                  )#Absolutepanel
                           ),  #Column 2
                  
                  ##### Graph param
                  
                  column(10,
                         box(title = "Graphical parameters", width = 11, collapsed = FALSE, collapsible = TRUE,
                             column(4,
                                    wellPanel(strong("Global"),
                                              textInput(ns("Title"),"Title","Main title"),
                                              sliderInput(ns("title_font_size"), label = "Title size", min = 1, 
                                                          max = 35, value = 18,step=1),
                                              selectInput(ns("Legend"), label =("External legend"),
                                                          choices=c("None"= "none",
                                                                    "Top"= "top",
                                                                    "Right"=  "right",
                                                                    "Left"=  "left",
                                                                    "Bottom"=   "bottom"),
                                                          selected = "right"),
                                              selectInput(ns("Colors"), label =("Colors palette"),
                                                          choices=c("Default",
                                                                    "Grey levels"= "grey",
                                                                    "Nature" =  "npg",
                                                                    "NEJM"=  "nejm",
                                                                    "JCO"=   "jco",
                                                                    "Lancet" = "lancet",
                                                                    "JAMA" = "jama",
                                                                    "AAAS" = "aaas")),
                                              textInput(ns("legend_title"),"Legend","Legend title"),
                                              sliderInput(ns("legend_font_size"), label = "Legend size (values - title)", min = 1, 
                                                          max = 35, value = c(12,14),step=1)
                                              
                                    )
                             ),
                             column(4,
                                    wellPanel(strong("X axis"),
                                              textInput(ns("x_lab"),"Title",""),
                                              sliderInput(ns("x_font_size"), label = "Size (values - title)", min = 1, 
                                                          max = 35, value = c(12,14),step=1),
                                              sliderInput(ns("x_angle"), label = "Rotation", min = 0, 
                                                          max = 90, value = 0, step=1)
                                              
                                    )#Wellpanel
                             ),#Column
                             column(4,
                                    wellPanel(strong("Y axis"),
                                              textInput(ns("y_lab"),"Title",""),
                                              sliderInput(ns("y_font_size"), label = "Size (values - title)", min = 1, 
                                                          max = 35, value = c(12,14),step=1),
                                              sliderInput(ns("y_limits"), label = "Limits", min = 0, 
                                                          max = 100, value = c(0,50)),
                                    )#Wellpanel
                             )#Column
                         )
                         
                  )
  ) # fluid row
  ) #tag list
}
    
#' Explo_paired Server Function
#'
#' @noRd 
mod_Explo_paired_server <- function(input, output, session, r){
  ns <- session$ns
  
  data <- reactive(r$test$data)
  
  ## Parameters
  observe({
    updateSelectInput(
      session,
      "Group",
      choices=c("None", names(data()[sapply(data(), class) %in% c("factor","character")])[-1]))
    
  })
  
  
  observe({
    updateSelectInput(
      session,
      "Variable",
      choices=setdiff(c(names(data()[-1][sapply(data()[,-1], class) %in% c("numeric","double","integer")])),input$Group))
  })


  ## reactive elements
  
  res <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    
    isolate({
      
      
      # autoplot_paired(data,timepoints,
      #                 group,
      #                 plot_type="Mean_lines",add_points=F, add_lines=T,add_individual_lines = F,
      #                 stat = "non_param",
      #                 alpha_line=0.4)
      
      
      res <- autoplot_paired(data(), 
                      group=input$Group,
                      timepoints=input$Variable, 
                      plot_type = input$Graphtype, 
                      stat = input$Stat, 
                      add_points = input$Show_points, 
                      add_lines=input$Show_lines,
                      add_individual_lines = input$Show_indiv_lines,
                      error_bar=input$Error,
                      alpha_line=input$line_opacity)
      
      
    })
  })
  
  p <- reactive({
    res()[["graph"]]
  })
  

  ## Output 
  
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
  
  
  
  
  ## Plots & tables

  output$Graph_comp <- renderPlot({
    
    p <- p() + theme_bw() +
      labs(title = input$Title, y = input$y_lab, x = input$x_lab, fill = input$legend_title) +
      theme(
        plot.title = element_text(size=input$title_font_size, face="bold"),
        axis.text.x = element_text(size=input$x_font_size[1], angle = input$x_angle, vjust=0.5,hjust=0.5, color="black"),
        axis.title.x = element_text(size=input$x_font_size[2], face="bold"),
        axis.text.y = element_text(size=input$y_font_size[1], color="black"),
        axis.title.y = element_text(size=input$y_font_size[2], face="bold"),
        legend.title = element_text(size=input$legend_font_size[2],face="bold"),
        legend.text = element_text(size = input$legend_font_size[1]),
        legend.position = input$Legend) 
    
    
    if(input$Colors == "Default"){
      p
    }else if(input$Colors == "grey"){
      p + scale_fill_grey()
    }else {
      set_palette(p, input$Colors)
    }
    
    
  })
  


  
  output$analysis_info <- renderText({
    paste(paste0("<b>Methods :</b>  Numerical variables are expressed as mean (",input$Error,") for default plot or median [IQR] for boxplots."),
    "<br>   - For intra-groups analysis (i.e. no groups selected), data are compared with the paired Welch t-test (or its non parametric alternative the paired Wilcoxon rank-sum test) for up to two measures, 
    or with an ANOVA for repeated measures (or its non parametric alternative the Friedman test) for more than two measures.",
    "<br>   - For inter-groups analysis (i.e. 2 or more groups to compare), data are compared  with a two ways ANOVA for repeated measures. 
    /!!\\ Be aware that this test has a multiple assumptions that may not be met.", sep = "\n"
    )

    
  })
  

 
}
    
## To be copied in the UI
# mod_Explo_paired_ui("Explo_paired_ui_1")
    
## To be copied in the server
# callModule(mod_Explo_paired_server, "Explo_paired_ui_1")
 
