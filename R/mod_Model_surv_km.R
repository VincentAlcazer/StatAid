#' Model_surv_km UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Model_surv_km_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    
    column(
      12,
      h4("Analysis informations"),
      p("The status column must be coded either with 0-1 (no event-event) or TRUE/FALSE (TRUE = event) or 1/2 (2=event).
             The corresponding time column must contain only numeric values. "),
      p("For categorical X variables, each different category is considered as an independent group.
             For continuous/numeric X variables, you can select the number of groups to cut your variable in (e.g. 2 will cut at the median,
             3 at terciles...). /!\ Continuous/Numeric variables with less than 10 unique values will be considered as categorial!")

    ),
    column(10,
    tabsetPanel(
      id = "Explo", type = "tabs",
      tabPanel("KM curves",
               column(10, box(title = "KM curves", width = 10, plotOutput(ns("km_curves")))),
               
               ##### Graph param
               
               column(
                 10,
                 box(
                   title = "Graphical parameters", width = 11, collapsed = FALSE, collapsible = TRUE,
                   column(
                     4,
                     wellPanel(
                       strong("Global"),
                       textInput(ns("Title"), "Title", "Main title"),
                       sliderInput(ns("title_font_size"),
                                   label = "Title font size", min = 1,
                                   max = 35, value = 20, step = 1
                       ),
                       
                       selectInput(ns("Colors"),
                                   label = ("Colors panels"),
                                   choices = c("Default",
                                               "Grey levels" = "grey",
                                               "Nature" = "npg",
                                               "NEJM" = "nejm",
                                               "JCO" = "jco",
                                               "Lancet" = "lancet",
                                               "JAMA" = "jama",
                                               "AAAS" = "aaas"
                                   )
                       ),
                       strong("Risk table"),
                       checkboxInput(ns("show_risk_table"), label = "Show Risk table", value = TRUE),
                       sliderInput(ns("risk_table_font_size"),
                                   label = "Font size (values - title)", min = 1,
                                   max = 35, value = c(6, 8), step = 1
                       )
                     )
                   ),
                   column(
                     4,
                     wellPanel(
                       strong("X axis"),
                       textInput(ns("x_lab"), "Title", "Time"),
                       sliderInput(ns("x_font_size"),
                                   label = "Font size (values - title)", min = 1,
                                   max = 20, value = c(14, 18), step = 1
                       )
                     ) # Wellpanel
                   ), # Column
                   column(
                     4,
                     wellPanel(
                       strong("Y Axis"),
                       textInput(ns("y_lab"), "Title", "Event Free Survival (%)"),
                       sliderInput(ns("y_font_size"),
                                   label = "Font size (values - title)", min = 1,
                                   max = 35, value = c(14, 18), step = 1
                       )
                     ) # Wellpanel
                   ), # Column
                   
                   column(
                     4,
                     wellPanel(
                       strong("Legend"),
                       selectInput(ns("Legend"),
                                   label = ("Position"),
                                   choices = c(
                                     "Hide" = "none",
                                     "Top" = "top",
                                     "Right" = "right",
                                     "Left" = "left",
                                     "Bottom" = "bottom"
                                   ),
                                   selected = "right"
                       ),
                       textInput(ns("legend_title"), "Title", "Group"),
                       sliderInput(ns("legend_font_size"),
                                   label = "Font size (values - title)", min = 1,
                                   max = 35, value = c(12, 14), step = 1
                       )
                     ) # Wellpanel
                   ), # Column
                   column(
                     4,
                     wellPanel(
                       strong("Statistics"),
                       checkboxInput(ns("show_pval"), label = "Show p-value", value = TRUE),
                       checkboxInput(ns("show_ci"), label = "Show confidence intervals", value = FALSE),
                       sliderInput(ns("pval_font_size"),
                                   label = "Font size", min = 0,
                                   max = 35, value = 6
                       ),
                       sliderInput(ns("pval_x_coord"),
                                   label = "X position", min = 0,
                                   max = 100, value = 1
                       ),
                       sliderInput(ns("pval_y_coord"),
                                   label = "Y position", min = 0,
                                   max = 100, value = 5, step = 1
                       )
                     ) # Wellpanel
                   ) # Column
                   
                 ) #box
               ) #graph panel box
      ), #tabpanel KM curve
      
      tabPanel("Summary table",
               p("The total number of patients at initial follow-up (n.start), number of event and median survival is shown for each group."),
               downloadButton(ns("download_summary"), "Download table (.tsv)"),
               p(),
               column(10, DT::DTOutput(ns("median_survival")))
      ),

      tabPanel("Full table",
               p("The percent of event-free population (=estimate) with its 95% CI (conf low - conf high) 
               is shown at each time-point."),
               downloadButton(ns("download_full"), "Download table (.tsv)"),
               p(),
               column(10, DT::DTOutput(ns("table_surv")))
               )
    ) #tabsetpanel
    
    ),#column
      ##### Data param
      column(
        2,
        absolutePanel(
          width = 200, right = 20, top = 50, draggable = T,
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
                        label = ("X Variable (explanatory/predictive)"),
                        multiple = F, selected = NULL,
                        choices = c("")
            ),
            sliderInput(ns("x_cut"),
                        label = ("Number of groups to cut X in (X numeric only)"),
                        min = 2, max = 10, value = 2, step = 1
            ),
            
            actionButton(ns("Run_analysis"), "Run analysis")
          )
        ) # Absolutepanel
      ) #column
      


 
  )
}

#' Model_surv_km Server Function
#'
#' @noRd
mod_Model_surv_km_server <- function(input, output, session, r) {
  ns <- session$ns

  ##### ===== Reactives elements

  data <- reactive(r$test$data)

  surv_object <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)

    isolate({
      data_km <- data() %>%
        select(time = one_of(input$time_var), status = one_of(input$y_var), x_var = one_of(input$x_var))

      if (is.numeric(data_km$x_var) & length(unique(data_km$x_var)) > 8) {
        data_km <- data_km %>%
          mutate(x_var = cut(x_var, input$x_cut))
      }

      # formula <- as.formula(sprintf(paste0("Surv(",input$time_var,",",input$status_var,")~",input$x_var)))
      # formula <- as.formula("Surv(time,status)~x_var")

      surv <- surv_fit(Surv(time, status) ~ x_var, data = data_km)
      return(surv)
    })
  })
  
  surv_df <- reactive({
    
    req(surv_object())
    dat <- broom::tidy(surv_object()) %>%
      mutate_if(is.numeric, function(x){round(x,3)})
    
    return(dat)
    
  })
  
  legend_labs <- reactive({
    
    input$Run_analysis
    req(input$Run_analysis >= 1)
    
    isolate({
    if(input$x_var == "Whole_cohort"){
      legend_labs = "Overall cohort"
    } else {
      legend_labs = gsub("x_var=","",names(surv_object()$strata))
    }
    return(legend_labs)
    })
    
  })

  ## Parameters update

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
      choices = c(setdiff(names(data()[sapply(data(), class) %in% c("numeric", "integer","character","factor")]), c(input$time_var)))
    )
  })

  observe({
    updateSelectInput(
      session,
      "x_var",
      choices = c(setdiff(names(data()), c(input$y_var, input$time_var)))
    )
  })

  observe({
    updateTextInput(
      session,
      "legend_title",
      value = input$x_var
    )
  })

  observe({
    updateSliderInput(
      session,
      "pval_x_coord",
      min = 0, max = max(surv_object()[["time"]]), step = max(surv_object()[["time"]]) / 100
    )
  })

  ##### ===== Outputs

  output$km_curves <- renderPlot(
    ggsurvplot(surv_object(),
      fun = "pct",
      title = input$Title,
      ylab = input$y_lab,
      xlab = input$x_lab,
      # legend= c(0.8,0.3),
      legend = input$Legend,
      font.title = c(input$title_font_size, "bold", "black"),
      # font.tickslab = c(14,"plain","black"),
      legend.title = input$legend_title,
      legend.labs = legend_labs(),
      linetype = 1, size = 1,
      # censor.size = 10,
      conf.int = input$show_ci,
      pval = input$show_pval, pval.method = input$show_pval,
      pval.size = input$pval_font_size,
      pval.coord = c(input$pval_x_coord, input$pval_y_coord),
      pval.method.size = input$pval_font_size,
      pval.method.coord = c(input$pval_x_coord, input$pval_y_coord + 15),
      risk.table = input$show_risk_table,
      risk.table.title.fontsize = input$risk_table_font_size[2],
      risk.table.font.title = c(input$risk_table_font_size[2], "bold", "black"),
      risk.table.fontsize = input$risk_table_font_size[1],
      tables.theme = theme_cleantable(),
      risk.table.y.text = FALSE,
      surv.median.line = "v",
      palette = input$Colors,
      ggtheme = theme_classic() +
        theme(
          axis.title.x = element_text(size = input$x_font_size[2], face = "bold", color = "black"),
          axis.text.x = element_text(size = input$x_font_size[1]),
          axis.title.y = element_text(size = input$y_font_size[2], face = "bold", color = "black"),
          axis.text.y = element_text(size = input$y_font_size[1]),
          legend.title = element_text(size = input$legend_font_size[2], face = "bold", color = "black"),
          legend.text = element_text(size = input$legend_font_size[1])
        )
    )
  )
  
  output$median_survival <-  DT::renderDT(
    summary(surv_object())$table %>% as.data.frame(check.names=F) %>% rownames_to_column("var"),
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
  
  
  output$table_surv <- DT::renderDT(
    surv_df(),
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
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("Survival_summary_table.tsv")
    },
    content = function(file) {
      
      write.table(summary(surv_object())$table %>% as.data.frame(check.names=F)  %>% rownames_to_column("var"), file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
  
  output$download_full <- downloadHandler(
    filename = function() {
      paste("Survival_full_table.tsv")
    },
    content = function(file) {
      
      write.table(surv_df(), file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
}

## To be copied in the UI
# mod_Model_surv_km_ui("Model_surv_km_ui_1")

## To be copied in the server
# callModule(mod_Model_surv_km_server, "Model_surv_km_ui_1")
