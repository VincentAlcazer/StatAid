#' Explo_table_des UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Explo_table_des_ui <- function(id) {
  ns <- NS(id)
  tagList(
fluidPage(
    column(
      10,
      h4("Analysis informations"),
      htmlOutput(ns("analysis_info"))
    ),

    column(
      10,
      DT::DTOutput(ns("Table_des"))
    ),
    column(
      2,
      absolutePanel(
        width = 200, right = 20, draggable = T,
        style = "opacity: 0.85",
        wellPanel(
          selectInput(ns("Group"),
            label = ("Groups"),
            multiple = F, selected = NULL,
            ""
          ),
          checkboxInput(ns("Include_na"),
            label = "Include NA in groups", value = F
          ),
          selectInput(ns("Variable"),
            label = ("Variable(s)"),
            multiple = T, selected = "All",
            "All"
          ),
          checkboxInput(ns("Percent_by_row"),
            label = "Percent by row", value = F
          ),
          radioButtons(ns("Adjust_pval"), "Adjust p-values:",
            choices = list("No" = "none", "Benjamini Hochberg (FDR)" = "fdr", "Holm" = "holm", "Bonferroni" = "bonferroni"),
            selected = "none"
          ),
          actionButton(ns("Run_analysis"), "Run analysis"),
          p(),
          downloadButton(ns("download"), "Download table (.tsv)")
        )
      ) # Absolutepanel
    ) # Column
  ) #fluidpage
)
}

#' Explo_table_des Server Function
#'
#' @noRd
mod_Explo_table_des_server <- function(input, output, session, r) {
  ns <- session$ns


  data <- reactive(r$test$data)


  ## Parameters

  observe({
    updateSelectInput(
      session,
      "Group",
      choices = c(names(data()[-1][sapply(data()[, -1], class) %in% c("factor", "character")]))
    )
  })

  observe({
    updateSelectInput(
      session,
      "Variable",
      choices = c("All", names(data()[-1][!names(data()[, -1]) %in% c("Whole_cohort")])),
      selected = "All"
    )
  })


  ## DF

  table_des_df <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({

      # group=as.character(input$Group)

      if (input$Group == "Whole_cohort") {
        data_sort <- data() %>% select(-all_of(c("Patient_id")))
      } else {
        data_sort <- data() %>% select(-all_of(c("Whole_cohort", "Patient_id")))
      }

      if (input$Variable != "All") {
        data_sort <- select(data_sort, one_of(input$Group), one_of(input$Variable))
      }

      df <- descriptive_table(data_sort,
        group = input$Group, na.include = input$Include_na, padj_method = input$Adjust_pval,
        percent_type = ifelse(input$Percent_by_row == T, 2, 1)
      )

      return(df)
    })
  })
  
  ## Output

  output$analysis_info <- renderText({
    paste(
      paste0("<b>Methods :</b> Categorical variables are expressed as n (%) and compared with the Chi-squared test or its non-parametric alternative Fisher's test with simulated p-values. 
    Numerical variables are expressed as mean (standard-deviation) or median [Interquartile Range] and compared with either Welch's t-test (or its non-parametric alternative Wilcoxon's rank-sum test) or 
    ANOVA (or its non-parametric alternative Kruskal-Wallis test) where appropriate.
                 Variables with >80% missing are removed from the analysis."),
      if_else(input$Adjust_pval == "none", "P-values are not adjusted.", paste0("P-values are adjusted with the ", as.character(input$Adjust_pval), " method."))
    )
  })

  output$Table_des <- DT::renderDT(
    table_des_df(),
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
      paste("Descriptive_table.tsv")
    },
    content = function(file) {
      
      write.table(table_des_df(), file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
}

## To be copied in the UI
# mod_Explo_table_des_ui("Explo_table_des_ui_1")

## To be copied in the server
# callModule(mod_Explo_table_des_server, "Explo_table_des_ui_1")
