#' Data_merge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_merge_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(12, 
           box(
             title = "File A",collapsible = T,
         
             fileInput(ns("file_a"),
                       label = NULL,
                       accept = c(
                         "text/tab-separated-values",
                         "text/comma-separated-values",
                         "text/plain",
                         "text/csv",
                         ".csv",
                         ".tsv",
                         ".xls",
                         ".xlsx"
                       )
             ),
             div(style = "margin-top: -25px"),
             radioButtons(ns("A_sep"), "File type (delim)",
                          choices = c(
                            "comma-delim (.csv1)" = ",",
                            "semi colon-delim (.csv2)" = ";",
                            "tab-delim (.tsv/.txt)" = "\t",
                            "Excel (.xls/.xlsx)" = "xl" 
                          ),
                          selected = "\t"
             ),
             actionButton(ns("load_A"), "Load file")
             
           ), #box
           box(
             title = "File B",collapsible = T,
       
             fileInput(ns("file_b"),
                       label = NULL,
                       accept = c(
                         "text/tab-separated-values",
                         "text/comma-separated-values",
                         "text/plain",
                         "text/csv",
                         ".csv",
                         ".tsv",
                         ".xls",
                         ".xlsx"
                       )
             ),
             div(style = "margin-top: -25px"),
             radioButtons(ns("B_sep"), "File type (delim)",
                          choices = c(
                            "comma-delim (.csv1)" = ",",
                            "semi colon-delim (.csv2)" = ";",
                            "tab-delim (.tsv/.txt)" = "\t",
                            "Excel (.xls/.xlsx)" = "xl" 
                          ),
                          selected = "\t"
             ),

             actionButton(ns("load_B"), "Load file")
             
           ) #box
           
           
           # box(
           #   title = "Computed Signature",collapsible = T,
           #   textOutput(ns("text")),
           #   actionButton(ns("calc_sig"), "Calculate signature"),
           #   downloadButton(ns("download"), "Download table (.tsv)"),
           #   DT::DTOutput(ns("table_sig_calc"))
           #   
           # )
           
           
    ),# column
    column(12,
           
    box(title = "Files key & mergin parameters",
           selectInput(ns("A_key"),
                       label = ("File A key"),
                       multiple = F, selected = NULL,
                       ""
           ),
           selectInput(ns("B_key"),
                       label = ("File B key"),
                       multiple = F, selected = NULL,
                       ""
           ),
          selectInput(ns("join_type"),
                    label = ("Join type"),
                    choices = c("Inner join","Left (A) join", "Full join"),
                    multiple = F, selected = "Inner join"
                    
        ),
        
           actionButton(ns("merge"), "Merge files"),
           downloadButton(ns("download"), "Download merged file (.tsv)"),
           DT::DTOutput(ns("missing"))
           ), #column
    
     box(title = "Merged file preview",
            DT::DTOutput(ns("table_merged"))
            
            )      
           
       ),# column
    column(6,
           h2("File A preview:"),
           DT::DTOutput(ns("table_a"))
           
    ),
    column(6,
           h2("File B preview:"),
           
           DT::DTOutput(ns("table_b"))
           
    )
    
  )
  
}
    
#' Data_merge Server Functions
#'
#' @noRd 
mod_Data_merge_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    file_a <- reactive({
      
      input$load_A
      req(input$load_A >= 1)
      req(input$file_a)
      isolate({
        

          if(input$A_sep == "xl"){
            
            dat <- readxl::read_xlsx(input$file_a$datapath)
            
          } else {
            
            dat <- data.table::fread(input$file_a$datapath, sep = input$A_sep, na.strings = c("", "NA", "#N/A"), stringsAsFactors = F, data.table = F)
            
          }
          
        
        return(dat)
      }) # isolate
    }) # reactive
    
    
    file_b <- reactive({
      
      input$load_B
      req(input$load_B >= 1)
      req(input$file_b)
      isolate({
        
        
        if(input$B_sep == "xl"){
          
          dat <- readxl::read_xlsx(input$file_b$datapath)
          
        } else {
          
          dat <- data.table::fread(input$file_b$datapath, sep = input$B_sep, na.strings = c("", "NA", "#N/A"), stringsAsFactors = F, data.table = F)
          
        }
        
        
        return(dat)
      }) # isolate
    }) # reactive
    
    merged_file <- reactive({
      req(input$load_A >= 1)
      req(input$load_B >= 1)
      req(input$file_b)
      req(input$file_a)
      req(input$merge)
      isolate({
        

        
        if(input$join_type == "Inner join"){
          merge_file <- file_a() %>% 
            select(key = input$A_key, everything()) %>%
            inner_join(select(file_b(), key = input$B_key, everything()), by = "key")
          
        } else if(input$join_type == "Left (A) join"){
          
          merge_file <- file_a() %>% 
            select(key = input$A_key, everything()) %>%
            left_join(select(file_b(), key = input$B_key, everything()), by = "key")
          
        } else if(input$join_type == "Full join"){
          
          merge_file <- file_a() %>% 
            select(key = input$A_key, everything()) %>%
            full_join(select(file_b(), key = input$B_key, everything()), by = "key")
          
          
        }
        
        return(merge_file)
       
        
        
      })
      
    })
    
    ## File keys
    observe({
      updateSelectInput(
        session,
        "A_key",
        choices = colnames(file_a())
      )
    })
    
    observe({
      updateSelectInput(
        session,
        "B_key",
        choices = colnames(file_b())
      )
    })
    
    
    
    output$table_a <- DT::renderDT(
      file_a(), # data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      server = T,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )
    
    output$table_b <- DT::renderDT(
      file_b(), # data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      server = T,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )
    
    output$table_merged <- DT::renderDT(
      merged_file(), # data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      server = T,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )
    
    
    missing_pts <- reactive({
      
      input$calc_sig
      req(input$calc_sig >= 1)
      isolate({
        
        A_samples <- file_a() %>% select(key = input$A_key) %>% distinct(key) %>% unlist() %>% as.character() 
        B_samples <- file_b() %>% select(key = input$B_key) %>% distinct(key) %>% unlist() %>% as.character() 
        
       missing <- data.frame(A_not_B = length(setdiff(A_samples, B_samples)),
                             B_not_A = length(setdiff(B_samples,A_samples)))
        
        return(missing)
      })
    })
    
    output$missing <- DT::renderDT(
      missing_pts(), # data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      server = T,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )
    
    
  
    
    
    # Download table
    output$download <- downloadHandler(
      filename = function() {
        paste("Signature_score.tsv")
      },
      content = function(file) {
        
        write.table(merged_file(), file, row.names = FALSE, sep = "\t", quote = F)
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_Data_merge_ui("Data_merge_ui_1")
    
## To be copied in the server
# mod_Data_merge_server("Data_merge_ui_1")
