#' Signature_calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Signature_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(12, 
           box(
             title = "Files input",collapsible = T,
             actionButton(ns("load_gene"), "Load dataset"),
                 fileInput(ns("genexp_df"),
                           label = "Gene expression data",
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
             
             
             actionButton(ns("load_sig"), "Load dataset"),
             fileInput(ns("sig_df"),
                       label = "Signature genes & coefs",
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
            
             
                 radioButtons(ns("sep"), "File type (delim)",
                              choices = c(
                                "comma-delim (.csv1)" = ",",
                                "semi colon-delim (.csv2)" = ";",
                                "tab-delim (.tsv/.txt)" = "\t",
                                "Excel (.xls/.xlsx)" = "xl" 
                              ),
                              selected = "\t"
                 ),
                 radioButtons(ns("dec"), "Decimal",
                              choices = c(
                                "Comma (,)" = ",",
                                "Period (.)" = "."
                              ),
                              selected = ","
                 )
             
           ), #box
           box(
             title = "Computed Signature",collapsible = T,
             textOutput(ns("text")),
             actionButton(ns("calc_sig"), "Calculate signature"),
             downloadButton(ns("download"), "Download table (.tsv)"),
             DT::DTOutput(ns("table_sig_calc"))
             
           )
           
           
           ),# column
    
   
    column(6,
      h2("Gene exp dataset preview:"),
      p("//!\\ First row = samples/patient ID, First column = gene_id,
      All other cells should be numeric values only!"),
      DT::DTOutput(ns("table_gene"))
      
    ),
    column(6,
           h2("Signature param preview:"),
           
           DT::DTOutput(ns("table_sig"))
           
    )
    
  )
}
    
#' Signature_calc Server Function
#'
#' @noRd 
mod_Signature_calc_server <- function(input, output, session){
  ns <- session$ns
  
  
  data_gene <- reactive({
    
    input$load_gene
    req(input$load_gene >= 1)
    isolate({
    
    if (is.null(input$genexp_df) == T) {
      dat <- readxl::read_xlsx("datasets/TCGA_LAML_RSEM_log2.xlsx") %>%
        mutate_at(-1, as.numeric)
      colnames(dat)[1] <- "gene_id"
    } else {
      
      if(input$sep == "xl"){
        
        dat <- readxl::read_xlsx(input$genexp_df$datapath)%>%
          mutate_at(-1, as.numeric)
        colnames(dat)[1] <- "gene_id"
      } else {
        
        dat <- data.table::fread(input$genexp_df$datapath, sep = input$sep, dec = input$dec, na.strings = c("", "NA", "#N/A"), stringsAsFactors = T, data.table = F)%>%
          mutate_at(-1, as.numeric)
        colnames(dat)[1] <- "gene_id"
      }
 
    }
    return(dat)
  }) # isolate
  }) # reactive
    
  
  data_sig <- reactive({
    
    input$load_sig
    req(input$load_sig >= 1)
    isolate({
    if (is.null(input$sig_df) == T) {
      dat <- data.table::fread("datasets/LSC17_signature.txt", data.table = F)
      colnames(dat)[1] <- "gene_id"
    } else {
      
      if(input$sep == "xl"){
        
        dat <- readxl::read_xlsx(input$sig_df$datapath)
        colnames(dat)[1] <- "gene_id"
      } else {
        
        dat <- data.table::fread(input$sig_df$datapath, sep = input$sep, dec = input$dec, na.strings = c("", "NA", "#N/A"), stringsAsFactors = T, data.table = F)
        colnames(dat)[1] <- "gene_id"
      }
      
    }
    return(dat)
      return(dat)
    }) # isolate
  }) # reactive
  
  
  
  
  output$table_gene <- DT::renderDT(
    data_gene()[1:5,1:5], # data
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
  
  output$table_sig <- DT::renderDT(
    data_sig(), # data
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
 
  
  sig_calc <- reactive({
    
    input$calc_sig
    req(input$calc_sig >= 1)
    isolate({
      
      genes_sig <- as.character(unlist(dplyr::select(data_sig(), 1)))
      genes_from_xp <- as.character(unlist(dplyr::select(data_gene(), 1)))
      
      intersect_genes <- intersect(genes_sig, genes_from_xp)
      
      coefs_df <- data_sig() %>% column_to_rownames("gene_id") %>% as.matrix()

      coefs <- as.numeric(unlist(coefs_df[intersect_genes,]))
      
      gene_xp <- data_gene() %>% filter(gene_id %in% intersect_genes) %>%
        group_by(gene_id) %>% summarise_all(mean) %>%
        column_to_rownames("gene_id") %>% as.matrix()
      
      sig_calc <- gene_xp[intersect_genes,] * coefs
      
      sig_final <- data.frame(Score = colSums(sig_calc)) %>%
        rownames_to_column("Sample_ID") %>%
        mutate(Median_cut = factor(cut(Score,2),labels = c("Low","High")),
               Tercile_cut = factor(cut(Score,3),labels = c("Low","Intermediate", "High")),
               Quartile_cut = factor(cut(Score,4),labels = c("Low","Intermediate1","Intermediate2", "High")))
      
      return(sig_final)
    })
  })
  
  missing_genes <- reactive({
    
    input$calc_sig
    req(input$calc_sig >= 1)
    isolate({
      
      genes_sig <- as.character(unlist(dplyr::select(data_sig(), 1)))
      genes_from_xp <- as.character(unlist(dplyr::select(data_gene(), 1)))
      
      missing_genes <- setdiff(genes_sig, genes_from_xp)
      
    
      return(missing_genes)
    })
  })
  
  output$text <- renderText({

      paste0("Genes found in signature but not in gene expression dataset: ",
             missing_genes())
    
    
    
    
    })
  
  
  output$table_sig_calc <- DT::renderDT(
    sig_calc(), # data
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
      
      write.table(sig_calc(), file, row.names = FALSE, sep = "\t", quote = F)
    }
  )
  
}
    
## To be copied in the UI
# mod_Signature_calc_ui("Signature_calc_ui_1")
    
## To be copied in the server
# callModule(mod_Signature_calc_server, "Signature_calc_ui_1")
 
