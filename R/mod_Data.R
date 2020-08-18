#' Data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shiny NS tagList 
#' @import dplyr
mod_Data_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data loading"),
    
    strong("Data table should contains one line per observation (sample/patient) and one column per variable."),
    HTML("<ul><li> The first column should be the sample/patient identification column (it can be a simple ID such as 1-2-3-4-5...) </li>
                   <li> Each column must have a different name. </li>
                            <li> Categorical variables (e.g. Low-Intermediate-High) should contain at least one symbol or letter. 
                            Variable containing only numbers will be considered as numerical variable. </li>
                            <li> Numerical variables should contain only numerical values.
                            Decimals can be else dot or comma (do not forget to change the parameters in the right control-panel). </li>
                            <li> Missing values can be encoded with NA or an empty value. </li>
                            <li> Avoid special characters (%, @, $...)  </li> </ul>
                        
                            
                           
      
      Once correctly encoded, your dataframe should be saved as a tab (.txt/.tsv), comma or semicolon (.csv) delimited file. 
      
      Do not forget to select the apropriate parameters in control panel on the right.</li>
                            "),
    
    h2("Current dataset "),
    p("An example dataset (151 patients with Acute Myeloid Leukemia from The Cancer Genome Atlas database) is preloaded."),
    DT::DTOutput(ns("table_brut"))
 
  )
}
    
#' Data Server Function
#'
#' @noRd 
mod_Data_server <- function(input, output, session, r){
  ns <- session$ns
  ## Preview data  
  
  output$table_brut <- DT::renderDT(
    dplyr::select(r$test$data,-"Whole_cohort"), # data
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    server = T,
    rownames = FALSE,
    options = list(lengthChange = TRUE,
                   columnDefs = list(list(className = 'dt-left', targets = "_all")))
  )
  
  


  
}
    
## To be copied in the UI
# mod_Data_ui("Data_ui_1")
    
## To be copied in the server
# callModule(mod_Data_server, "Data_ui_1")
 
