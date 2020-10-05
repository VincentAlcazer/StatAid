#' Data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @import dplyr
mod_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Data loading"),

    strong("Data table should contain one line per observation (sample/patient) and one column per variable."),
    HTML("<ul><li> The first column should be the sample/patient identification column (it can be a simple ID such as 1-2-3-4-5...) </li>
                   <li> Each column must have a different name. </li>
                            <li> Categorical variables (e.g. Low-Intermediate-High) should contain at least one symbol or letter. 
                            Variable containing only numbers will be considered as numerical variable. </li>
                            <li> Numerical variables should contain only numerical values.
                            Decimals can be either dot or comma (do not forget to change the parameters in the right control-panel). </li>
                            <li> Missing values can be encoded with NA or an empty value. </li>
                            <li> Avoid using special characters in your variables (&, $...)  </li> </ul>
                        
                            
                           
      
      Once correctly encoded, <b> your dataframe should be saved as a tab (.txt/.tsv), comma or semicolon (.csv) delimited file.</b> 
      
      Do not forget to select the appropriate parameters in control panel on the right. 
	  
	  If you do not manage to load your dataset please check the aforementioned instructions.
	  
                            "),
    h3("Frequent issues"),

    HTML("<ul><li><b> Disconnected from the server when trying to load the dataset :</b> Your dataset has probably two columns with the exact same name. </li>
        <li><b> Red bar with <!DOCTYPE html> error: </b> At least one of your variable includes special characters. Characters with known issues: &, $ ... </li>
        <li><b> No numerical variable found in modules: </b> Please check that you have correctly selected the decimal separator (comma / period) <b>before</b> loading your data.</li>
		</ul>

"),


    h2("Current dataset "),
    p("An example dataset (151 patients with Acute Myeloid Leukemia from The Cancer Genome Atlas database) is preloaded. 
      FAB, ELN2017 and Karyotype are three categorical variables relevant for disease classification. 
      BM_BLAST_PERCENTAGE,	WBC and	PB_BLAST_PERCENTAGE are numerical variable associated with disease burden.
      DFS_MONTHS,	DFS_STATUS,	OS_MONTHS and	OS_STATUS are time-dependent variables related to Disease-free survival and Overall-survival (with both time and status data).
      "),
    DT::DTOutput(ns("table_brut"))
  )
}

#' Data Server Function
#'
#' @noRd
mod_Data_server <- function(input, output, session, r) {
  ns <- session$ns
  ## Preview data

  output$table_brut <- DT::renderDT(
    dplyr::select(r$test$data, -"Whole_cohort"), # data
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
}

## To be copied in the UI
# mod_Data_ui("Data_ui_1")

## To be copied in the server
# callModule(mod_Data_server, "Data_ui_1")
