#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  #################### ==================== Global links ====================  ####################   
  
  observeEvent(input$link_to_guide, {
    newvalue <- "Intro_guide"
    updateTabItems(session, "tabs", newvalue)
  })
  observeEvent(input$link_to_contact, {
    newvalue <- "Contact"
    updateTabItems(session, "tabs", newvalue)
  })
  
  
  
  ########## ========== DATAFRAME: Loading and constant modifications 

  
  r <- reactiveValues(
    test = reactiveValues()
  )
  
  data = reactive({
    if(is.null(input$df) == T){
      dat <- data_aml %>%
        mutate("Whole_cohort" = as.factor("Whole cohort"))
      colnames(dat)[1] <- "Patient_id"
      dat$Patient_id <- as.factor(dat$Patient_id)
    }else {
      dat <- data.table::fread(input$df$datapath, sep=input$sep, dec=input$dec, na.strings=c("","NA","#N/A"),stringsAsFactors=T, data.table = F ) %>%
        mutate("Whole_cohort" = as.factor("Whole cohort"))
      colnames(dat)[1] <- "Patient_id"
      dat$Patient_id <- as.factor(dat$Patient_id)}
    return(dat)
  })
  
  

   observe({
    r$test$data <- data()
      
    })

   

  #################### ==================== App modules ====================  #################### 
   
  callModule(mod_Intro_server, "Intro_ui_1")
  callModule(mod_Data_server, "Data_ui_1",  r=r)
  callModule(mod_Explo_distri_server, "Explo_distri_ui_1", r=r)
  callModule(mod_Explo_cat_server, "Explo_cat_ui_1", r=r)
  callModule(mod_Explo_table_des_server, "Explo_table_des_ui_1", r=r)
  callModule(mod_Explo_paired_server, "Explo_paired_ui_1",r=r)
  callModule(mod_Explo_custom_graph_server, "Explo_custom_graph_ui_1", r=r)
  callModule(mod_Model_num_graph_coreg_server, "Model_num_graph_coreg_ui_1", r=r)
  callModule(mod_Model_num_server, "Model_num_ui_1", r=r)
  callModule(mod_Model_cat_server, "Model_cat_ui_1", r=r)
  callModule(mod_Model_surv_km_server, "Model_surv_km_ui_1",r=r)
  callModule(mod_Model_surv_server, "Model_surv_ui_1", r=r)
  callModule(mod_Model_num_multi_server, "Model_num_multi_ui_1",r=r)
  callModule(mod_Model_cat_multi_server, "Model_cat_multi_ui_1", r=r)

}
