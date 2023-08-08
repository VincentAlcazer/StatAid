#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  shinyalert(
    title = "Welcome to StatAid v1.2.7!",
    text = "
    If you found StatAid useful please: <br><br>
    <b> <a href = https://joss.theoj.org/papers/10.21105/joss.02630> - Cite the original paper </a></b><br><br>
    <b> - Consider <a href = https://paypal.me/StatAid> making a donation to StatAid to maintain servers power / capacity </a></b><br><br>
    
    Additional servers are now available: <br>
    <a href = https://vincentalcazer.shinyapps.io/StatAid/> Mirror 1 </a></b><br>
    <a href = https://alcazerv.shinyapps.io/StatAid/> Mirror 2 </a></b><br>
    ",
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = T,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
  

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

  data <- reactive({
    if (is.null(input$df) == T) {
      dat <- data_aml %>%
        mutate("Whole_cohort" = as.factor("Whole cohort"))
      colnames(dat)[1] <- "Patient_id"
      dat$Patient_id <- as.factor(dat$Patient_id)
    } else {
      
      if(input$sep == "xl"){
        dat <- readxl::read_xlsx(input$df$datapath) %>%
          mutate("Whole_cohort" = as.factor("Whole cohort")) %>%
          as.data.frame()
        colnames(dat)[1] <- "Patient_id"
        dat$Patient_id <- as.factor(dat$Patient_id)
        
      } else {
        dat <- data.table::fread(input$df$datapath, sep = input$sep, dec = input$dec, na.strings = c("", "NA", "#N/A"), stringsAsFactors = T, data.table = F) %>%
          mutate("Whole_cohort" = as.factor("Whole cohort"))
        colnames(dat)[1] <- "Patient_id"
        dat$Patient_id <- as.factor(dat$Patient_id)
      }
      
     
    }
    return(dat)
  })



  observe({
    r$test$data <- data()
  })



  #################### ==================== App modules ====================  ####################

  callModule(mod_Intro_server, "Intro_ui_1")
  callModule(mod_Data_server, "Data_ui_1", r = r)
  callModule(mod_Explo_distri_server, "Explo_distri_ui_1", r = r)
  callModule(mod_Explo_cat_server, "Explo_cat_ui_1", r = r)
  callModule(mod_Explo_table_des_server, "Explo_table_des_ui_1", r = r)
  callModule(mod_Explo_paired_server, "Explo_paired_ui_1", r = r)
  callModule(mod_Explo_custom_graph_server, "Explo_custom_graph_ui_1", r = r)
  callModule(mod_Model_num_graph_coreg_server, "Model_num_graph_coreg_ui_1", r = r)
  callModule(mod_Model_num_server, "Model_num_ui_1", r = r)
  callModule(mod_Model_cat_server, "Model_cat_ui_1", r = r)
  callModule(mod_Model_surv_km_server, "Model_surv_km_ui_1", r = r)
  callModule(mod_Model_surv_server, "Model_surv_ui_1", r = r)
  callModule(mod_Model_num_multi_server, "Model_num_multi_ui_1", r = r)
  callModule(mod_Model_cat_multi_server, "Model_cat_multi_ui_1", r = r)
  callModule(mod_Model_surv_multi_server, "Model_surv_multi_ui_1", r = r)
  callModule(mod_ROC_server, "ROC_ui_1", r=r)
  mod_Data_merge_server("Data_merge_ui_1")
  callModule(mod_Signature_calc_server, "Signature_calc_ui_1")
}
