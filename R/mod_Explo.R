#' Explo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Explo_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Explo Server Function
#'
#' @noRd 
mod_Explo_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_Explo_ui("Explo_ui_1")
    
## To be copied in the server
# callModule(mod_Explo_server, "Explo_ui_1")
 
