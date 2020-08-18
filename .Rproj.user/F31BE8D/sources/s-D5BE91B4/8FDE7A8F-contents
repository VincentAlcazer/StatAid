#' Contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h1("About the author"),

    p("My name is ",a("Vincent Alcazer", href = "https://www.linkedin.com/in/vincent-alcazer-673834151/"), 
    "and I am a medical doctor resident in clinical Hematology in Lyon, FRANCE.
    I am currently doing a PhD in immunology and bioinformatic, developping a new approach to 
    identify and select relevant CD8+ T cells epitopes among original targets (human endogenous retroviruses)
    expressed in different kind of cancers."),
    
    h2("Development"),
    p("This software has been developed with the R software using the ",a("Shiny package",href = "http://shiny.rstudio.com"), 
      "under the", a("Rstudio",href = "https://rstudio.com/"), " environment. The R package has been created using", 
      a("Golem.",href = "https://thinkr-open.github.io/golem/")),
    
    p("Full package list:"),
    HTML("<ul><li> Ggplot2 </li> 
         <li> dplyr </li> 
          <li> tidyr </li> 
         <li> ggrepel </li> 
         <li> ggpubr </li> 
         <li> tibble </li> 
               <li> forcats </li> 
          <li> RColorBrewer </li> 
                <li> broom </li> 
          <li> survival </li> 
                <li> survminer </li> 
         </ul>"),

    a("Favicon by icon8", href="https://icons8.com/icon/67592/statistics"),

    
    h1("Contact"),
    p("To report a bug or request/suggest a new feature, please open a", 
    a("Github issue.", href="https://github.com/VincentAlcazer/StatAid/issues"),
    "You can also contact me by ",a("mail.", href = "mailto:vincent.alcazer@chu-lyon.fr"))
    
 
  )
}
    
#' Contact Server Function
#'
#' @noRd 
mod_Contact_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_Contact_ui("Contact_ui_1")
    
## To be copied in the server
# callModule(mod_Contact_server, "Contact_ui_1")
 
