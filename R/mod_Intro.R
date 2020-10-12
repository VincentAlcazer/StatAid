#' Intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Intro_ui <- function(id) {
  ns <- NS(id)
  tagList(

    #################### ==================== INTRO ====================  ####################
    tabItem(
      tabName = "Intro_start",
      fluidPage(
        column(
          10,
          h1("Introduction"),
          h2("Welcome to StatAid"),
          p("StatAid is a free open-source statistical software designed for life-science applied data analysis. It has been developed with the R software,
                     using the", a("Shiny", href = "http://shiny.rstudio.com"), "package."),
          
          strong("Lien vers le ", a("guide d'utilisation en Francais.", href = "https://vincentalcazer.shinyapps.io/StatAid_guide/")),
          
          h3("An evolving software"),
          
          p(
            "The main goal of StatAid is to fit the needs for every-day statistical analysis in life science. 
                   If any feature you find useful / would like to use on StatAid is not available yet, you can ask for 
                   its implementation on the",
            a("StatAid Github Repository.", href = "https://github.com/VincentAlcazer/StatAid/issues")
          ),
          
          
          h2("Current Features"),
          p("StatAid features will be regurlarly updated:"),
          HTML("<ul><li> Exploratory data analysis: distribution, count, missing-values and outliers check  </li>
                            <li> Descriptive analysis, simple comparative analysis and publication ready 'table 1' output </li>
                             <li> Publication-ready graph customization  </li>
                            <li> Paired data analysis (repeated measures,matched case-control studies) </li>
                            <li> Univariate analysis and models for continuous and categorical outcome: Correlation, linear and logistic regression  </li>
						              	<li> Univariate analysis and models for time-dependant outcome: Kaplan-Meier curves and cox regression </li>
                            <li> Multivariate analysis and models for continuous and categorical outcome </li>

                            </ul>"),
          
          h2("Change log"),
          
          p("12/10/2020: v1.1 - Complete release for JOSS publication"),
          p("26/08/2020: v1.0 - Github Public Release for JOSS review"),
          p("24/08/2020: v0.8 - Univariate & multivariate models for time-dependant outcome"),
          p("20/08/2020: v0.7.1 - Minor tweaks and bug fixes"),
          p("18/08/2020: v0.7 - Paired-data exploration module"),
          p("29/07/2020: v0.6 - Multivariate analysis and models modules (numeric & categorical outcomes)"),
          p("09/05/2020: v0.5 - Univariate analysis and models modules (time-dependant outcome)"),
          p("02/05/2020: v0.4 - Univariate analysis and models modules (categorical outcome)"),
          p("21/04/2020: v0.3 - Univariate analysis and models modules (numeric outcome)"),
          p("19/03/2020: v0.2 - Descriptive analysis module"),
          p("17/02/2020: v0.1.1 - Global stability enhancement"),
          p("11/01/2020: v0.1 - Data loading & Exploratory data modules")
        ) # Column
        
      ) #Fluidpage
      
    ), # tabitem
  )
}

#' Intro Server Function
#'
#' @noRd
mod_Intro_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_Intro_ui("Intro_ui_1")

## To be copied in the server
# callModule(mod_Intro_server, "Intro_ui_1")
