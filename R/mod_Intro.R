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
          
          
          strong("Link to ", a("StatAid's quick-start user guide", 
                                    href = "https://github.com/VincentAlcazer/StatAid/blob/master/STATAID_QUICK_START_USER_GUIDE.pdf")),
          br(),
          strong("Lien vers le ", a("guide d'utilisation en Francais.", 
                                    href = "https://github.com/VincentAlcazer/StatAid/blob/master/StatAid_guide_FR.pdf")),
          
          
          
          h3("An evolving software"),
          
          p(
            "The main goal of StatAid is to fit the needs for every-day statistical analysis in life science. 
                   If any feature you find useful / would like to use on StatAid is not available yet, you can ask for 
                   its implementation on the",
            a("StatAid Github Repository.", href = "https://github.com/VincentAlcazer/StatAid/issues")
          ),
          
          
          h3("Citing StatAid"),
          p("If you found StatAid useful and used it for your research, please cite the", 
            a("paper published in the Journal of Open Source Software.",href="https://joss.theoj.org/papers/10.21105/joss.02630" )),
          
          
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
          
          HTML("
            31/07/2023: v1.2.7 - Bug corrections for KM and Cox modules </br>
            12/07/2023: v1.2.6 - Minor bug corrections for KM curves </br>
            12/07/2022: v1.2.5 - Survival table (for KM estimate) </br>
            06/05/2022: v1.2.4 - Additional server released </br>
            16/03/2022: v1.2.3 - ROC module improvement <br/>
            02/03/2021: v1.2.2 - Excel file support and signature calculator module <br/>
            15/12/2020: v1.2.1 - ROC curves module <br/>
            29/10/2020: v1.2 - Official release for JOSS publication <br/>
            12/10/2020: v1.1 - Complete release for JOSS publication <br/>
            26/08/2020: v1.0 - Github Public Release for JOSS review <br/>
            24/08/2020: v0.8 - Univariate & multivariate models for time-dependant outcome <br/>
            20/08/2020: v0.7.1 - Minor tweaks and bug fixes <br/>
            18/08/2020: v0.7 - Paired-data exploration module <br/>
            29/07/2020: v0.6 - Multivariate analysis and models modules (numeric & categorical outcomes <br/>
            09/05/2020: v0.5 - Univariate analysis and models modules (time-dependant outcome) <br/>
            02/05/2020: v0.4 - Univariate analysis and models modules (categorical outcome) <br/>
            21/04/2020: v0.3 - Univariate analysis and models modules (numeric outcome) <br/>
            19/03/2020: v0.2 - Descriptive analysis module <br/>
            17/02/2020: v0.1.1 - Global stability enhancement <br/>
            11/01/2020: v0.1 - Data loading & Exploratory data modules <br/>
            ")

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
