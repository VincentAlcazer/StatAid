#' Contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Contact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("About the author"),

    p(
      "My name is ", a("Vincent Alcazer", href = "https://www.linkedin.com/in/vincent-alcazer-673834151/"),
      "and I am a medical doctor resident in clinical Hematology in Lyon, FRANCE.
    I am currently doing a PhD in immunology and bioinformatics, developping a new approach to 
    identify and select relevant CD8+ T cells epitopes among original targets (human endogenous retroviruses)
    expressed in different types of cancers."
    ),

    h1("Development"),
    p(
      "This software has been developed with the R software using the ", a("Shiny package", href = "http://shiny.rstudio.com"),
      "under the", a("Rstudio", href = "https://rstudio.com/"), " environment. The R package has been created using",
      a("Golem.", href = "https://thinkr-open.github.io/golem/")
    ),
    
    h3("Packages and function used"),
    p("StatAid is using multiple packages and function to provide you an easy-to-use interface for statistical analysis. 
      All graphs are plotted with ",strong("ggplot2"), "combined with the ",strong("ggrepel"), " and ",strong("ggpubr"), " packages, unless stated otherwise. Colors are modified using the 
      ",strong("Rcolorbrewer's"), " package palettes.
      The ",strong("tidyr"), ", ",strong("dplyr"), ", ",strong("tibble"), " and ",strong("broom"), " packages are used for data tidying and formatting."
    ),
    

    HTML("<ul><li> <b>Data Exploration : </b> 
    Statistics in the descriptive tables are calculated with the base R functions (mean(), median(), sd() and IQR()). 
    Statistical tests are also performed with base R functions (t.test(), wilcox.test(), anova() with lm(), kruskal.test(), chisq.test(), fisher.test()).
    P-values are corrected with the base p.adjust() function. </li> 
    
    <li> <b>Uni and multivariate analysis : </b> Correlations are calculated with the base cor.test() function.
    Linear and logistic model parameters are calculated with the base glm() function, with family = gaussian or binomial/logit, respectively.
    Generalized additive model parameters are calculated with the gam() function from the <b>mgcv</b> package, with the REML method.
    Linear, GAM and LOESS models are plotted using ggplot2's geom_smooth() function.  </li> 
    
    <li> <b>Time-dependant outcome : </b> Survival curves (Kaplan-Meier estimates) are plotted with the <b>survival</b>  and <b>survminer</b>  packages. 
    Cox model parameters are calculated with the coxph() function. </li>  
    
      <li> <b>ROC curves : </b> ROC curves are plotted using the <b>plotROC</b> package. 
      Other parameters (sensitivity, specificity...) are computed using the <b>ROCit</b> package. </li>  
         

         </ul>"),

    a("Favicon by icon8", href = "https://icons8.com/icon/67592/statistics"),


    h1("Contact"),
    p(
      "To report a bug or request/suggest a new feature, please open a",
      a("Github issue.", href = "https://github.com/VincentAlcazer/StatAid/issues"),
      "You can also contact me by ", a("mail.", href = "mailto:vincent.alcazer@chu-lyon.fr")
    )
    

  )
}

#' Contact Server Function
#'
#' @noRd
mod_Contact_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_Contact_ui("Contact_ui_1")

## To be copied in the server
# callModule(mod_Contact_server, "Contact_ui_1")
