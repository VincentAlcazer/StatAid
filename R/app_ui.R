#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd

options(shiny.maxRequestSize = 150*1024^2)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      dashboardHeader(title = "StatAid v1.2.2"),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Introduction", tabName = "Intro_start", icon = icon("play-circle")),
          menuItem("Data loading", tabName = "Data", icon = icon("spinner")),
          menuItem("Data exploration", tabName = "Explo", icon = icon("poll")),
          menuItem("Paired-data analysis", tabName = "Paired", icon = icon("link")),
          menuItem("Univariate analysis & models",
            tabName = "Model", icon = icon("long-arrow-alt-right"),
            "Outcome (Y variable):",
            menuSubItem("Continuous", tabName = "Model_num", icon = icon("sort-numeric-down")),
            menuSubItem("Categorical", tabName = "Model_cat", icon = icon("table")),
            menuSubItem("Time-dependant", tabName = "Model_surv", icon = icon("clock"))
          ),
          menuItem("Multivariate analysis & models",
            tabName = "Multiv", icon = icon("arrows-alt-h"),
            "Outcome (Y variable):",
            menuSubItem("Continuous", tabName = "Model_num_multi", icon = icon("sort-numeric-down")),
            menuSubItem("Categorical", tabName = "Model_cat_multi", icon = icon("table")),
            menuSubItem("Time-dependant", tabName = "Model_surv_multi", icon = icon("clock"))
          ),
          # menuItem("Bioinformatic tools", tabName="Bioinfo", icon = icon("desktop"),
          #          menuSubItem("Heatmaps", tabName = "Heat")),
          menuItem("ROC Curves", tabName = "ROC", icon = icon("firstdraft")),
          menuItem("Bioinformatics tools", tabName = "Bioinfo", icon = icon("desktop"),
                   menuSubItem("Data merge", tabName = "Data_merge", icon = icon("code-merge")),
                   menuSubItem("Signature calculator", tabName = "Signature_calc", icon = icon("calculator")),
                   menuSubItem("Panel Informativity Optimizer", tabName = "PIO", icon = icon("percentage"))
                    ),
          menuItem("Contact/About", tabName = "Contact", icon = icon("info")),
          menuItem("Support StatAid", tabName = "Support", icon = icon("life-ring"))
        )
      ), #dashboard sidebar
      dashboardBody(
        tabItems(
          #################### ==================== INTRO ====================  ####################
          tabItem(
            tabName = "Intro_start",
            mod_Intro_ui("Intro_ui_1")
          ),


          #################### ==================== GUIDE ====================  ####################
          # tabItem(tabName = "Intro_guide",
          #         mod_Guide_ui("Guide_ui_1")
          # ),
          # tabItem(tabName = "Intro_test_table",
          #         fluidRow(
          #
          #                  img(src='www/Tableau_choix_tests.PNG', align = "center",
          #                       width="90%")
          #
          #         )
          # ),

          #################### ==================== DATA ====================  ####################
          tabItem(
            tabName = "Data",
            fluidRow(
              column(8, mod_Data_ui("Data_ui_1")), # column
              column(
                2,
                absolutePanel(
                  width = 200, draggable = T,
                  style = "opacity: 0.85",
                  wellPanel(
                    fileInput("df",
                      label = "File input",
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
                    radioButtons("sep", "Separator",
                      choices = c(
                        "comma-delim (.csv1)" = ",",
                        "semi colon-delim (.csv2)" = ";",
                        "tab-delim (.tsv/.txt)" = "\t",
                        "Excel (.xls/.xlsx)" = "xl" 
                      ),
                      selected = "\t"
                    ),
                    radioButtons("dec", "Decimal",
                      choices = c(
                        "Comma (,)" = ",",
                        "Period (.)" = "."
                      ),
                      selected = ","
                    ),
                    radioButtons("na", "Missing values",
                      choices = c(
                        "NA" = "NA",
                        "Empty case" = ""
                      ),
                      selected = ""
                    ),
                    br()
                  ) # WellPanel
                )
              ) # Absolute panel
            ) # FluidRow
          ),

          #################### ==================== EXPLO ====================  ####################
          tabItem(
            tabName = "Explo",
            fluidRow(
              tabsetPanel(
                id = "Explo", type = "tabs",
                tabPanel("Numerical variables", mod_Explo_distri_ui("Explo_distri_ui_1")),
                tabPanel("Categorical variables", mod_Explo_cat_ui("Explo_cat_ui_1")),
                tabPanel("Descriptive table", mod_Explo_table_des_ui("Explo_table_des_ui_1")),
                tabPanel("Custom graph", mod_Explo_custom_graph_ui("Explo_custom_graph_ui_1"))
              ) # tabsetPanel
            ) # FluidRow
          ), # tabItem

          #################### ==================== Paired data ====================  ####################
          tabItem(
            tabName = "Paired",
            fluidRow(
              h2("Paired-data analysis"),
              column(12, mod_Explo_paired_ui("Explo_paired_ui_1"))
            ) # FluidRow
          ), # tabItem

          #################### ==================== Univariate Predictive models ====================  ####################

          ########## ========== Numeric

          tabItem(
            tabName = "Model_num",
            fluidRow(
              tabsetPanel(
                id = "Model", type = "tabs",
                tabPanel(
                  "Correlation & Regression graph",
                  mod_Model_num_graph_coreg_ui("Model_num_graph_coreg_ui_1")
                ),
                tabPanel("Univariate analysis", mod_Model_num_ui("Model_num_ui_1"))

                # tabPanel("Custom graph", "A venir")
              )
            )
          ), # tabItem

          ########## ========== Categorical

          tabItem(
            tabName = "Model_cat",
            fluidRow(column(12, mod_Model_cat_ui("Model_cat_ui_1")))
          ), # tabItem


          ########## ========== Surv

          tabItem(
            tabName = "Model_surv",
            fluidRow(
              tabsetPanel(
                id = "Model", type = "tabs",
                tabPanel(
                  "Kaplan Meier curves",
                  mod_Model_surv_km_ui("Model_surv_km_ui_1")
                ),
                tabPanel(
                  "Univariate Cox model",
                  mod_Model_surv_ui("Model_surv_ui_1")
                )
              )
            )
          ), # tabItem


          #################### ==================== Multivariate Predictive models ====================  ####################

          ########## ========== Numeric

          tabItem(
            tabName = "Model_num_multi",
            fluidRow(
              tabsetPanel(
                id = "Model", type = "tabs",
                tabPanel("Manual variables selection", mod_Model_num_multi_ui("Model_num_multi_ui_1"))
              )
            )
          ), # tabItem

          ########## ========== Categorical

          tabItem(
            tabName = "Model_cat_multi",
            fluidRow(
              tabsetPanel(
                id = "Model", type = "tabs",
                tabPanel(
                  "Manual variables selection",
                  mod_Model_cat_multi_ui("Model_cat_multi_ui_1")
                )
              )
            )
          ), # tabItem

          tabItem(
            tabName = "Model_surv_multi",
            fluidRow(
              tabsetPanel(
                id = "Model", type = "tabs",
                tabPanel(
                  "Manual variables selection",
                  mod_Model_surv_multi_ui("Model_surv_multi_ui_1")
                )
              )
            )
          ), # tabItem
          
          tabItem(
            tabName = "ROC",
            fluidRow(
              h2("ROC curves"),
              column(12,  mod_ROC_ui("ROC_ui_1"))
            ) # FluidRow
          ), # tabItem
        
          #################### ==================== BIOINFO ====================  ####################
          
          tabItem(
            tabName = "Data_merge",
            fluidRow(
              h2("Data merging"),
              column(12,mod_Data_merge_ui("Data_merge_ui_1"))
            )
          ), # tabItem
           
          tabItem( 
          tabName = "PIO",
          fluidRow(
            h2("Panel Informativity Optimizer")
          )
        ), # tabItem
        tabItem(
            tabName = "Signature_calc",
            fluidRow(
              h2("Signature calculator for NGS data"),
              p("NB: Large gene expression dataset may not load (too big for the server - disconnect).
              Prefer the local version of StatAid in that case (see StatAid github)"),
              column(12,  mod_Signature_calc_ui("Signature_calc_ui_1"))
              )
            ), # tabItem
          
          

          #################### ==================== CONTACT ====================  ####################
          tabItem(tabName = "Contact", mod_Contact_ui("Contact_ui_1")),
          
          tabItem(
            tabName = "Support",
            fluidPage(
              column(
                10,
      
                    h1("Support StatAid"),
                    
                    p("StatAid is a free open-source software and will remain free, without any contribution needed in return.
                    
                   If you used the software for your research project, please consider",
                      a("citing StatAid.",
                        href="https://joss.theoj.org/papers/10.21105/joss.02630" )),

                    
                    p("If you find the program useful and want to further support the project, you can also make a donation
                      on",a("the StatAid's paypal page.",href="https://paypal.me/StatAid")),
                    
                    h2("Why donating to StatAid?"),
                  
                    p("The online version of StatAid is currently hosted on the Shinyapps.io servers. In a first step, contributions
                    to StatAid will help to improve the website performances and allow more users to access it
                    by increasing the available active hours.
                    
                    In case of multiple contributions, a dedicated webserver will be deployed 
                    to ensure a stable and unlimited access to StatAid. 
                      
                      ")
                    
                    
                  )#column
            )#fluidrow   
          ) # tabItem
          
          
        ) # TabItems
        
        
      ) # Dashboard body
    ) # Dashboard page
  ) # taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "StatAid"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
