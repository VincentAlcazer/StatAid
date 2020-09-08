#' Model_num_graph_coreg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import mgcv
mod_Model_num_graph_coreg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      10,
      column(8, offset = 2, box(title = "Scatter plot", plotOutput(ns("graph")), width = 12)),
      column(
        12,
        box(title = "Residuals vs predicted values", plotOutput(ns("Residuals_vs_predicted")), width = 4, collapsible = T),
        box(title = "Residuals QQ plot", plotOutput(ns("QQplot")), width = 4, collapsible = T),
        box(title = "Actual vs predicted values ", plotOutput(ns("Actual_vs_predicted")), width = 4, collapsible = T)
      )
    ),
    column(
      2,
      absolutePanel(
        width = 200, right = 20, draggable = T,
        style = "opacity: 0.85",
        wellPanel(
          selectInput(ns("y_var"),
            label = ("Y variable (outcome/to predict) - Numeric only"),
            multiple = F, selected = NULL,
            ""
          ),
          selectInput(ns("x_var"),
            label = ("X Variable (explanatory/predictive) - Numeric only"),
            multiple = F, selected = NULL,
            choices = c("")
          ),
          radioButtons(ns("model"),
            label = ("Model"),
            choices = list("Linear" = "lm", "GAM" = "gam", "LOESS" = "loess"),
            selected = "lm"
          ),
          radioButtons(ns("cor_type"),
            label = ("Correlation"),
            choices = list("Pearson" = "pearson", "Spearman" = "spearman"),
            selected = "pearson"
          ),

          actionButton(ns("Run_analysis"), "Run analysis")
        )
      ) # Absolutepanel
    )
  )
}

#' Model_num_graph_coreg Server Function
#'
#' @noRd
mod_Model_num_graph_coreg_server <- function(input, output, session, r) {
  ns <- session$ns

  ## DF

  data <- reactive(r$test$data)

  data_graph <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)

    isolate({
      temp_df <- data()
      df <- data.frame(
        Patient_id = temp_df$Patient_id,
        Variable_x = temp_df[, input$x_var],
        Variable_y = temp_df[, input$y_var]
      )
      return(df)
    })
  })

  ## Parameters update

  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c(names(data()[sapply(data(), class) %in% c("numeric", "integer")]))
    )
  })

  observe({
    updateSelectInput(
      session,
      "x_var",
      choices = c(setdiff(names(data()[sapply(data(), class) %in% c("numeric", "integer")]), input$y_var))
    )
  })


  ## Regression df

  regression_df <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)

    isolate({
      regression_dataframes(
        y_var = input$y_var, x_var = input$x_var, data = data(), model = input$model,
        cor_type = input$cor_type
      )
    })
  })

  smooth <- reactive({
    if (input$model == "gam") {
      geom_smooth(
        method = "gam", formula = y ~ s(x), se = T, color = "navyblue",
        fill = "lightblue", alpha = 0.5
      )
    } else if (input$model == "lm") {
      geom_smooth(
        method = "lm", se = T, color = "navyblue",
        fill = "lightblue", alpha = 0.5
      )
    } else if (input$model == "loess") {
      geom_smooth(
        method = "loess", se = T, color = "navyblue",
        fill = "lightblue", alpha = 0.5
      )
    }
  })


  ##### ===== OUTPUT

  output$graph <- renderPlot({
    input$Run_analysis
    req(input$Run_analysis >= 1)
    isolate({
      reg_param <- regression_df()[["tidy_df"]] %>% filter(grepl(input$x_var, term))


      if (input$model == "lm") {
        title <- paste0(
          "Linear model (Beta Coeff:", round(reg_param$estimate, 3),
          "; P-value: ", format.pval(reg_param$p.value, 3, eps = 0.0001), ")"
        )
      } else if (input$model == "gam") {
        title <- paste0(
          "Generalized additive model (Edf:", round(reg_param$edf, 3),
          "; P-value: ", format.pval(reg_param$p.value, 3, eps = 0.0001), ")"
        )
      } else if (input$model == "loess") {
        title <- "Locally Weighted Scatterplot Smoother"
      }

      data() %>%
        ggplot(aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        labs(title = paste0(
          title,
          "\n (", input$cor_type, "'s R:", round(regression_df()[["cor_df"]]$estimate, 2),
          "; P-value: ", format.pval(regression_df()[["cor_df"]]$p.value, 3, eps = 0.0001), ")"
        )) +
        smooth() +
        theme_bw() +
        default_theme +
        theme(title = element_text(size = 14))
    })
  })

  output$Residuals_vs_predicted <- renderPlot({
    regression_df()[["augment_df"]] %>%
      ggplot(aes(x = .fitted, y = Std_residuals)) +
      geom_point() +
      labs(title = "Standardized residuals vs predicted values", y = "Standardized residuals", x = "Predicted Y values") +
      geom_smooth(
        method = "gam", formula = y ~ s(x), se = T, color = "navyblue",
        fill = "lightblue", alpha = 0.5
      ) +
      geom_hline(yintercept = 2, size = 1, linetype = 2) +
      geom_hline(yintercept = -2, size = 1, linetype = 2) +
      geom_hline(yintercept = 0, size = 0.5, linetype = 1) +
      theme_bw() +
      default_theme
  })

  output$QQplot <- renderPlot({
    shapiro <- format.pval(shapiro.test(regression_df()[["augment_df"]]$Std_residuals)$p.value, 4, eps = 0.0001)
    y_pos <- max(regression_df()[["augment_df"]]$Std_residuals) * 0.9

    qplot(sample = Std_residuals, data = regression_df()[["augment_df"]]) +
      theme_bw() +
      labs(title = "Residuals Quantile-Quantile plot", y = "Standardized residuals", x = "Theorical values") +
      geom_abline(intercept = 0, slope = 1, size = 1, linetype = 2) +
      annotate(
        geom = "text", x = -1.75, y = y_pos, label = paste0("Shapiro p: ", shapiro),
        size = 5
      ) +
      default_theme
  })


  output$Actual_vs_predicted <- renderPlot({
    regression_df()[["augment_df"]] %>%
      ggplot(aes_string(x = ".fitted", y = input$y_var)) +
      geom_point() +
      labs(title = "Actual vs predicted values", y = "Actual Y values", x = "Predicted Y values") +
      geom_smooth(
        method = "gam", formula = y ~ s(x), se = T, color = "navyblue",
        fill = "lightblue", alpha = 0.5
      ) +
      theme_bw() +
      default_theme
  })
}

## To be copied in the UI
# mod_Model_num_graph_coreg_ui("Model_num_graph_coreg_ui_1")

## To be copied in the server
# callModule(mod_Model_num_graph_coreg_server, "Model_num_graph_coreg_ui_1")
