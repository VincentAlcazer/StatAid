#' Explo_custom_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Explo_custom_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(8,
      offset = 1,
      box(width = 11, plotOutput(ns("Graph_comp")))
    ),
    column(2,
      offset = 1,
      absolutePanel(
        width = 200, right = 20, draggable = T,
        style = "opacity: 1; z-index: 10 !important ;",
        wellPanel(
          strong("GRAPH TYPE"),
          selectInput(ns("Group"),
            label = ("Groups"),
            multiple = F, selected = NULL,
            ""
          ),
          checkboxInput(ns("Exclude_na"),
            label = "Exclude NA (groups)", value = T
          ),
          selectInput(ns("Group_level"),
            label = ("Group filter"),
            multiple = T, selected = NULL,
            ""
          ),
          selectInput(ns("Variable"),
            label = ("Variable"),
            multiple = F, selected = NULL,
            ""
          ),
          selectInput(ns("Graphtype"),
            label = ("Graph type"),
            multiple = F, selected = NULL,
            ""
          ),
          radioButtons(ns("Error"), "Error bars (Barchart)",
            choices = c(
              "Hide" = "hide",
              "Standard derivation" = "sd",
              "Standard error" = "se",
              "95% CI" = "IC95"
            ),
            selected = "IC95"
          ),

          radioButtons(ns("Stat"), "Statistics",
            choices = c(
              "Hide" = "no",
              "Parametrics" = "param",
              "Non parametrics" = "non_param"
            ),
            selected = "no"
          ),
          checkboxInput(ns("Show_points"),
            label = "Show data points (scatter)", value = T
          ),

          actionButton(ns("Run_analysis"), "Run")
        )
      ) # Absolutepanel
    ), # Column

    ##### Graph param

    column(
      10,
      box(
        title = "Graphical parameters", width = 11, collapsed = FALSE, collapsible = TRUE,
        column(
          4,
          wellPanel(
            strong("Global"),
            textInput(ns("Title"), "Title", "Main title"),
            sliderInput(ns("title_font_size"),
              label = "Title size", min = 1,
              max = 35, value = 18, step = 1
            ),
            selectInput(ns("Legend"),
              label = ("External legend"),
              choices = c(
                "None" = "none",
                "Top" = "top",
                "Right" = "right",
                "Left" = "left",
                "Bottom" = "bottom"
              )
            ),
            selectInput(ns("Colors"),
              label = ("Colors palette"),
              choices = c("Default",
                "Grey levels" = "grey",
                "Nature" = "npg",
                "NEJM" = "nejm",
                "JCO" = "jco",
                "Lancet" = "lancet",
                "JAMA" = "jama",
                "AAAS" = "aaas"
              )
            ),
            textInput(ns("legend_title"), "Legend", "Legend title"),
            sliderInput(ns("legend_font_size"),
              label = "Legend size (values - title)", min = 1,
              max = 35, value = c(12, 14), step = 1
            )
          )
        ),
        column(
          4,
          wellPanel(
            strong("X axis"),
            textInput(ns("x_lab"), "Title", ""),
            sliderInput(ns("x_font_size"),
              label = "Size (values - title)", min = 1,
              max = 35, value = c(12, 14), step = 1
            ),
            sliderInput(ns("x_angle"),
              label = "Rotation", min = 0,
              max = 90, value = 0, step = 1
            )
          ) # Wellpanel
        ), # Column
        column(
          4,
          wellPanel(
            strong("Y axis"),
            textInput(ns("y_lab"), "Title", ""),
            sliderInput(ns("y_font_size"),
              label = "Size (values - title)", min = 1,
              max = 35, value = c(12, 14), step = 1
            ),
            sliderInput(ns("y_limits"),
              label = "Y Limits", min = 0,
              max = 100, value = c(0, 50)
            ),
          ) # Wellpanel
        ) # Column
      )
    )
  )
}

#' Explo_custom_graph Server Function
#'
#' @noRd
mod_Explo_custom_graph_server <- function(input, output, session, r) {
  ns <- session$ns

  ########## ========== Graph comparatifs

  ## Parameters

  data <- reactive(r$test$data)

  observe({
    updateSelectInput(
      session,
      "Group",
      choices = c(names(data()[sapply(data(), class) %in% c("factor", "character")])[-1])
    )
  })

  observe({
    exclude <- which(names(data()) %in% c("Patient_id", input$Group, "Whole_cohort"))
    updateSelectInput(
      session,
      "Variable",
      choices = c(names(data())[-exclude])
    )
  })


  observe({
    var_id <- which(names(data()) == input$Variable)

    if (is.numeric(data()[, var_id]) == TRUE) {
      updateSelectInput(
        session,
        "Graphtype",
        choices = c("Boxplot",
          "Barchart (Mean)" = "Barchart_mean",
          "Histogram" = "Histogram"
        )
      )
    } else {
      updateSelectInput(
        session,
        "Graphtype",
        choices = c(
          "Barchart (count)" = "Barchart_count",
          "Histogram" = "Histogram"
        )
      )
    }
  })

  observe({
    req(input$Run_analysis >= 1)
    updateSelectInput(
      session,
      "Group_level",
      choices = c(NULL, (levels(data() %>% select(input$Group) %>% unlist() %>% as.factor())))
    )

    updateSliderInput(session,
                      "y_limits",
                      min = 0,
                      max =  as.numeric((res()[["lim"]]$ylim[[2]]))*3, 
                      value = c(as.numeric((res()[["lim"]]$ylim[[1]])), as.numeric((res()[["lim"]]$ylim[[2]])))
                      )
    
  })

 

  ##### Observe: legend parameters
  observe({
    updateTextInput(
      session, "y_lab",
      value = input$Variable
    )
    updateTextInput(
      session, "x_lab",
      value = input$Group
    )
  })





  ## DF

  data <- reactive(r$test$data)


  ## Reactives elements

  res <- reactive({
    input$Run_analysis
    req(input$Run_analysis >= 1)

    isolate({
      res <- autoplot(data(),
        group = input$Group,
        variable = input$Variable,
        group_filter_vector = input$Group_level,
        na_exclude_group = input$Exclude_na,
        plot_type = input$Graphtype,
        stat = input$Stat, add_points =
          input$Show_points,
        error_bar = input$Error
      )
    })
  })

  p <- reactive({
    res()[["graph"]]
  })


  ## Output

  output$Graph_comp <- renderPlot({
    p <- p() + theme_bw() +
      labs(title = input$Title, y = input$y_lab, x = input$x_lab, fill = input$legend_title) +
      ylim(input$y_limits[1],input$y_limits[2]) +
      theme(
        plot.title = element_text(size = input$title_font_size, face = "bold"),
        axis.text.x = element_text(size = input$x_font_size[1], angle = input$x_angle, vjust = 0.5, hjust = 0.5, color = "black"),
        axis.title.x = element_text(size = input$x_font_size[2], face = "bold"),
        axis.text.y = element_text(size = input$y_font_size[1], color = "black"),
        axis.title.y = element_text(size = input$y_font_size[2], face = "bold"),
        legend.title = element_text(size = input$legend_font_size[2], face = "bold"),
        legend.text = element_text(size = input$legend_font_size[1]),
        legend.position = input$Legend
      )


    if (input$Colors == "Default") {
      p
    } else if (input$Colors == "grey") {
      p + scale_fill_grey()
    } else {
      set_palette(p, input$Colors)
    }
  })
}

## To be copied in the UI
# mod_Explo_custom_graph_ui("Explo_custom_graph_ui_1")

## To be copied in the server
# callModule(mod_Explo_custom_graph_server, "Explo_custom_graph_ui_1")
