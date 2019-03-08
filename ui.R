shinyUI(
  dashboardPage(title = "ShinyAB Design",
    dashboardHeader(title = logo_grey_light, titleWidth = 250),
    dashboardSidebar(
      collapsed = TRUE, # sidebar hide
      width = 150,
      sidebarMenu(
        menuItem("Proportion", icon=icon("info"), tabName = "menu1"),
        menuItem("Mean",  icon=icon("info"), tabName = "menu2")
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "grey_light"),
      tabItems(
        tabItem(
          tabName = "menu1",
            fluidRow(
              box(title = "Parameter", width = 6, solidHeader = T, status = "primary", 
                fluidRow(
                  column(2, 
                    p(HTML("<b>UU</b>"),span(shiny::icon("info-circle"), id = "info_uu"),numericInput('uu', NULL, 10000),
                      tippy::tippy_this(elementId = "info_uu",tooltip = "Number of Unique Users of your experiment",placement = "right")
                    )
                  ),
                  column(2, 
                    p(HTML("<b>Test Period</b>"),span(shiny::icon("info-circle"), id = "info_test_period"),numericInput('test_period', NULL, 14),
                      tippy::tippy_this(elementId = "info_test_period",tooltip = "Only used when you check 'Running Lift'",placement = "right")
                    )
                  ),
                  column(3, numericInput('number_of_samples', "Number of Samples", 2, min = 2, max = 100, step = 1)),
                  column(2, uiOutput("ui_max_comparison"))
                ),
              fluidRow(
                column(4, numericInput('current_value', "As Is - Ratio", 0.3, min = 0, max = 1.0, step = 0.01)),
                column(4, numericInput('expected_value', "To Be - Ratio", 0.4, min = 0, max = 1.0, step = 0.01)),
                column(4, uiOutput("ui_current_lift"))
              ),
              fluidRow(
                column(4, numericInput('alpha', "significance level = α", 0.05, min = 0.01, max = 0.10, step = 0.01)),
                column(4, numericInput('power', "power = 1 - β", 0.80, min = 0.80, max = 0.99, step = 0.01)),
                column(4, radioButtons('alternative', "Choose test", choices  = c("two.sided","one.sided"), selected = "two.sided", inline = F))
              ),
              fluidRow(
                column(3,
                       p(HTML("<b>(Optional) Case</b>"),span(shiny::icon("info-circle"), id = "info_optional_case"), textInput('optional_case', NULL, ""),
                         tippy::tippy_this(elementId = "info_optional_case", tooltip = "You can annotate any label per experimental plan.", placement = "right")
                       )
                ),
                column(9, 
                       p(HTML("<b>(Optional) Choose plot</b>"),span(shiny::icon("info-circle"), id = "info_optional_plot"), 
                         checkboxGroupInput("optional_plot", NULL, 
                                            choices = c("Sample Size × Lift" = "lift_plot", "Running Lift" = "running_lift_plot", "Reject region and Power" = "rrp", "Probability Mass Function" = "pmf"), 
                                            selected = c("lift_plot","running_lift_plot","rrp","pmf"), inline = T),
                         tippy::tippy_this(elementId = "info_optional_plot", tooltip = "You can choose multiple plots for understanding AB Test.", placement = "right")
                       )    
                )
              ),
              fluidRow(
                column(12, actionButton("btn_go", "Add Record"))
              )     
            ),
            box(title = "Updater", width = 3, solidHeader = T, status = "primary", 
              fluidRow(
                column(6, numericInput('lift', "Lift (%)", 5, min = 0.01, max = 999, step = 0.01)),
                column(6, actionButton("btn_cal", "Update 'To Be'"))
              ),
              column(12, hr()),
              fluidRow(
                column(6, numericInput('number_of_comparison', "Number of Comparison", 1, step = 1)),
                column(6, actionButton("btn_com", "Update 'α'"))
              )
            ),
            box(title = "Summary of TypeⅠ and TypeⅡ Errors", width = 3, solidHeader = T, status = "primary", 
                tableOutput("kable_error_matrix")
            ),
            box(title = "Table", width = 12, solidHeader = T, status = "success", 
              tableOutput("kable_proportion"),
              fluidRow(
                column(1, actionButton("btn_remove", "Remove Record")),
                column(11, uiOutput("ui_unvisible_columns"))
              )
            ),
            tabBox(
              title = "", width = 6,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1",
              tabPanel("Sample Size × Lift", plotlyOutput("simulation_plot")),
              tabPanel("Running Lift", plotlyOutput("running_lift"))
            ),
            tabBox(title = "", width = 6, 
              id = "tabset2",
              tabPanel("Reject region and Power", plotlyOutput("rrp_plot")),
              tabPanel("Probability Mass Function", plotlyOutput("pmf_plot"))
            )
          )
        ),
        tabItem(
          tabName = "menu2",
            h2("Content")
        )
      )
    )
  )
)