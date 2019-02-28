shinyUI(
  dashboardPage(title = "ShinyAB",
    dashboardHeader(title = logo_grey_light),
    dashboardSidebar(
      collapsed = TRUE, # sidebar hide
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
                  column(3, numericInput('uu', "UU", 10000)),
                  column(3, numericInput('number_of_samples', "Number of Samples", 2, min = 2, max = 100, step = 1)),
                  column(3, numericInput('number_of_comparison', "Number of Comparison", 1)),
                  column(3, uiOutput("max_comparison"))
                ),
              fluidRow(
                column(4, numericInput('current_value', "As Is - Ratio", 0.3, min = 0, max = 1.0, step = 0.01)),
                column(4, numericInput('expected_value', "To Be - Ratio", 0.4, min = 0, max = 1.0, step = 0.01)),
                column(4, uiOutput("current_lift"))
              ),
              fluidRow(
                column(4, numericInput('alpha', "α", 0.05, min = 0.01, max = 0.10, step = 0.01)),
                column(4, numericInput('power', "1 - β", 0.80, min = 0.80, max = 0.99, step = 0.01)),
                column(4, radioButtons('alternative', "Choose test", choices  = c("two.sided","one.sided"), selected = "two.sided", inline = F))
              ),
              fluidRow(
                column(12, actionButton("btn_go", "Add Record"))
              )     
            ),
            box(title = "Option", width = 3, solidHeader = T, status = "primary", 
              fluidRow(
                column(6, numericInput('lift', "Lift (%)", 5, min = 0.01, max = 999, step = 0.01)),
                column(6, actionButton("btn_cal", "Update 'To Be'"))
              ),
              column(12, hr()),
              fluidRow(
                column(6, textInput('optional_case', "(Optional) Case", "")),
                column(6, checkboxGroupInput("optional_plot", "Plot", choices = c("Sample Size × Lift" = "lift_plot", "PMF" = "pmf"), selected = c("lift_plot","pmf")))
              )
            ),
            box(title = "Summary of TypeⅠ and TypeⅡ Errors", width = 3, solidHeader = T, status = "primary", 
                tableOutput("kable_error_matrix")
            ),
            box(title = "Table", width = 12, solidHeader = T, status = "success", 
              tableOutput("kable_proportion"),
              fluidRow(
                column(1, actionButton("btn_remove", "Remove Record"))  
              )
            ),
            box(title = "Sample Size × Lift", width = 6, solidHeader = T, status = "success", 
              plotlyOutput("simulation_plot")
            ),
            box(title = "Probability Mass Function", width = 6, solidHeader = T, status = "success", 
              plotOutput("pmf_plot")
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


