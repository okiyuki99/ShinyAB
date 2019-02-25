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
                  shinyDashboardThemes(
                    theme = "grey_light"
                  ),
                  tabItems(
                    tabItem(
                      tabName = "menu1",
                      fluidRow(
                        box(title = "Parameter", width = 6, solidHeader = T, status = "primary", 
                            fluidRow(
                              column(6, numericInput('uu', "UU", 10000)),
                              column(6, numericInput('number_of_samples', "Number of Samples", 2, min = 2, max = 100, step = 1))
                            ),
                            fluidRow(
                              column(6, numericInput('current_value', "Current Value", 0.3, min = 0, max = 1.0, step = 0.01)),
                              column(6, numericInput('expected_value', "Expected Value", 0.4, min = 0, max = 1.0, step = 0.01))
                            ),
                            fluidRow(
                              column(6, numericInput('alpha', "α", 0.05, min = 0.01, max = 0.10, step = 0.01)),
                              column(6, numericInput('power', "1 - β", 0.80, min = 0.80, max = 0.99, step = 0.01))
                            ),
                            fluidRow(
                              column(12, actionButton("btn_go", "Add Record"))
                            )
                            
                        ),
                        box(title = "Option", width = 6, solidHeader = T, status = "primary", 
                            fluidRow(
                              column(6, numericInput('lift', "Lift (%)", 5, min = 0.01, max = 999, step = 0.01)),
                              column(6, actionButton("btn_cal", "Go Expected Value"))
                            ),
                            column(12, hr()),
                            fluidRow(
                              column(6, textInput('optional_case', "(Optional) Case", "")),
                              column(6, checkboxGroupInput("optional_plot", "Plot", choices = c("Sample Size × Lift" = "lift_plot", "PMF" = "pmf"), selected = c("lift_plot","pmf")))
                            )
                        ),
                        box(title = "Table", width = 12, solidHeader = T, status = "primary", 
                            tableOutput("kable_proportion"),
                            fluidRow(
                              column(1, actionButton("btn_remove", "Remove Record"))  
                            )
                        ),
                        box(title = "Sample Size × Lift", width = 6, solidHeader = T, status = "primary", 
                            plotlyOutput("simulation_plot")
                        ),
                        box(title = "Probability Mass Function", width = 6, solidHeader = T, status = "primary", 
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