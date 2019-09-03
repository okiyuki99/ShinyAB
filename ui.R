shinyUI(
  dashboardPage(title = "ShinyAB",
    dashboardHeader(title = logo_grey_light, titleWidth = 200),
    dashboardSidebar(
      collapsed = T,
      width = 200,
      sidebarMenu(
        menuItem("AB Design", icon = icon("th"), tabName = "menu_top"),
        menuItem("Github", icon = icon("github"), href = "https://github.com/okiyuki99/ShinyAB"),
        menuItem("RStudio Cloud", icon = icon("cloud"), href = "https://rstudio.cloud/project/245977"),
        menuItem("shinyapps.io", icon = icon("external-link-square"), href = "https://okiyuki.shinyapps.io/ShinyAB"),
        menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about")
      )
    ),
    dashboardBody(
      theme_grey_light,
      tabItems(
        tabItem(tabName = "menu_top",
          fluidRow(
              box(title = "Parameter", width = 6, solidHeader = T, status = "primary", 
                  fluidRow(
                    column(3, 
                           p(HTML("<b>UU</b>"),span(shiny::icon("info-circle"), id = "info_uu"),numericInput('uu', NULL, 10000),
                             tippy::tippy_this(elementId = "info_uu",tooltip = "Number of Unique Users of your experiment",placement = "right")
                           )
                    ),
                    column(3, 
                           p(HTML("<b>Test Period</b>"),span(shiny::icon("info-circle"), id = "info_test_period"),numericInput('test_period', NULL, 14),
                             tippy::tippy_this(elementId = "info_test_period",tooltip = "Only used when you check 'Running Lift'",placement = "right")
                           )
                    ),
                    column(3, numericInput('number_of_samples', "Number of Samples", 2, min = 2, max = 100, step = 1)),
                    column(3, uiOutput("ui_max_comparison"))
                  ),
                  fluidRow(
                    column(3, radioButtons('test_method', "Choose test", choices  = c("prop.test"), selected = "prop.test", inline = T)),
                    column(3, numericInput('alpha', "significance level = α", 0.05, min = 0.01, max = 0.10, step = 0.01)),
                    column(3, numericInput('power', "power = 1 - β", 0.80, min = 0.80, max = 0.99, step = 0.01))
                  ),
                  uiOutput("ui_test_method_paramter"),
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
                column(1, uiOutput("ui_dlbtn")),
                column(10, uiOutput("ui_unvisible_columns"))
              )
            ),
            tabBox(
              title = "", width = 6,
              id = "tabset1",
              tabPanel("Sample Size × Lift", plotlyOutput("simulation_plot") %>% withSpinner(type = 5)),
              tabPanel("Running Lift", plotlyOutput("running_lift") %>% withSpinner(type = 5))
            ),
            tabBox(title = "", width = 6, 
              id = "tabset2",
              tabPanel("Reject region and Power", plotlyOutput("rrp_plot") %>% withSpinner(type = 5)),
              tabPanel("Probability Mass Function", plotlyOutput("pmf_plot") %>% withSpinner(type = 5))
            )
          )
        ),
        tabItem(tabName = "menu_about",
          includeMarkdown("docs/about.md")
        )
      )
    )
  )
)