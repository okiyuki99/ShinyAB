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
                  column(2, numericInput('test_period', "Test Period", 14)),
                  column(3, numericInput('number_of_samples', "Number of Samples", 2, min = 2, max = 100, step = 1)),
                  column(3, numericInput('number_of_comparison', "Number of Comparison", 1)),
                  column(2, uiOutput("ui_max_comparison"))
                ),
              fluidRow(
                column(4, numericInput('current_value', "As Is - Ratio", 0.3, min = 0, max = 1.0, step = 0.01)),
                column(4, numericInput('expected_value', "To Be - Ratio", 0.4, min = 0, max = 1.0, step = 0.01)),
                column(4, uiOutput("ui_current_lift"))
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
                column(6, checkboxGroupInput("optional_plot", "Plot", 
                                             choices = c("Sample Size × Lift" = "lift_plot", "PMF" = "pmf", "Running Lift" = "running_lift_plot"), 
                                             selected = c("lift_plot","pmf","running_lift_plot")))
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
            box(title = "Sample Size × Lift", width = 6, solidHeader = T, status = "success", 
              plotlyOutput("simulation_plot")
            ),
            box(title = "Probability Mass Function", width = 6, solidHeader = T, status = "success", 
              plotOutput("pmf_plot")
            ),
            box(title = "Running Lift", width = 6, solidHeader = T, status = "success", 
              plotlyOutput("running_lift")
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


c("lad.ad", "lad.adconfig", "lad.ad_event_lads", "lad.ad_filter", "lad.adaccount", "lad.adaccount_billing_link", "lad.adaccount_block_category", "lad.adgroup", "lad.app_conversion_ad", "lad.app_conversion_adgroup_breakdown", "lad.audience_group", "lad.billing", "lad.billing_owner", "lad.brid_to_idfa", "lad.campaign", "lad.creative", "lad.creative_asset", "lad.custom_conversion_ad", "lad.custom_conversion_adgroup_breakdown", "lad.group", "lad.group_adaccount_link", "lad.hike_adspot", "lad.hike_adspot_ctid", "lad.inventory", "lad.inventory_ad_config", "lad.inventory_ad_style", "lad.lads_event", "lad.lads_service_v1", "lad.lasm_hourly_report", "lad.lass_ad_response", "lad.lass_event", "lad.line_tag_user", "lad.media", "lad.mediation", "lad.mediation_rule", "lad.prefill_webtraffic_audience_group", "lad.prefill_webtraffic_url_condition", "lad.publisher", "lad.report_ad", "lad.report_adgroup_breakdown", "lad.review", "lad.service", "lad.service_group", "lad.targeting", "lad.targeting_audience", "lad.targeting_spec", "lad.user", "lad.user_adaccount_access", "lad.user_group_access", "lad.visual_format")

