shinyServer(function(input, output, session){
  print("Initialize Start")
  values <- reactiveValues()
  values$data <- data.frame(stringsAsFactors = F)
  values$work_data <- data.frame(stringsAsFactors = F)
  values$pmf_data <- data.frame(stringsAsFactors = F)
  values$reject_line <- 0
  values$selected_columns <- c()
  
  id_notification <- ""
  
  # --------------------------
  # Display Max number of pairs
  output$ui_max_comparison <- renderUI({
    HTML("<div style=\"font-style: italic;\">Max number of pairs :<strong>", base::choose(input$number_of_samples, 2), "</strong></div>")
  })
  
  # --------------------------
  # Dynamic UI by per method 
  output$ui_test_method_paramter <- renderUI({
    if(input$test_method == "prop.test"){
      tagList(
        fluidRow(
          column(4, numericInput('current_value', "As Is - Ratio", 0.3, min = 0, max = 1.0, step = 0.01)),
          column(4, numericInput('expected_value', "To Be - Ratio", 0.4, min = 0, max = 1.0, step = 0.01)),
          column(4, uiOutput("ui_lift"))
        ),
        fluidRow(
          column(3, radioButtons('alternative', "Choose two or one sided", choices  = c("two.sided","one.sided"), selected = "two.sided", inline = T)),
          column(9, 
                 p(
                   HTML("<b>(Optional) Choose plot</b>"),span(shiny::icon("info-circle"), id = "info_optional_plot"), 
                   checkboxGroupInput("optional_plot", NULL, 
                                      choices = c("Sample Size × Lift" = "lift_plot", "Running Lift" = "running_lift_plot", "Reject region and Power" = "rrp", "Probability Mass Function" = "pmf"), 
                                      selected = c("lift_plot","running_lift_plot","rrp","pmf"), inline = T),
                   tippy::tippy_this(elementId = "info_optional_plot", tooltip = "You can choose multiple plots for understanding AB Test.", placement = "right")
                 )    
          )
        )
      )
    }else if(input$test_method == "t.test"){
      tagList(
        fluidRow(
          column(4, numericInput('current_value', "As Is - Mean", 1)),
          column(4, numericInput('expected_value', "To Be - Mean", 1.5)),
          column(4, uiOutput("ui_lift"))
        ),
        fluidRow(
          column(4, numericInput('standard_deviation', "standard deviation", 1)),
          column(4, radioButtons('test_type', "Choose type", choices  = c("two.sample", "one.sample", "paired"), selected = "two.sample", inline = T)),
          column(4, radioButtons('alternative', "Choose two or one sided", choices  = c("two.sided","one.sided"), selected = "two.sided", inline = T))
        )
      )
    }
  })
  
  # --------------------------
  # Display Lift from as_is to to_be
  output$ui_lift <- renderUI({
    if(input$test_method == "prop.test"){
      HTML("<div style=\"font-style: italic;\">Lift from As-Is ratio to To-Be ratio :<strong>", paste0(round(100 * (input$expected_value / input$current_value - 1),2),"%"), "</strong></div>")
    }else if(input$test_method == "t.test"){
      HTML("<div style=\"font-style: italic;\">Lift from As-Is mean to To-Be mean :<strong>", paste0(round(100 * (input$expected_value / input$current_value - 1),2),"%"), "</strong></div>")
    }
  })
  
  output$ui_dlbtn <- renderUI({
    if(nrow(values$data) > 0){
      downloadButton("dl_data", "Download")
    }
  })
  
  # --------------------------
  # Insert unvisible columns
  output$ui_unvisible_columns <- renderUI({
    checkboxGroupInput(
      "unvisible_columns", NULL, 
      choices = c("UU" = "UU", "number_of_samples" = "number_of_samples", "alpha" = "alpha", "power" = "power","test_method" = "test_method", "as_is" = "as_is",
                  "to_be" = "to_be", "sample_size_per_group" = "sample_size_per_group",
                  "required_sample_size" = "required_sample_size", "sampling_rate" = "sampling_rate"), 
      selected = c("UU","number_of_samples","alpha","power","test_method","as_is","to_be","sample_size_per_group","required_sample_size","sampling_rate"),
      inline = T
    )
    #selectInput(
    #  "unvisible_columns", NULL, 
    #  choices = c("UU" = "UU", "number_of_samples" = "number_of_samples", "alpha" = "alpha", "power" = "power","test" = "test", "as_is" = "as_is",
    #              "to_be" = "to_be", "sample_size_per_group" = "sample_size_per_group",
    #              "required_sample_size" = "required_sample_size", "sampling_rate" = "sampling_rate"), 
    #  selected = c("UU","number_of_samples","alpha","power","test","as_is","to_be","sample_size_per_group","required_sample_size","sampling_rate"),
    #  multiple = T
    #)
  })
  
  # --------------------------
  # Create Data for table and plot
  observeEvent(input$btn_go, {
    # from input
    number_of_samples <- input$number_of_samples
    test_method <- input$test_method
    alpha <- input$alpha
    power <- input$power
    current_value <- input$current_value
    expected_value <- input$expected_value
    alternative <- input$alternative
    unvisible_columns <- input$unvisible_columns
    
    # calculate sample size
    if(test_method == "prop.test"){
      sample_size_per_group <- ceiling(power.prop.test(n = NULL, 
                                                       p1 = current_value, p2 = expected_value,
                                                       sig.level = alpha, power = power, alternative = alternative)$n)
    }else if(test_method == "t.test"){
      sample_size_per_group <- ceiling(power.t.test(n = NULL, 
                                                    delta = expected_value - current_value,
                                                    sd = input$standard_deviation,
                                                    sig.level = alpha, power = power, type = input$test_type, alternative = alternative)$n)
    }
    required_sample_size <- number_of_samples * sample_size_per_group
    sampling_rate <- required_sample_size / input$uu
    
    # save
    values$work_data <- data.frame(UU = input$uu, number_of_samples, alpha, power, test_method,
                                   as_is = current_value, 
                                   to_be = expected_value, 
                                   sample_size_per_group,
                                   required_sample_size, 
                                   sampling_rate,
                                   stringsAsFactors = F)
    
    # bind data
    values$data <- bind_rows(values$data, values$work_data) 
    
    # create data for pmf plot
    if("pmf" %in% input$optional_plot){
      pmf_as_is <- dbinom(0:sample_size_per_group, sample_size_per_group, current_value)
      pmf_to_be <- dbinom(0:sample_size_per_group, sample_size_per_group, expected_value)
      ind <- pmf_as_is >= 0.0001 | pmf_to_be >= 0.0001 
      values$pmf_data <- bind_rows(data.frame(n = c(0:sample_size_per_group)[ind], pmf = pmf_as_is[ind], group = "as_is", stringsAsFactors = F),
                                   data.frame(n = c(0:sample_size_per_group)[ind], pmf = pmf_to_be[ind], group = "to_be", stringsAsFactors = F))
    }
  })
  
  # --------------------------
  # Calculate expected probability from lift 
  # --------------------------
  observeEvent(input$btn_cal,{
    updateNumericInput(session, "expected_value", value = input$current_value * (1 + 0.01 * input$lift))
  })
  
  # --------------------------
  # Update alpha from number of comparison based on Bonferroni correction
  # --------------------------
  observeEvent(input$btn_com,{
    updateNumericInput(session, "alpha", value = input$alpha / input$number_of_comparison)
  })
  
  # --------------------------
  # Remove Row of Table
  # --------------------------
  observeEvent(input$btn_remove,{
    if(nrow(values$data) > 0){
      values$data <- dplyr::slice(values$data, 1:nrow(values$data)-1)
    }
  })
  
  # --------------------------
  # Download CSV Table
  # --------------------------
  output$dl_data <- downloadHandler(
    filename = function() { 
      paste0("data-", format(Sys.time(),"%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$data, file)
    }
  )

  # --------------------------
  # Output : summary table
  # --------------------------
  output$kable_proportion <- function() {
    shiny::req(values$data)
    
    if(nrow(values$data) == 0){
      return(NULL)
    }
    
    values$data %>%
      replace(is.na(.), "") %>%
      mutate(sampling_rate = paste0(round(sampling_rate * 100, 2),"%")) %>%
      mutate_if(is.numeric, function(x){formatC(x,format = "f", big.mark = ",", drop0trailing = T)}) %>%
      mutate(sample_size_per_group = cell_spec(sample_size_per_group, color = "blue")) %>%
      mutate(required_sample_size = cell_spec(required_sample_size, color = "blue")) %>%
      mutate(sampling_rate = cell_spec(sampling_rate, color = "blue")) %>%
      select(input$unvisible_columns) %>%
      save_to(num_cols, ncol) %>%
      knitr::kable(align = "r", escape = F) %>% 
      kable_styling(c("striped", "bordered"), full_width = T) %>%
      collapse_rows(columns = 1:num_cols, valign = "top")
  }

  # --------------------------
  # Output : Samplesize × Lift
  # --------------------------
  output$simulation_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>%
      config(displayModeBar = F)
    p$elementId <- NULL
    
    if(input$test_method == "t.test" | input$btn_go == 0 | !"lift_plot" %in% isolate(input$optional_plot)){
      return(p) 
    }
    
    if(nrow(values$work_data) > 0){
      if(values$work_data[["test_method"]] != "prop.test"){
        return(p)
      }
    }
    
    alpha <- isolate(input$alpha)
    power <- isolate(input$power)
    current_value <- isolate(input$current_value)
    alternative <- isolate(input$alternative)
    
    lift <- seq(0.01, 0.5, 0.01)
    expected_values <- current_value * (1 + lift)
    sample_size_per_groups <- sapply(expected_values, function(x){
      ceiling(power.prop.test(n = NULL, 
                              p1 = current_value, p2 = x,
                              sig.level = alpha, power = power, alternative = alternative)$n)})
    
    df <- data.frame(lift = lift * 100, sample_size_per_groups, stringsAsFactors = F)

    p <- plot_ly(df, x = ~lift, y = ~sample_size_per_groups, text = ~paste0('Lift(%) : ', lift, '\r\nsample size per group : ', format(sample_size_per_groups, big.mark = ',')), hoverinfo = "text") %>% 
      add_trace(showlegend = F, type = "scatter", mode = 'lines+markers', line = list(color = "#67BF5C"), marker = list(color = "#67BF5C")) %>%
      layout(title = paste("As Is:",current_value, "alpha:",alpha, "power:",power), 
             yaxis = list(title = "sample size per group", exponentformat = "none"), xaxis = list(title = "Lift (%)", dtick = 5)) %>%
      config(displayModeBar = F)
    p$elementId <- NULL
    p
  })
  
  # --------------------------
  # Output : Running Lift
  # --------------------------
  output$running_lift <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>%
      config(displayModeBar = F)
    p$elementId <- NULL
    
    if(input$test_method == "t.test" | input$btn_go == 0 | !"running_lift_plot" %in% isolate(input$optional_plot)){
      return(p) 
    }
    
    if(nrow(values$work_data) > 0){
      if(values$work_data[["test_method"]] != "prop.test"){
        return(p)
      }
    }
    
    uu <- isolate(input$uu)
    alpha <- isolate(input$alpha)
    power <- isolate(input$power)
    current_value <- isolate(input$current_value)
    test_period <- isolate(input$test_period)
    alternative <- isolate(input$alternative)
    
    uu_daily <- floor(uu / test_period)
    
    lifts <- sapply(1:test_period, function(x){
      p2 <- power.prop.test(n = x * uu_daily, p1 = current_value, p2 = NULL,
                            sig.level = alpha, power = power, alternative = alternative)$p2
      100 * (p2 / current_value - 1.0)
    })
    
    df <- data.frame(lifts, period = 1:test_period, uu = 1:test_period * uu_daily, stringsAsFactors = F)
    p <- plot_ly(df, x = ~period) %>% 
      add_trace(y = ~lifts, name = "Lift(%)", text = ~paste0('Running Day : ', period, '\r\nLift (%) : ', round(lifts,2)), hoverinfo = "text", showlegend = T, type = "scatter", mode = 'lines+markers', line = list(color = "#67BF5C"), marker = list(color = "#67BF5C")) %>%
      add_trace(y = ~uu, name = "Sample Size", text = ~paste0('Running Day : ', period, '\r\nSample Size : ', format(uu, big.mark = ",")), hoverinfo = "text", showlegend = T, type = "scatter", mode = 'lines+markers', yaxis = "y2", line = list(color = "#ED97CA"), marker = list(color = "#ED97CA")) %>%
      layout(title = paste("As Is:",current_value, "alpha:",alpha, "power:",power), 
             legend = list(orientation = 'h'),
             yaxis = list(title = "Lift(%)", exponentformat = "none"), 
             yaxis2 = list(title = "Sample Size",exponentformat = "none", overlaying = "y", side = "right", automargin = T),
             xaxis = list(title = "Running Day", dtick = 1)) %>%
      config(displayModeBar = F)
    p$elementId <- NULL
    p 
  })
  
  # --------------------------
  # Output : Reject region and Power Plot
  # --------------------------
  output$rrp_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>%
      config(displayModeBar = F)
    p$elementId <- NULL
    
    if(input$test_method == "t.test" | input$btn_go == 0 | !"rrp" %in% isolate(input$optional_plot)){
      return(p)
    }
    
    if(nrow(values$work_data) > 0){
      if(values$work_data[["test_method"]] != "prop.test"){
        return(p)
      }
    }
    
    alpha <- isolate(input$alpha)
    power <- isolate(input$power)
    
    df <- data_power_prop_test(values$work_data$sample_size_per_group, 
                               values$work_data$sample_size_per_group, 
                               values$work_data$as_is, 
                               values$work_data$to_be)
    
    g <- ggplot(df, aes(x = x) ) +
      geom_line(aes(y = y1, colour = 'H0 is True'), size = 0.5) +
      geom_line(aes(y = y2, colour = 'H1 is True'), size = 0.5) +
      geom_point(aes(y = y1, colour = 'H0 is True'), shape = 21, size = 1) +
      geom_point(aes(y = y2, colour = 'H1 is True'), shape = 21, size = 1) +
      geom_area(aes(y = y1, x = ifelse(x > qnorm(alpha / 2, lower.tail = F), x, NA)), fill = "#67BF5C", na.rm = T) +
      geom_area(aes(y = y2, x = ifelse(x > qnorm(alpha / 2, lower.tail = F), x, NA)), fill = '#67BF5C', alpha = 0.3, na.rm = T) +
      annotate("text", x = qnorm(alpha / 2, lower.tail = F) + 0.5, y = 0.2, label=paste0("Power\r\n",round(100 * power,1),"%")) +
      labs(x = '', y = '', 
           title = sprintf('As Is: %s To Be : %s Sample Size = %d', 
                           values$work_data$as_is, values$work_data$to_be, values$work_data$sample_size_per_group)) +
      scale_x_continuous(breaks = seq(-4,6,1)) +
      scale_colour_manual(breaks = c("H0 is True", "H1 is True"), values = c("#729ECE", "#FF9E4A")) +
      theme_bw()
    p <- ggplotly(g) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p$elementId <- NULL
    p
  })
  
  # --------------------------
  # Output : PMF Plot
  # --------------------------
  output$pmf_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
    
    if(input$test_method == "t.test" | input$btn_go == 0 | !"pmf" %in% isolate(input$optional_plot)){
      return(p) 
    }
    
    if(nrow(values$work_data) > 0){
      if(values$work_data[["test_method"]] != "prop.test"){
        return(p)
      }
    }
    
    g <- ggplot(values$pmf_data, aes(x = n, y = pmf, group = group, color = group)) + 
      geom_line(size = 0.5) +
      geom_point(shape = 21, size = 0.8) +
      scale_color_manual(values = c("#729ECE", "#FF9E4A")) +
      theme_bw() +
      theme(legend.position = "top") + 
      labs(y = "percent")
    p <- ggplotly(g) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p$elementId <- NULL
    p
  })
  
  # --------------------------
  # Output : Summary of Type 1 and 2 Errors
  # --------------------------
  output$kable_error_matrix <- function() {
    df.error <- tibble(
      ` ` = c("Decision","Decision","Decision","Decision"),
      `  ` = c("Do not reject H0","Do not reject H0","Reject H0","Reject H0"),
      `H0 is True` = c("Correct<br>True Negative<br>", paste0(100*(1 - input$alpha),"%"), "TypeⅠ Error<br>False Positive<br>", paste0(100*input$alpha,"%")),
      `H1 is True` = c("TypeⅡ Error<br>False Negative<br>", paste0(100*(1 - input$power),"%"), "Correct<br>True Positive<br>", paste0(100*input$power,"%"))
    )
    df.error[2, 3] <- cell_spec(df.error[2, 3], "html", color = "#3A5FCD")
    df.error[4, 3] <- cell_spec(df.error[4, 3], "html", color = "#CD3333")
    df.error[2, 4] <- cell_spec(df.error[2, 4], "html", color = "#CD3333")
    df.error[4, 4] <- cell_spec(df.error[4, 4], "html", color = "#3A5FCD")
    df.error %>%
      mutate(` ` = cell_spec(` `, "html", angle = -90)) %>%
      knitr::kable(align = "c", escape = F) %>% 
      kable_styling(c("striped", "bordered"), full_width = T) %>%
      column_spec(1, bold = T, width = "1cm") %>%
      column_spec(2, bold = T) %>%
      collapse_rows(columns = 1:2, valign = "middle") %>%
      add_header_above(c("　" = 2, "Real" = 2), bold = T)
  }
})