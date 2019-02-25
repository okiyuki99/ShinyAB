shinyServer(function(input, output, session){
  print("Initialize Start")
  values <- reactiveValues()
  values$data <- data.frame(stringsAsFactors = F)
  values$work_data <- data.frame(stringsAsFactors = F)
  values$pmf_data <- data.frame(stringsAsFactors = F)
  values$reject_line <- 0
  #values$power_estimate <- 0
  
  # --------------------------
  # Create Data for table and plot
  observeEvent(input$btn_go, {
    # from input
    alpha <- input$alpha
    power <- input$power
    current_value <- input$current_value
    expected_value <- input$expected_value
    
    # calculate sample size
    sample_size_per_group <- ceiling(power.prop.test(n = NULL, 
                                                     p1 = current_value, p2 = expected_value,
                                                     sig.level = alpha, power = power, alternative = "two.sided")$n)
    required_sample_size <- input$number_of_samples * sample_size_per_group
    sampling_rate <- required_sample_size / input$uu
    values$work_data <- data.frame(UU = input$uu, alpha, power, 
                                   current_value = current_value, 
                                   expected_value = expected_value, 
                                   sample_size_per_group,　required_sample_size, sampling_rate, 
                                   stringsAsFactors = F)
    
    # create case label
    optional_label <- isolate(input$optional_case)
    if(optional_label != ""){
      values$work_data[["case"]] <- optional_label
    }else if(optional_label == "" & "case" %in% names(values$data)){
      values$work_data[["case"]] <- ""
    }
    
    # bind data
    values$data <- bind_rows(values$data, values$work_data) 
    if("case" %in% names(values$data)){
      values$data <- select(values$data, case, everything())
    }
    
    # create data for pmf plot
    if("pmf" %in% input$optional_plot){
      pmf_as_is <- dbinom(0:sample_size_per_group, sample_size_per_group, current_value)
      pmf_to_be <- dbinom(0:sample_size_per_group, sample_size_per_group, expected_value)
      ind <- pmf_as_is >= 0.0001 | pmf_to_be >= 0.0001 
      values$pmf_data <- bind_rows(data.frame(n = c(0:sample_size_per_group)[ind], pmf = pmf_as_is[ind], group = "as_is", stringsAsFactors = F),
                                   data.frame(n = c(0:sample_size_per_group)[ind], pmf = pmf_to_be[ind], group = "to_be", stringsAsFactors = F))
      values$reject_line <- min(which(cumsum(pmf_as_is) >= (1.0 - alpha/2))) # 両側検定の右側の棄却域を求める
    }
    #values$power_estimate <- round(100 * (1.0 - cumsum(pmf_to_be)[values$reject_line]),2)
  })
  
  # --------------------------
  # Calculate expected probability from lift 
  # --------------------------
  observeEvent(input$btn_cal,{
    updateNumericInput(session, "expected_value", value = input$current_value * (1 + 0.01 * input$lift))
  })
  
  # --------------------------
  # Remove Row of Table
  # --------------------------
  observeEvent(input$btn_remove,{
    if(nrow(values$data) > 0){
      values$data <- slice(values$data, 1:nrow(values$data)-1)
      if("case" %in% names(values$data)){
        if(all(is.na(values$data["case"]))){
          values$data["case"] <- NULL
        }
      }
    }
  })
  
  # --------------------------
  # Output : summary table
  # --------------------------
  output$kable_proportion <- function() {
    if(nrow(values$data) > 0){
      values$data %>%
        replace(is.na(.), "") %>%
        mutate_if(is.numeric, function(x){formatC(x,format = "f", big.mark = ",", drop0trailing = T)}) %>%
        mutate(sample_size_per_group = cell_spec(sample_size_per_group, color = "blue")) %>%
        mutate(required_sample_size = cell_spec(required_sample_size, color = "blue")) %>%
        mutate(sampling_rate = cell_spec(sampling_rate, color = "blue")) %>%
        knitr::kable(align = "r", escape = F) %>% 
        kable_styling(c("striped", "bordered"), full_width = T)
    }  
  }
  
  # --------------------------
  # Output : Samplesize × Lift
  # --------------------------
  output$simulation_plot <- renderPlotly({
    if(input$btn_go == 0 | !"lift_plot" %in% isolate(input$optional_plot)){
      plot_ly(type="scatter", mode = "markers") %>% 
        layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
               yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F))
      
    }else{
      alpha <- isolate(input$alpha)
      power <- isolate(input$power)
      current_value <- isolate(input$current_value)
      
      lift <- seq(0.01, 0.5, 0.01)
      expected_values <- current_value * (1 + lift)
      sample_size_per_groups <- sapply(expected_values, function(x){
        ceiling(power.prop.test(n = NULL, 
                                p1 = current_value, p2 = x,
                                sig.level = alpha, power = power, alternative = "two.sided")$n)})
      
      df <- data.frame(lift = lift * 100, sample_size_per_groups, stringsAsFactors = F)
      p <- plot_ly(df, x = ~lift, y = ~sample_size_per_groups) %>% 
        add_trace(showlegend = F, type = "scatter", mode = 'lines+markers') %>%
        layout(title = paste("current value:",current_value, "alpha:",alpha, "power:",power), 
               yaxis = list(title = "sample size per group", exponentformat = "none"), xaxis = list(title = "Lift (%)", dtick = 5))
      p 
    }
  })
  
  # --------------------------
  # Output : PMF Plot
  # --------------------------
  output$pmf_plot <- renderPlot({
    input$btn_go
    
    if(nrow(values$pmf_data) > 0 & "pmf" %in% isolate(input$optional_plot)){
      ggplot(values$pmf_data, aes(x = n, y = pmf, group = group, color = group)) + 
        geom_line() +
        scale_color_manual(values = c("#00008B","#B22222")) +
        theme_bw() +
        theme(legend.position = "top") + 
        geom_vline(xintercept = values$reject_line, colour="#5d5d5d", linetype="longdash") +
        geom_text(aes(x = values$reject_line, label = paste0("Right rejected region for binom test: \r\n", values$reject_line), y = 0), alpha = 0.5, colour="#5d5d5d", angle = 0,  hjust = 0, vjust = 0) +
        labs(y = "percent")
    }
  })
})