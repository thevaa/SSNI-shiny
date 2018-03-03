library(SSNI)
library(plotly)
h  <- seq(0.05, 0.95, 0.01)

function(input, output) {
  sliderValues <- reactive({
    if(input$margin == "add"){
      val <- add.margin(delta = input$marginval, varC = input$varC, varT = input$varT,
                 alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization treatment to control (k:1)",
                            "Realtive Efficiency (compared to 1:1)"),
      Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
    }
    else{
      val <- multi.margin(delta = input$marginval, muC = input$muC, varC = input$varC, varT = input$varT,
                          alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization treatment to control (k:1)",
                            "Realtive Efficiency (compared to 1:1)"),
                 Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
    }
  })
  eff <- reactive({
      if(input$margin == "add"){
          data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval, varC = input$varC, varT = input$varT,
                                                             alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power)))[1:2])), 
                     eff1 = (input$varC^2 / (h) + input$varT^2/ (1 - h)) / (input$varC + input$varT)^2)
      }
      else{
          data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval, muC = input$muC, varC = input$varC, varT = input$varT,
                                                               alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power)))[1:2])),
                     eff1 = (input$varC^2 / h + input$marginval^2 * input$varT^2/ (1 - h)) / (input$varC + input$marginval * input$varT)^2)
      }
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  output$plot <- renderPlotly({
    plot_ly(eff(), x = ~control, y = ~eff1, type = 'scatter', mode = 'lines')
  })
  
}
