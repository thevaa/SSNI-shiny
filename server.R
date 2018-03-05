library(plotly)
h  <- seq(0.05, 0.95, 0.005)

multi.margin <- function(delta, varC, varT = NULL, muC,  alpha = 0.05, beta = 0.20){
  stopifnot(delta > 1, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1 / (delta)
    varT <- varC
  }
  else{
    k <- sqrt(varC) / (delta * sqrt(varT))
  }
  nT <- (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * (delta^2 * varT + varC / k) / (muC * (1 - delta))^2
  nC <- k * nT
  eff <- 2 * (varC + delta^2 * varT) / (sqrt(varC) + delta * sqrt(varT))^2
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

add.margin <- function(delta, varC, varT = NULL, alpha = 0.05, beta = 0.20){
  stopifnot(delta > 0, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1
    varT <- varC
  }
  else{
    k <- sqrt(varC) / sqrt(varT)
  }
  nT <- (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * (varT + varC / k) / (delta)^2
  nC <- k * nT
  if(k == 1){
    eff <- 1
  }
  else{
    eff <- 2 * (varC + varT) / (sqrt(varC) + sqrt(varT))^2
  }
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}


function(input, output) {
  sliderValues <- reactive({
    if(input$margin == "add"){
      val <- add.margin(delta = input$marginval, varC = input$varC, varT = input$varT,
                 alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization control to treatment (k:1))",
                            "Realtive Efficiency (compared to 1:1)"),
      Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
    }
    else{
      val <- multi.margin(delta = input$marginval, muC = input$muC, varC = input$varC, varT = input$varT,
                          alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization control to treatment (k:1)",
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
  f <- list(family = "Courier New, monospace", size = 14, color = "#7f7f7f")
  x <- list(title = "Sample Size in Control", titlefont = f)
  y <- list(title = "Relative Efficiency", titlefont = f)
  output$plot <- renderPlotly({
    plot_ly(eff(), x = ~control, y = ~eff1, type = 'scatter', mode = 'lines') %>%
      layout(title = "Relative Efficiency of Sample size in Control vs Optimal Allocation", xaxis = x, yaxis = y)
  })
}
