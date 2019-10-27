#calling two different libraries
library(shiny)
library(plotly)
#sequence for different sample size sampling
h  <- seq(0.05, 0.95, 0.005)


#sample size for multiple margin for non-inferiority, beta = 1 - power 
multi.margin <- function(delta, varC, varT = NULL, muC,  alpha = 0.05, beta = 0.20){
  stopifnot(delta > 1, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1 / (delta)
    varT <- varC
  }
  else{
    k <- sqrt(varC) / (delta * sqrt(varT))
  }
  nT <- (qnorm(1 - alpha) + qnorm(1 - beta))^2 * (delta^2 * varT + varC / k) / (muC * (1 - delta))^2
  nC <- k * nT
  eff <- 2 * (varC + delta^2 * varT) / (sqrt(varC) + delta * sqrt(varT))^2
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

#sample size for additive margin in non-inferiority trial 
add.margin <- function(delta, varC, varT = NULL, alpha = 0.05, beta = 0.20){
  stopifnot(delta > 0, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1
    varT <- varC
  }
  else{
    k <- sqrt(varC) / sqrt(varT)
  }
  nT <- (qnorm(1 - alpha) + qnorm(1 - beta))^2 * (varT + varC / k) / (delta)^2
  nC <- k * nT
  if(k == 1){
    eff <- 1
  }
  else{
    eff <- 2 * (varC + varT) / (sqrt(varC) + sqrt(varT))^2
  }
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}


function(input, output){
  
  sliderValues <- reactive({
    # for normal data
    if(input$dist == "Normal"){
      # additive margin
      if(input$margin_n == "add"){
        val <- add.margin(delta = input$marginval_n1, varC = input$varC, varT = input$varT,
                          alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n))
        data.frame(Output = c("Sample Size in Control",
                              "Sample Size in Treatment",
                              "Randomization control to treatment (k:1))",
                              "Realtive Efficiency (compared to 1:1)"),
                   Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
      }
      # multiplicative margin
      else{
        # 
        if(input$null == "Greater"){
          val <- multi.margin(delta = input$marginval_n2, muC = input$muC1, varC = input$varC, varT = input$varT,
                              alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n))
          data.frame(Output = c("Sample Size in Control",
                                "Sample Size in Treatment",
                                "Randomization control to treatment (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
        else if(input$null == "Less"){
          val <- multi.margin(delta = input$marginval_n2, muC = input$muC2, varC = input$varC, varT = input$varT,
                              alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n))
          data.frame(Output = c("Sample Size in Treatment",
                                "Sample Size in Control",
                                "Randomization treatment to control (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
      }
    }
    else if(input$dist == "Binomial"){
      if(input$margin_b == "add"){
        if(input$null == "Greater"){
          pT <- input$pC1 - input$marginval_b1
          val <- add.margin(delta = input$marginval_b1, varC = input$pC1 * (1 - input$pC1), varT = pT * (1 - pT),
                            alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
          data.frame(Output = c("Sample Size in Control",
                                "Sample Size in Treatment",
                                "Randomization control to treatment (k:1))",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
        else if(input$null == "Less"){
          pT <- input$pC2 - input$marginval_b1
          val <- add.margin(delta = input$marginval_b1, varC = input$pC2 * (1 - input$pC2), varT = pT * (1 - pT),
                            alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
          data.frame(Output = c("Sample Size in Treatment",
                                "Sample Size in Control",
                                "Randomization treatment to control (k:1))",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
          
        }
      }
      else{
        if(input$null == "Greater"){
          pT <- input$pC1 / input$marginval_b2
          val <- multi.margin(delta = input$marginval_b2, muC = input$pC1, varC = input$pC1 * (1 - input$pC1), varT = pT * (1 - pT),
                              alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
          data.frame(Output = c("Sample Size in Control",
                                "Sample Size in Treatment",
                                "Randomization control to treatment (k:1))",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
        else if(input$null == "Less"){
          pT <- input$pC2 / input$marginval_b2
          val <- multi.margin(delta = input$marginval_b2, muC = input$pC2, varC = input$pC2 * (1 - input$pC2), varT = pT * (1 - pT),
                              alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
          data.frame(Output = c("Sample Size in Treatment",
                                "Sample Size in Control",
                                "Randomization treatment to control (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
          
        }
      }
    }
    
    else if(input$dist == "Poisson"){
      if(input$margin_p == "add"){
        if(input$null == "Greater"){
          lambdaT <- input$lambdaC1 - input$marginval_p1
          val <- add.margin(delta = input$marginval_p1, varC = input$lambdaC1, varT = lambdaT,
                            alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
          data.frame(Output = c("Sample Size in Control",
                                "Sample Size in Treatment",
                                "Randomization control to treatment (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
        else if(input$null == "Less"){
          lambdaT <- input$lambdaC2 - input$marginval_p1
          val <- add.margin(delta = input$marginval_p1, varC = input$lambdaC2, varT = lambdaT,
                            alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
          data.frame(Output = c("Sample Size in Treatment",
                                "Sample Size in Control",
                                "Randomization treatment to control (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
      }
      else{
        if(input$null == "Greater"){
          lambdaT <- input$lambdaC1 / input$marginval_p2
          val <- multi.margin(delta = input$marginval_p2, muC = input$lambdaC1, varC = input$lambdaC1 , varT = lambdaT,
                              alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
          data.frame(Output = c("Sample Size in Control",
                                "Sample Size in Treatment",
                                "Randomization control to treatment (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
        }
        else if(input$null == "Greater"){
          lambdaT <- input$lambdaC2 / input$marginval_p2
          val <- multi.margin(delta = input$marginval_p2, muC = input$lambdaC2, varC = input$lambdaC2 , varT = lambdaT,
                              alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
          data.frame(Output = c("Sample Size in Treatment",
                                "Sample Size in Control",
                                "Randomization treatment to control (k:1)",
                                "Realtive Efficiency (compared to 1:1)"),
                     Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
          
        }
      }
    }
  })
  
  
  
  eff <- reactive({
    if(input$dist == "Normal"){      
      if(input$margin_n == "add"){
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_n1, varC = input$varC, varT = input$varT,
                                                                   alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n)))[1:2])),
                   eff1 = (input$varC / h + input$varT/ (1 - h)) / (sqrt(input$varC) + sqrt(input$varT))^2)
      }
      else{
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_n2, muC = input$muC, varC = input$varC, varT = input$varT,
                                                                     alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n)))[1:2])),
                   eff1 = (input$varC / h + input$marginval_n2^2 * input$varT/ (1 - h)) / (sqrt(input$varC) + input$marginval_n2 * sqrt(input$varT))^2)
      }
    }
    
    else if(input$dist == "Binomial"){      
      if(input$margin_b == "add"){
        pT <- input$pC - input$marginval_b1
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_b1, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                                                                   alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b)))[1:2])),
                   eff1 = (input$pC *(1 - input$pC)  / (h) + pT *(1 - pT) / (1 - h)) / (sqrt(input$pC *(1 - input$pC)) + sqrt(pT *(1 - pT)))^2)
      }
      else{
        pT <- input$pC / input$marginval_b2
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_b2, muC = input$pC, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                                                                     alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b)))[1:2])),
                   eff1 = (input$pC *(1 - input$pC)  / h + input$marginval_b2^2 * pT *(1 - pT) / (1 - h)) / (sqrt(input$pC *(1 - input$pC)) + input$marginval_b2 * sqrt(pT *(1 - pT)))^2)
      }
    }
    
    
    else if(input$dist == "Poisson"){      
      if(input$margin_p == "add"){
        lambdaT  <- input$lambdaC - input$marginval_p1
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_p1, varC = input$lambdaC, varT = lambdaT,
                                                                   alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p)))[1:2])),
                   eff1 = (input$lambdaC / h + lambdaT/ (1 - h)) / (sqrt(input$lambdaC) + sqrt(lambdaT))^2)
      }
      else{
        lambdaT  <- input$lambdaC / input$marginval_p2
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_p2, muC = input$lambdaC, varC = input$lambdaC, varT = lambdaT,
                                                                     alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p)))[1:2])),
                   eff1 = (input$lambdaC / h + input$marginval_p2^2 * lambdaT/ (1 - h)) / (sqrt(input$lambdaC) + input$marginval_p2 * sqrt(lambdaT))^2)
      }
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

