library(shiny)
library(plotly)

fluidPage(
  # App title 
  titlePanel("Sample Size in Non-Inferiority Trial"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
  # Sidebar layout with input and output definitions ----
  selectInput("dist", "Distribution of the data", c(Normal = "Normal", Binomial = "Binomial", Poisson = "Poisson")),
  
  
  #only show panel if the condition is met
  conditionalPanel(
    condition = "input.dist == 'Normal'",
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\mu_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\Delta\\mu_T\\text{,} \\quad \\Delta > 1$$")),
    radioButtons("margin", "Type of margin used:",
                 c("Additive" = "add", "Multiplicative" = "multi")),
    
    # Input: Simple integer interval ----
    numericInput("varC", withMathJax("Variance of Control: ,  $\\sigma_C^2:$"),
                 min = 0, max = 100,
                 value = 20, step = 0.01),
    
    # Input: Decimal interval with step value ----
    numericInput("varT", withMathJax("Variance of Treatment: ,  $\\sigma_T^2:$"),
                 min = 0, max = 100,
                 value = 20, step = 0.01),
    
    numericInput("muC", withMathJax("Mean of Control: ,  $\\mu_C:$"),
                 min = 0, max = 100,
                 value = 5, step = 0.01),
    
    sliderInput("marginval_n", "Margin:",
                min = 0, max = 5,
                value = 1.1, step = 0.01),
    
    #
    selectInput("type1_n", "Type 1:", c("0.01", "0.025", "0.05", "0.1")),
    # Input: Animation with custom interval (in ms) ----
    # to control speed, plus looping
    selectInput("power_n", "Power:", c("0.8", "0.9", "0.95"))
  ),
  
  
    # Only show this panel if Custom is selected
    conditionalPanel(
      condition = "input.dist == 'Binomial'",
      p("Additve:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } p_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
      p("Multiplicative:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } \\Delta p_T\\text{,} \\quad \\Delta > 1$$")),
      radioButtons("margin", "Type of margin used:",
                   c("Additive" = "add", "Multiplicative" = "multi")),
      numericInput("pC", withMathJax("Control Group Proportion,  $p_C:$"),
                   min = 0, max = 1,
                   value = 0.20, step = 0.001),
      numericInput("pT", withMathJax("Treatment Group Proportion,  $p_T:$"),
                   min = 0, max = 1,
                   value = 0.20, step = 0.001),
      sliderInput("marginval_b", "Margin:",
                  min = 0, max = 2,
                  value = 1.1, step = 0.001),
      selectInput("type1_b", "Type 1:", c("0.01", "0.025", "0.05", "0.1")),
      selectInput("power_b", "Power:", c("0.8", "0.9", "0.95"))
    ),
  
  
  conditionalPanel(
    condition = "input.dist == 'Poisson'",
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\lambda_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\Delta \\lambda_T\\text{,} \\quad \\Delta > 1$$")),
    radioButtons("margin", "Type of margin used:",
                 c("Additive" = "add", "Multiplicative" = "multi")),
    numericInput("lambdaC", withMathJax("Mean of Control,  $\\lambda_C:$"),
                 min = 0, max = 50,
                 value = 3, step = 0.05),
    numericInput("lambdaT", withMathJax("Mean of Treatment,  $\\lambda_T:$"),
                 min = 0, max = 50,
                 value = 3, step = 0.05),
    sliderInput("marginval_p", "Margin:",
                min = 0, max = 2,
                value = 1.2, step = 0.01),
    selectInput("type1_p", "Type 1:", c("0.01", "0.025", "0.05", "0.1")),
    selectInput("power_p", "Power:", c("0.8", "0.9", "0.95"))
    )),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      h3("Sample Size and Eficiency"),
      tableOutput("values"),

      h3("Plot of Efficiency"),
      plotlyOutput("plot")
    ))
  )
