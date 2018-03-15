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

    radioButtons("margin_n", "Type of margin used:",
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



    conditionalPanel(condition = "input.margin_n == 'add'",
                    sliderInput("marginval_n1", "Margin:", min = 0.01, max = 5, value = 0.4, step = 0.01)),

    conditionalPanel(condition = "input.margin_n == 'multi'",
                     sliderInput("marginval_n2", "Margin:", min = 1.01, max = 2, value = 1.1, step = 0.01)),


    #
    selectInput("type1_n", "Type 1:", c("0.01", "0.025", "0.05", "0.1"), selected = "0.05"),
    # Input: Animation with custom interval (in ms) ----
    # to control speed, plus looping
    selectInput("power_n", "Power:", c("0.8", "0.9", "0.95"), selected = "0.9")
  ),
  
  
    # Only show this panel if Custom is selected
    conditionalPanel(
      condition = "input.dist == 'Binomial'",
      p("Additve:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } p_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
      p("Multiplicative:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } \\Delta p_T\\text{,} \\quad \\Delta > 1$$")),

      radioButtons("margin_b", "Type of margin used:",
                   c("Additive" = "add", "Multiplicative" = "multi")),
      numericInput("pC", withMathJax("Control Group Proportion,  $p_C:$"),
                   min = 0, max = 1,
                   value = 0.25, step = 0.001),
      sliderInput("marginval_b", "Margin:",
                  min = 0, max = 2,
                  value = 0.05, step = 0.002),
      selectInput("type1_b", "Type 1:", c("0.01", "0.025", "0.05", "0.1"), selected = "0.05" ),
      selectInput("power_b", "Power:", c("0.8", "0.9", "0.95"), selected = "0.9")
    ),
  
  
  conditionalPanel(
    condition = "input.dist == 'Poisson'",
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\lambda_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\Delta \\lambda_T\\text{,} \\quad \\Delta > 1$$")),

    radioButtons("margin_p", "Type of margin used:",
                 c("Additive" = "add", "Multiplicative" = "multi")),
    numericInput("lambdaC", withMathJax("Mean of Control,  $\\lambda_C:$"),
                 min = 0, max = 50,
                 value = 3, step = 0.05),
    sliderInput("marginval_p", "Margin:",
                min = 0, max = 2,
                value = 1.2, step = 0.01),
    selectInput("type1_p", "Type 1:", c("0.01", "0.025", "0.05", "0.1"), selected = "0.05"),
    selectInput("power_p", "Power:", c("0.8", "0.9", "0.95"), selected= "0.9")
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
