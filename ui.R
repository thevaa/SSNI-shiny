library(shiny)
library(plotly)

fluidPage(
  # App title 
  titlePanel("Sample Size in Non-Inferiority Trial"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
  # Sidebar layout with input and output definitions ----
  selectInput("dist", "Distribution of the data", c(Normal = "Normal", Binomial = "Binomial", Poisson = "Poisson"), selected = "Binomial"),
  
  # Sidebar layout with input and output definitions ----
  selectInput("null", "Null", c(Less = "Less", Greater = "Greater"), 
              selected = "Greater"),
  
  #only show panel if the condition is met
  conditionalPanel(
    condition = "input.dist == 'Normal'",
    p("Null: Greater"),
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\mu_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\Delta\\mu_T\\text{,} \\quad \\Delta > 1$$")),
    p("Null: Less"),
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_T \\text{  =  } \\mu_C  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\mu_T \\text{  =  } \\Delta\\mu_C\\text{,} \\quad \\Delta > 1$$")),

    radioButtons("margin_n", "Type of margin used:",
                 c("Additive" = "add", "Multiplicative" = "multi")),


    # Input: Simple integer interval ----
    numericInput("varC", withMathJax("Variance of Control: ,  $\\sigma_C^2:$"),
                 min = 0, max = 100,
                 value = 20, step = 0.01),
    
    # Input: Decimal interval with step value ----
    numericInput("varT", withMathJax("Variance of Treatment: ,  $\\sigma_T^2:$"),
                 min = 0, max = 100,
                 value = 10, step = 0.01),
    
    conditionalPanel(
      condition = "input.null == 'Greater'", 
      numericInput("muC", withMathJax("Mean of Control: ,  $\\mu_C:$"),
                   min = 0, max = 100,
                   value = 5, step = 0.01)),
    
    conditionalPanel(
      condition = "input.null == 'Less'", 
      numericInput("muC", withMathJax("Mean of Treatmentl: ,  $\\mu_T:$"),
                   min = 0, max = 100,
                   value = 5, step = 0.01)),
    


    conditionalPanel(condition = "input.margin_n == 'add'",
                    sliderInput("marginval_n1", withMathJax("Margin,  $\\Delta$:"), min = 0.01, max = 5, value = 0.4, step = 0.01)),

    conditionalPanel(condition = "input.margin_n == 'multi'",
                     sliderInput("marginval_n2", withMathJax("Margin,  $\\Delta$:"), min = 1.01, max = 2, value = 1.1, step = 0.01)),


    #
    selectInput("type1_n", withMathJax("Type I error,  $\\alpha$ (one-sided):"), c("0.01", "0.025", "0.05", "0.1"), selected = "0.05"),
    # Input: Animation with custom interval (in ms) ----
    # to control speed, plus looping
    selectInput("power_n", withMathJax("Type II error,  $1 - \\beta$:"), c("0.8", "0.9", "0.95"), selected = "0.9")
  ),
  
  
    # Only show this panel if Custom is selected
    conditionalPanel(
      condition = "input.dist == 'Binomial'",
      p("Null: Greater"),
      p("Additve:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } p_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
      p("Multiplicative:"),
      p(withMathJax("$$H_0 \\text{ : } p_C \\text{  =  } \\Delta p_T\\text{,} \\quad \\Delta > 1$$")),
      p("Null: Less"),
      p("Additve:"),
      p(withMathJax("$$H_0 \\text{ : } p_T \\text{  =  } p_C  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
      p("Multiplicative:"),
      p(withMathJax("$$H_0 \\text{ : } p_T \\text{  =  } \\Delta p_C\\text{,} \\quad \\Delta > 1$$")),

      radioButtons("margin_b", "Type of margin used:",
                   c("Additive" = "add", "Multiplicative" = "multi")),
      
      conditionalPanel(
        condition = "input.null == 'Greater'", 
        numericInput("pC", withMathJax("Control Group Proportion,  $p_C:$"),
                     min = 0, max = 1,
                     value = 0.25, step = 0.001)),
      
      conditionalPanel(
        condition = "input.null == 'Less'", 
        numericInput("pC", withMathJax("Treatment Group Proportion,  $p_T:$"),
                     min = 0, max = 1,
                     value = 0.25, step = 0.001)),
      

      conditionalPanel(condition = "input.margin_b == 'add'",
      sliderInput("marginval_b1", withMathJax("Margin,  $\\Delta$:"), min = 0.001, max = 1 , value = 0.10, step = 0.001)),

      conditionalPanel(condition = "input.margin_b == 'multi'",
      sliderInput("marginval_b2", withMathJax("Margin,  $\\Delta$:"), min = 1.001, max = 2, value = 1.1, step = 0.001)),


      selectInput("type1_b", withMathJax("Type I error,  $\\alpha$ (one-sided):"), c("0.01", "0.025", "0.05", "0.1"), selected = "0.05" ),
      selectInput("power_b", withMathJax("Type II error,  $1 - \\beta$:"), c("0.8", "0.9", "0.95"), selected = "0.9")
    ),
  
  
  conditionalPanel(
    condition = "input.dist == 'Poisson'",
    p("Null: Greater"),
    p("Additve:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\lambda_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_C \\text{  =  } \\Delta \\lambda_T\\text{,} \\quad \\Delta > 1$$")),
    p("Null: Less"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_T \\text{  =  } \\lambda_C  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
    p("Multiplicative:"),
    p(withMathJax("$$H_0 \\text{ : } \\lambda_T \\text{  =  } \\Delta \\lambda_C\\text{,} \\quad \\Delta > 1$$")),

    radioButtons("margin_p", "Type of margin used:",
                 c("Additive" = "add", "Multiplicative" = "multi")),
    
    conditionalPanel(
      condition = "input.null == 'Greater'", 
      numericInput("lambdaC", withMathJax("Mean of Control,  $\\lambda_C:$"),
                 min = 0, max = 50,
                 value = 3, step = 0.05)),
    
    conditionalPanel(
      condition = "input.null == 'Less'", 
      numericInput("lambdaC", withMathJax("Mean of Treatment,  $\\lambda_T:$"),
                   min = 0, max = 50,
                   value = 3, step = 0.05)),
    
    conditionalPanel(condition = "input.margin_p == 'add'",
                     sliderInput("marginval_p1", withMathJax("Margin,  $\\Delta$:"), min = 0.01, max = 3, value = 0.5, step = 0.01)),
    
    conditionalPanel(condition = "input.margin_p == 'multi'",
                     sliderInput("marginval_p2", withMathJax("Margin,  $\\Delta$:"), min = 1.01, max = 2, value = 1.1, step = 0.01)),
    
    
    
    selectInput("type1_p", withMathJax("Type I error,  $\\alpha$ (one-sided):"), c("0.01", "0.025", "0.05", "0.1"), selected = "0.05"),
    selectInput("power_p", withMathJax("Type II error,  $1 - \\beta$:"), c("0.8", "0.9", "0.95"), selected= "0.9")
    ),
    p("MIT License"),
    p("Copyright (c) 2018 Thevaa Chandereng & Richard Chappell")
    ),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      h3("Sample Size and Eficiency"),
      tableOutput("values"),

      h3("Plot of Efficiency"),
      plotlyOutput("plot")
    ))
  )
