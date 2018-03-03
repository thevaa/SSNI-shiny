library(shiny)
library(SSNI)
library(plotly)

fluidPage(

  # App title ----
  titlePanel("Sample Size for Non-Inferiority Trial"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      p("Additve:"),
      p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\mu_T  \\text{ + } \\Delta \\text{,} \\quad \\Delta > 0$$")),
      p("Multiplicative:"),
      p(withMathJax("$$H_0 \\text{ : } \\mu_C \\text{  =  } \\Delta\\mu_T\\text{,} \\quad \\Delta > 1$$")),
      radioButtons("margin", "Type of margin used:",
                   c("Additive" = "add", "Multiplicative" = "multi")),

      # Input: Simple integer interval ----
      numericInput("varC", "Variance of Control:",
                  min = 0, max = 100,
                  value = 20, step = 0.01),

      # Input: Decimal interval with step value ----
      numericInput("varT", "Variance of Treatment:",
                  min = 0, max = 100,
                  value = 20, step = 0.01),

      numericInput("muC", "Mean of Control:",
                   min = 0, max = 100,
                   value = 5, step = 0.01),

      sliderInput("marginval", "Margin:",
                  min = 0, max = 5,
                  value = 1.1, step = 0.01),

      #
      selectInput("type1", "Type 1:", c("0.01", "0.025", "0.05", "0.1")),
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      selectInput("power", "Power:", c("0.8", "0.9", "0.95"))
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      h3("Sample Size and Eficiency"),
      tableOutput("values"),

      h3("Plot of Efficiency"),
      plotlyOutput("plot")
    )),
    print("Thevaa Chandereng, Rick Chappell")
)
