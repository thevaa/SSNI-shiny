library(shiny)
library(SSNI)

fluidPage(

  # App title ----
  titlePanel("Sample Size for Non-Inferiority Trial"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      radioButtons("margin", "Type of margin used:",
                   c("Additive" = "add", "Multiplicative" = "multi")),

      # Input: Simple integer interval ----
      numericInput("varC", "Variance of Control:",
                  min = 0, max = 100,
                  value = 20, step = 0.001),

      # Input: Decimal interval with step value ----
      numericInput("varT", "Variance of Treatment:",
                  min = 0, max = 100,
                  value = 20, step = 0.001),

      sliderInput("marginval", "Margin:",
                  min = 0, max = 2,
                  value = 1.1, step = 0.001),

      #
      selectInput("type1", "Type 1:", c("0.01", "0.025", "0.05", "0.1")),
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      selectInput("power", "Power:", c("0.8", "0.9", "0.95"))
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      tableOutput("values")

    )
  )
)
