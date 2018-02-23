library(SSNI)

function(input, output) {
  sliderValues <- reactive({
    if(input$margin == "add"){
      add.margin(delta = input$marginval, varC = input$varC, varT = input$varT,
                 alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
    }
    else{
      multi.margin(delta = input$marginval, muC = input$muC, varC = input$varC, varT = input$varT,
                 alpha = as.numeric(input$type1), beta = 1 - as.numeric(input$power))
    }
  })

  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })

}
