# shiny app basics
# this is an app that shows you what different beta distributions look like

require(shiny)

# define the user interface
# includes all user inputs and the layout of what users will see
# refers to objects created in the server function

ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      
    checkboxInput("giveshape", value = T, label = "Input shape parameters"),
    sliderInput("a", value = 5, label = bquote("alpha"), min = 0.5, max = 100, step = 0.5),
    sliderInput("b", value = 50, label = bquote("beta"), min = 0.5, max = 100, step = 0.5),
    
    checkboxInput("givemean", value = F, label = "Input mean and variance"),
    numericInput("avg", value = 0.75, min = 0, max = 1, label = "Mean", step = 0.05),
    numericInput("sd", value = 0.15, label = "Standard deviation", step = 0.01)
    
    
  ),
  
  mainPanel(
    plotOutput("plot1"),
    textOutput("tab1")
  )
  )

)

server <- function(input, output){

    
  output$plot1 <- renderPlot({
  
  if(input$giveshape){
    x = rbeta(1000000, input$a, input$b)
    plot(density(x), xlab = "Value", main = "", lwd = 3, xlim = c(0,1))
  }
  
  if(input$givemean){
    sig2 = input$sd^2
    
    a = input$avg*(input$avg*(1-input$avg)/sig2 - 1)
    b = a*((1-input$avg)/input$avg)
    
    #alpha <- ((1 - input$avg) / input$variance - 1 / input$avg) * input$avg ^ 2
    #beta <- alpha * (1 / input$avg - 1)
    y = rbeta(1000000, a, b)
    plot(density(y), xlab = "Value", main = "", lwd = 3, xlim = c(0,1))
  }
  })
  
  output$tab1 <- renderText({
    
    if(input$giveshape){
      avg = round(input$a/(input$a+input$b), 3)
      sig2 = round(sqrt((input$a*input$b)/((input$a+input$b)^2 * (input$a + input$b + 1))), 3)
      
      paste("mean = ", avg, ", sd = ", sig2)
    }
    
    if(input$givemean){
      sig2 = input$sd^2
      
      a = round(input$avg*(input$avg*(1-input$avg)/sig2 - 1), 3)
      b = round(a*((1-input$avg)/input$avg), 3)
      
      paste("alpha = ", a, ", beta = ", b)
    }
    
  })
}

shinyApp(ui = ui, server = server)
