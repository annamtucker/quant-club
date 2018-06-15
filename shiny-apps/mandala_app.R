# mandala shiny app

require(shiny)

ui <- fluidPage(
  titlePanel("make your mandala"),
  
  plotOutput("mandala"),
  
  hr(),
  
  fluidRow(
    column(3, 
           sliderInput("iter", value = 3, min = 2, max= 5, step = 1,
                       label = "Number of iterations")),
    column(3,
           sliderInput("points", value = 6, min = 2, max = 10, step = 1,
                       label = "Number of points")),
    column(3,
           sliderInput("radius", value = 3.8, step = 0.1,min = 0, max = 10,
                       label = "Compression/expansion")),
    column(3,
           selectInput("colpal", label = "Color palette",
                       c("Spectral", "Set1", "Paired", "Oranges",
                         "Purples", "Blues", "Greens")),
           selectInput("bgd", label = "Background",
                       c("Light", "Dark")))
    
  )
  
)

server <- function(input, output){
  
  output$mandala <- renderPlot({
    
  # Load in libraries
  library(ggplot2)
  library(dplyr)
  library(deldir)

  # Parameters to change as you like
  iter= input$iter
  points= input$points
  radius= input$radius
  
  # Angles of points from center
  angles=seq(0, 2*pi*(1-1/points), length.out = points)+pi/2
  
  # Initial center
  df=data.frame(x=0, y=0)
  
  # Iterate over centers again and again
  for (k in 1:iter)
  {
    temp=data.frame()
    for (i in 1:nrow(df))
    {
      data.frame(x=df[i,"x"]+radius^(k-1)*cos(angles), 
                 y=df[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
    }
    df=temp
  }
  
  # Obtain Voronoi regions
  df %>%
    select(x,y) %>% 
    deldir(sort=TRUE) %>% 
    .$dirsgs -> data
  
  # Plot regions with geom_segment
  background = ifelse(input$bgd == "Light", "white", "gray10")
  
    data %>% 
      mutate(col = c(1:nrow(data))) %>% 
      ggplot() +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, col= col), lwd = 1.5) +
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      coord_fixed() +
      theme(legend.position  = "none",
            panel.background = element_rect(fill=background),
            panel.border     = element_rect(colour = "black", fill=NA),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank()) +
     scale_color_distiller(palette = input$colpal)
  })
  
  
  
}

shinyApp(ui = ui, server = server)
