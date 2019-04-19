# adding stochasticity to population projections
# mar 2018

require(shiny)

ui <- navbarPage(title = "Stochastic projections",
      
      tabPanel("Projections with b, d, and r",
                          
      fluidPage(
        fluidRow(column(4,

        wellPanel(
        
          fluidRow(
            column(12,

                   fluidRow(column(4, checkboxInput("para.b", label = "Parametric uncertainty - b",
                                                    value = F)),
                            column(4, checkboxInput("para.d", label = "Parametric uncertainty - d",
                                                    value = F)),
                            column(4, checkboxInput("para.r", label = "Parametric uncertainty - r",
                                                    value = F))),
                   
                   fluidRow(column(6, checkboxInput("demo", label = "Demographic stochasticity",
                                                    value = F)),
                            column(6, checkboxInput("env", label = "Environmental stochasticity",
                                                    value = F))),
                   
                   fluidRow(column(6,  sliderInput("nyrs", label = "Number of years", value = 20,
                                                    min = 5, max = 100, step = 5)),
                            column(6, sliderInput("reps", label = "Number of replications",
                                                   value = 100, min = 0, max = 5000, step = 100))),
                   
                   fluidRow(column(6, numericInput("Ninit", label = "Initial population size",
                                                    value = 50, min = 5, max = 1000, step = 5)),
                            column(6, sliderInput("threshold", label = "Quasi-extinction threshold",
                                                  value = 1, min = 1, max = 1000))),
                   
                   fluidRow(column(12, selectInput("type", "What kind of data do you have?", 
                                                   choices = c("Birth and death rates", 
                                                               "Population growth rate (r)")))),
                   
                   fluidRow(column(4, numericInput("input.b", label = "Average birth rate",
                                                   value = 0.4, min = 0, max = 6, step = 0.1)),
                            column(4, numericInput("input.d", label = "Average death rate",
                                                   value = 0.3, min = 0, max = 1, step = 0.1)),
                            column(4, numericInput("input.r", label = "Average r", 
                                                   value = 0.6, min = -2, max = 2, step = 0.1))),
                   
                   
                   fluidRow("If including parametric uncertainty, enter possible range of parameter values."),
      
                   fluidRow(column(4, numericInput("min.b", label = "Minimum birth rate",
                                                   value = 0.2, min = 0, max = 4, step = 0.1)),
                            column(4, numericInput("min.d", label = "Minimum death rate",
                                                   value = 0.2, min = 0, max = 1, step = 0.1)),
                            column(4, numericInput("min.r", label = "Minimum r",
                                                   value = 0.2, min = -2, max = 2, step = 0.1))),
                   
                   fluidRow(column(4, numericInput("max.b", label = "Maximum birth rate",
                                                   value = 1.2, min = 0.1, max = 6, step = 0.1)),
                            column(4, numericInput("max.d", label = "Maximum death rate",
                                                   value = 0.3, min = 0.1, max = 1, step = 0.1)),
                            column(4, numericInput("max.r", label = "Maximum r", value = 1.2,
                                                   min = -2, max = 2, step = 0.1))),
                   
                  
                   fluidRow("If including environmental stochasticity, enter among-year standard deviations."),
                   
                   fluidRow(column(4, numericInput("sd.b", label = "Among-year standard deviation in birth rate",
                                                   value = 0.1, min = 0.1, max = 2, step = 0.1)),
                            column(4, numericInput("sd.d", label = "Among-year standard deviation in death rate",
                                                   value = 0.1, min = 0.1, max = 1, step = 0.1)),
                            column(4, numericInput("sd.r", label = "Among-year standard deviation in r",
                                                   value = 0.1, min = 0, max = 2, step = 0.1))),
                   
                   fluidRow(column(6, checkboxInput("dd", label = "Add density-dependence", value = F)),
                            column(6, numericInput("K", label = "Carrying capacity", value = 100, min = 0, 
                                                   max = 500000, step = 10)))
            )
          )
        )),
        column(6,
               fluidRow(plotOutput("plot1")),
               fluidRow(plotOutput("plot2")),
               fluidRow(htmlOutput("p.ext")))
         )
        )
      ),
          
        tabPanel("Environmental stochasticity in lambda",
           sidebarLayout(
              sidebarPanel(
                              
                  sliderInput("nyrs2", label = "Number of years", value = 20,
                              min = 5, max = 100, step = 5),
                  sliderInput("reps2", label = "Number of replications",
                              value = 100, min = 0, max = 5000, step = 100),
                  numericInput("Ninit2", label = "Initial population size",
                              value = 10, min = 5, max = 1000, step = 5),
                              
                  sliderInput("threshold2", label = "Quasi-extinction threshold",
                              value = 1, min = 1, max = 1000),
                              
                  numericInput("mean_lambda", label = "Average annual growth rate",
                               value = 1, min = 0, max = 3, step = 0.1),
                  numericInput("sd_lambda", label = "Among-year standard deviation in growth rate",
                               value = 0.3, min = 0.1, max = 1, step = 0.1),
                  
                  checkboxInput("dd2", label = "Add density-dependence", value = F),
                  numericInput("K", label = "Carrying capacity", value = 10, min = 0,
                               max = 10000, step = 10)
               ),
            mainPanel(
               fluidPage(
                  fluidRow(column(8, plotOutput("plot_lam1")),
                           column(4, plotOutput("lambda_hist"))),
                  fluidRow(column(8, plotOutput("plot_lam2")),
                           column(4, htmlOutput("p.ext_lam")))
               )
            )
          )
        ),
      
      tabPanel("Stage-structured model",
        
        fluidPage(
         fluidRow(column(4,
                                 
               wellPanel(
                                   
                     fluidRow(
                       column(12,
                              
                              fluidRow(column(6, checkboxInput("para3", label = "Parametric uncertainty in transition probabilities",
                                                               value = F)),
                                       column(6, actionButton("go", label = "Run projection"))),
                              
                              fluidRow(column(6, checkboxInput("demo3", label = "Demographic stochasticity",
                                                               value = F)),
                                       column(6, checkboxInput("env3", label = "Environmental stochasticity",
                                                               value = F))),
                              
                              fluidRow(column(6,  sliderInput("nyrs3", label = "Number of years", value = 20,
                                                              min = 5, max = 100, step = 5)),
                                       column(6, sliderInput("reps3", label = "Number of replications",
                                                             value = 100, min = 0, max = 5000, step = 100))),
                              
                              fluidRow(column(6, numericInput("Ninit3", label = "Initial population size",
                                                              value = 100, min = 5, max = 1000, step = 5)),
                                       column(6, sliderInput("threshold3", label = "Quasi-extinction threshold",
                                                             value = 1, min = 1, max = 1000))),
                              fluidRow(column(12)),
                              
                              fluidRow(column(12, strong("Initial stage distribution:"))),
                              fluidRow(column(12, "Enter the number of states and proportion of the population in each state at time 0")),
                              
                              fluidRow(column(3, sliderInput("nstates", label = "Number of states",
                                                             value = 6, min = 2, max = 3, step = 1)),
                                       column(3, numericInput("prop1", label = "1", value = 0.1,
                                                             min = 0, max = 1, step = 0.1)),
                                       column(3, numericInput("prop2", label = "2", value = 0.2,
                                                              min = 0, max = 1, step = 0.1)),
                                       column(3, numericInput("prop3", label = "3", value = 0.3,
                                                              min = 0, max = 1, step = 0.1))),
         
                              fluidRow(column(12)),
                              
                              fluidRow(column(12, strong("Average state transition probabilities:"))),
                              fluidRow(column(12, "Enter the average transition probabilities among states. 
                                              Set unused state transitions to 0.")),
                              
                              
                              fluidRow(column(4, numericInput("psi11", label = "1 to 1",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4, numericInput("psi12", label = "2 to 1",
                                                              value = 0.8, min = 0, max = 1, step = 0.1)),
                                       column(4, numericInput("psi13", label = "3 to 1",
                                                              value = 0.7, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("psi21", label = "1 to 2",
                                                              value = 0.5, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("psi22", label = "2 to 2",
                                                              value = 0.8, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("psi23", label = "3 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("psi31", label = "1 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("psi32", label = "2 to 3",
                                                              value = 0.3, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("psi33", label = "3 to 3",
                                                              value = 0.8, min = 0, max = 1, step = 0.1))),
                            
                              
                              fluidRow(column(12, strong("Among-year standard deviation in transition probabilities"))),
                              fluidRow(column(12, "If including environmental stochasticity, enter standard deviation for each transition below.")),
                              
                              
                              fluidRow(column(4,  numericInput("sd_psi11", label = "1 to 1",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi12", label = "2 to 1",
                                                              value = 0.1, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi13", label = "3 to 1",
                                                              value = 0.1, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("sd_psi21", label = "1 to 2",
                                                              value = 0.1, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi22", label = "2 to 2",
                                                              value = 0.1, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi23", label = "3 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("sd_psi31", label = "1 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi32", label = "2 to 3",
                                                              value = 0.1, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("sd_psi33", label = "3 to 3",
                                                              value = 0.1, min = 0, max = 1, step = 0.1))),
                              
                             
                              
                              fluidRow(column(12, strong("Minimum state transition probabilities:"))),
                              fluidRow(column(12, "If including parameteric uncertainty, enter minimum and maximum below.")),
                              
                              
                              fluidRow(column(4,  numericInput("min_psi11", label = "1 to 1",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi12", label = "2 to 1",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi13", label = "3 to 1",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("min_psi21", label = "1 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi22", label = "2 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi23", label = "3 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("min_psi31", label = "1 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi32", label = "2 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("min_psi33", label = "3 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              
                              fluidRow(column(12, strong("Maximum state transition probabilities:"))),
                              
                              
                              fluidRow(column(4,  numericInput("max_psi11", label = "1 to 1",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi12", label = "2 to 1",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi13", label = "3 to 1",
                                                              value = 0.9, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("max_psi21", label = "1 to 2",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi22", label = "2 to 2",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi23", label = "3 to 2",
                                                              value = 0, min = 0, max = 1, step = 0.1))),
                              
                              fluidRow(column(4,  numericInput("max_psi31", label = "1 to 3",
                                                              value = 0, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi32", label = "2 to 3",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)),
                                       column(4,  numericInput("max_psi33", label = "3 to 3",
                                                              value = 0.9, min = 0, max = 1, step = 0.1)))
                              
                             
                       )
                       )
                       )
                     ),
               column(6,  
                      fluidRow(plotOutput("plot_st1")),
                      fluidRow(plotOutput("plot_st2")),
                      fluidRow(htmlOutput("p.ext_st")))

      )
    )
)
)




server <- function(input, output){
  
  dat1 <- reactive({
    reps = input$reps
    nyrs = input$nyrs
    N.init = input$Ninit
    
    min.b = input$min.b
    max.b = input$max.b
    
    min.d = input$min.d
    max.d = input$max.d
    
    min.r = input$min.r
    max.r = input$max.r
    
    input.b = input$input.b
    input.d = input$input.d
    input.r = input$input.r
    
    sd.b = input$sd.b
    sd.d = input$sd.d
    sd.r = input$sd.r
    
    
    N = B = D = matrix(nrow = nyrs, ncol = reps)
    N[1,] = N.init
    
    if(input$type == "Birth and death rates"){
    
      for(r in 1:reps){
        
        mean.b = ifelse(input$para.b, runif(1, min.b, max.b), input.b)
        mean.d = ifelse(input$para.d, runif(1, min.d, max.d), input.d)
        
        if(input$env){b = abs(rnorm(nyrs, mean.b, sd.b))} else b = rep(mean.b, nyrs)
        if(input$env){d = abs(rnorm(nyrs, mean.d, sd.b)); d = d/max(d)} else d = rep(mean.d, nyrs)
        
        if(!input$dd){
          for(t in 2:nyrs){
            
            B[t-1,r] = ifelse(input$demo, rpois(1, N[t-1,r]*b[t-1]), N[t-1,r]*b[t-1])
            D[t-1,r] = ifelse(input$demo, rbinom(1, N[t-1,r], d[t-1]), N[t-1,r]*d[t-1])
            N[t,r] = ifelse(N[t-1,r] + B[t-1,r] - D[t-1,r] > 0, 
                            N[t-1,r] + B[t-1,r] - D[t-1,r], 0)
          }
        }
        
        if(input$dd){
          for(t in 2:nyrs){
            b.real = ifelse(N[t-1,r] < input$K, b[t], 0.05)
            d.real = ifelse(N[t-1,r] < input$K, d[t], 0.05)
            
            B[t-1,r] = ifelse(input$demo, rpois(1, N[t-1,r]*b.real), N[t-1,r]*b.real)
            D[t-1,r] = ifelse(input$demo, rbinom(1, N[t-1,r], d.real), N[t-1,r]*d.real)
            N[t,r] = ifelse(N[t-1,r] + B[t-1,r] - D[t-1,r] > 0, 
                            N[t-1,r] + B[t-1,r] - D[t-1,r], 0)
          } 
        }
      }
    }
    
    if(input$type == "Population growth rate (r)"){
      for(r in 1:reps){
        
        mean.r = ifelse(input$para.r, runif(1, min.r, max.r), input.r)
        
        if(input$env){r.yr = abs(rnorm(nyrs, mean.r, sd.r))} else r.yr = rep(mean.r, nyrs)
        
        if(!input$dd){
          for(t in 2:nyrs){
            N[t,r] = N[t-1,r]*exp(r.yr[t-1])
          }
        }
        
        if(input$dd){
          for(t in 2:nyrs){
            r.real = ifelse(N[t-1,r] < input$K, r.yr[t], 0)
            
            N[t,r] = N[t-1,r]*exp(r.real)
          } 
        }
      }
    }
    
    
    data.frame(N = c(N), 
               year = rep(c(1:nyrs), reps),
               rep = rep(c(1:reps), each = nyrs))
      
  })
  
  
  output$plot1 <- renderPlot({
    require(ggplot2)
    require(cowplot)
    
    ggplot(dat1(), aes(x = year, y = N, col = as.character(rep))) +
      geom_line(lwd = 2, alpha = 0.3) +
      theme(legend.position = "none") +
      xlab("Year") +
      ylab("Population size\nAll replicates") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    
  })
  
  output$plot2 <- renderPlot({
    
    require(tidyverse)
    
    dat1() %>% 
      group_by(year) %>% 
      summarize(med = median(N, na.rm = T),
                lci = quantile(N, 0.025, na.rm = T),
                uci = quantile(N, 0.975, na.rm = T)) %>% 
      ggplot(aes(x = year)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), fill = "gray30", alpha = 0.5) +
      geom_line(aes(y = med), lwd = 2) +
      geom_hline(yintercept = input$threshold, lty = 2, col = "red", lwd = 1)+
      xlab("Year") +
      ylab("Population size\nMedian and 95% quantiles") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    
    
  })
  
  output$p.ext <- renderText({
    paste("<font size=\"4\">", 
          "Quasi-extinction probability =", 
          round(mean(dat1()$N[dat1()$year == input$nyrs] < (input$threshold+1)), 3),
          "</font>")
  })
  
  dat2 <- reactive({
    
    N = matrix(nrow = input$nyrs2, ncol = input$reps2)
    N[1,] = input$Ninit2
    
    for(r in 1:input$reps2){
      
      lambda = rnorm(input$nyrs2, input$mean_lambda, input$sd_lambda)
      
      if(!input$dd2){
        for(i in 2:input$nyrs2){
          N[i,r] = ifelse(N[i-1,r]*lambda[i-1] > 0,
                          N[i-1,r]*lambda[i-1], 0)
        }
      }
      if(input$dd2){
        for(i in 2:input$nyrs2){
          lambda.real = lambda[i-1]*((input$K-N[i-1,r])/input$K)
          N[i,r] = ifelse(N[i-1,r]*lambda.real > 0,
                          N[i-1,r]*lambda.real, 0)
        }
      }
    }
    
    data.frame(N = c(N), 
               year = rep(c(1:input$nyrs2), input$reps2),
               rep = rep(c(1:input$reps2), each = input$nyrs2))
    
  })
  
  output$plot_lam1 <- renderPlot({
    require(ggplot2)
    require(cowplot)
    
    ggplot(dat2(), aes(x = year, y = N, col = as.character(rep))) +
      geom_line(lwd = 2, alpha = 0.3) +
      theme(legend.position = "none") +
      xlab("Year") +
      ylab("Population size\nAll replicates") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    
  })
  
  output$plot_lam2 <- renderPlot({
    
    require(tidyverse)
    
    dat2() %>% 
      group_by(year) %>% 
      summarize(med = median(N),
                lci = quantile(N, 0.025),
                uci = quantile(N, 0.975)) %>% 
      ggplot(aes(x = year)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), fill = "gray30", alpha = 0.5) +
      geom_line(aes(y = med), lwd = 2) +
      geom_hline(yintercept = input$threshold2, lty = 2, col = "red", lwd = 1)+
      xlab("Year") +
      ylab("Population size\nMedian and 95% quantiles") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    
  })
  
  output$lambda_hist <- renderPlot({
    x = rnorm(100000, input$mean_lambda, input$sd_lambda)
    hist(x, breaks = 100, col = "gray", main = "Distribution of possible annual growth rates", 
         xlab = "Possible values")
  })
  
  output$p.ext_lam <- renderText({
    paste("<font size=\"4\">", 
          "Quasi-extinction probability =", 
          round(mean(dat2()$N[dat2()$year == input$nyrs2] < (input$threshold2+1)), 3),
          "</font>")
  })
  
  

  dat3 <- eventReactive(input$go, {
    
    withProgress(message = "Running population projection",{
    
    dist = c(input$prop1, input$prop2, input$prop3)
    dist = dist[1:input$nstates]
    if(sum(dist) != 1) dist = dist/sum(dist)

    N = array(0, dim = c(input$nstates, 1, input$nyrs3, input$reps3))
    N[,1,,] = round(input$Ninit3*dist)
    
    # average transition 
    psi = matrix(nrow = 3, byrow = T,
                 c(input$psi11, input$psi12, input$psi13,
                   input$psi21, input$psi22, input$psi23, 
                   input$psi31, input$psi32, input$psi33))
    
    psi = psi[1:input$nstates, 1:input$nstates]
    
    
    # sd transition 
    sd_psi = matrix(nrow = 3, byrow = T,
                 c(input$sd_psi11, input$sd_psi12, input$sd_psi13, 
                   input$sd_psi21, input$sd_psi22, input$sd_psi23, 
                   input$sd_psi31, input$sd_psi32, input$sd_psi33))
    
    sd_psi = sd_psi[1:input$nstates, 1:input$nstates]
    sd_psi[which(psi == 0)] = 0
    
    # minimum transition
    min_psi = matrix(nrow = 3, byrow = T,
                 c(input$min_psi11, input$min_psi12, input$min_psi13, 
                   input$min_psi21, input$min_psi22, input$min_psi23,
                   input$min_psi31, input$min_psi32, input$min_psi33))
    
    min_psi = min_psi[1:input$nstates, 1:input$nstates]
    
    # maximum transition 
    max_psi = matrix(nrow = 3, byrow = T,
                 c(input$max_psi11, input$max_psi12, input$max_psi13,
                   input$max_psi21, input$max_psi22, input$max_psi23,
                   input$max_psi31, input$max_psi32, input$max_psi33))
    
    max_psi = max_psi[1:input$nstates, 1:input$nstates]
    
    if(input$para3){psi = min_psi}
    
    # set up transition probabilities
    psi_real = array(0, dim = c(input$nstates, input$nyrs3, input$reps3, input$nstates))
    psi_avg = array(0, dim = c(input$nstates, input$reps3, input$nstates))
    
    
    
    for(i in 1:input$nstates){
      for(j in 1:input$nstates){
        
        if(psi[i,j] != 0){
          for(r in 1:input$reps3){
              
            psi_avg[i,r,j] = ifelse(input$para3, runif(1, min_psi[i,j], max_psi[i,j]), psi[i,j])
            
          
            for(t in 1:input$nyrs3){
              psi_real[i,t,r,j] = ifelse(input$env3, abs(rnorm(1, psi_avg[i,r,j], sd_psi[i,j])), psi_avg[i,r,j])
            }
          }
        }
        
      }
    }
    
    # for(r in 1:input$reps3){
    #   for(t in 1:input$nyrs3){
    #       for(j in 1:input$nstates){
    #         psi_real[,t,r,j] = ifelse(sum(psi_real[,t,r,j]) != 1, psi_real[,t,r,j]/sum(psi_real[,t,r,j]), psi_real[,t,r,j])
    #       }
    #     }
    #   }
    
    
    
    # population projection
      
    for(r in 1:input$reps3){
      for(t in 2:input$nyrs3){
          
        if(!input$demo3){
          N[,,t,r] = N[,,t-1,r] %*% psi_real[,t-1,r,]
        }
        
        if(input$demo3){
          N[,,t,r] = rpois(N[,,t-1,r], N[,,t-1,r] %*% psi_real[,t-1,r,])
        }
          
      }
    }
    
    data.frame(N = c(N), 
               state = rep(c(1:input$nstates), input$nyrs3*input$reps3),
               year = rep(c(1:input$nyrs3), each = input$nstates, input$reps3),
               rep = rep(c(1:input$reps3), each = input$nyrs3*input$nstates))
    
    })
  })
  
  output$plot_st1 <- renderPlot({
    df <- dat3()
    
    ggplot(df, aes(x= year, y = N, col = as.character(rep))) +
      geom_line(lwd = 2, alpha = 0.3) +
      theme(legend.position = "none") +
      xlab("Year") +
      ylab("Population size\nAll replicates") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20)) +
      facet_wrap(~state, scales = "free") +
      ylim(min(df$N, na.rm = T), max(df$N, na.rm = T))
    
  })
  
  output$plot_st2 <- renderPlot({
    
    dat3() %>% 
      group_by(year, rep) %>% 
      summarize(Ntot = sum(N, na.rm = T)) %>% 
      ungroup() -> dat
      
      dat %>% 
      group_by(year) %>% 
      summarize(med = median(Ntot, na.rm = T),
                lci = quantile(Ntot, 0.025, na.rm = T),
                uci = quantile(Ntot, 0.975, na.rm = T)) %>% 
      ggplot(aes(x = year)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), fill = "gray30", alpha = 0.5) +
      geom_line(aes(y = med), lwd = 2) +
      geom_hline(yintercept = input$threshold2, lty = 2, col = "red", lwd = 1)+
      xlab("Year") +
      ylab("Population size\nMedian and 95% quantiles") +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 20))
    
  })
  
  output$p.ext_st <- renderText({
    dat3() %>% 
      group_by(year, rep) %>% 
      summarize(Ntot = sum(N, na.rm = T)) %>% 
      ungroup() -> dat
    
    paste("<font size=\"4\">", 
          "Quasi-extinction probability =", 
          round(mean(dat$Ntot[dat$year == input$nyrs3] < (input$threshold3+1)), 3),
          "</font>")
  })

}



shinyApp(ui = ui, server = server)


