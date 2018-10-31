# shiny app to prototype reintroduction decisions
# 1 site, 3-stage life cycle

# alternatives:
# - number to release
# - stage to release (hatchling or juvenile)
# - number of years to release for


### Anna found mistakes in input parameters
# need to change initial values may not actually change the parameter value

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("yeti"),
   
   # Application title
   titlePanel("Reintroduction projection prototype - three-stage life cycle"),
   
   fluidRow(column(4,
   wellPanel(
   fluidRow(
     column(4, "Simulation parameters",
         numericInput("reps",
                     "Number of replicates",
                     min = 10,
                     max = 5000,
                     value = 1000,
                     step = 10),
         numericInput("tyrs",
                     "Number of years to project",
                     min = 5, 
                     max = 50,
                     value = 30,
                     step = 5),
         numericInput("Nmax",
                      "Population ceiling",
                      min = 0, 
                      max = 1000,
                      value = 500,
                      step = 10),
         numericInput("mPsamp",
                      "Detection probability",
                      min = 0,
                      max = 1,
                      value = 0.2,
                      step = 0.01)
         ),
     column(4, "Release alternatives",
         numericInput("nrel",
                      "Number of juveniles to release",
                      min = 0, 
                      max = 50,
                      value = 30,
                      step = 1),
         numericInput("nrelh",
                      "Number of hatchlings to release",
                      min = 0, 
                      max = 50,
                      value = 0, 
                      step = 1),
         numericInput("nyr",
                      "Duration of release program",
                      min = 1, 
                      max = 25,
                      value = 3, 
                      step = 1)
         ),
     column(4,
            actionButton("go",
                         "Run projection")
     )
   ),
   fluidRow(
     column(4, "Adult and juvenile survival",
            numericInput("mSa",
                         "Mean adult survival",
                         min = 0,
                         max = 1, 
                         value = 0.75,
                         step = 0.01),
            numericInput("SDmSa",
                         "SD of adult survival",
                         min = 0, 
                         max = 2,
                         value = 0.15,
                         step = 0.01),
            numericInput("mSj",
                         "Mean juvenile survival",
                         min = 0,
                         max = 1, 
                         value = 0.2,
                         step = 0.01),
            numericInput("SDmSj",
                         "SD of juvenile survival",
                         min = 0,
                         max = 2, 
                         value = 0.05,
                         step = 0.01),
            numericInput("mTja",
                         "Mean transition probability from juvenile to adult",
                         min = 0, 
                         max = 1,
                         value = 0.26,
                         step = 0.01),
            numericInput("SDmTja",
                         "SD of transition probability from juvenile to adult",
                         min = 0,
                         max = 1,
                         value = 0.05,
                         step = 0.01)
     ),
     column(4,"Hatchling survival",
            numericInput("Sh",
                         "Mean hatchling survival",
                         min = 0,
                         max = 1, 
                         value = 0.6,
                         step = 0.01),
            numericInput("SDSh",
                         "SD of hatchling survival",
                         min = 0,
                         max = 1, 
                         value = 0.1,
                         step = 0.01),
            numericInput("Sr",
                         "Post-release reduction in survival",
                         min = 0,
                         max = 1,
                         value = 0.4,
                         step = 0.01)
            ),
     column(4, "Productivity",
            numericInput("meggs",
                         "Mean number of eggs per female",
                         min = 0, 
                         max = 50,
                         value = 8.2,
                         step = 0.1),
            numericInput("Pv",
                         "Proportion of eggs viable",
                         min = 0,
                         max = 1,
                         value = 0.85,
                         step = 0.1),
            numericInput("Se",
                         "Mean nest survival",
                         min = 0,
                         max = 1,
                         value = 0.75,
                         step = 0.1),
            numericInput("SDSe",
                         "SD of nest survival",
                         min = 0,
                         max = 1,
                         value = 0.15,
                         step = 0.01)
            )
     ))),
   column(8,
   fluidRow(
     column(12,
            plotOutput("pop_projection")
            )),
   fluidRow(
     column(5, offset = 1,
            plotOutput("count")),
     column(1),
     column(3,
            fluidRow(" "),
            fluidRow(tableOutput("sum_table")))
      )
   )
   )
)


server <- function(input, output) {

  dat <- eventReactive(input$go, {
    
    withProgress(message = "Simulating population trajectories", {
    library(statmod)
    
    r = input$reps
    t = input$tyrs
    Nmax = input$Nmax
    
    nrel = input$nrel
    nrelh = input$nrelh
    nyr = input$nyr
    
    mSa = input$mSa
    SDmSa = input$SDmSa^2
    
    mSj = input$mSj
    SDmSj = input$SDmSj^2
    
    mTja = input$mTja
    SDmTja = input$SDmTja
    
    meggs = input$meggs
    Pv = input$Pv
    
    Se = input$Se
    SDSe = input$SDSe^2
    
    Sh = input$Sh
    SDSh = input$SDSh^2
    
    Sr = input$Sr
    
    mPsamp = input$mPsamp
    
    # set up data structures
    
    Na = matrix(0,r,t) #adult abundance
    Nj = matrix(0,r,t) #Juvenile Abundance
    Nh =  matrix(0,r,t)# No. of hatchlings 
    Nrh = matrix(0,r,t) # No. released hatchlings
    Nrj = matrix(0,r,t) # No. released 4 year olds
    No = matrix(0,r,t) # No. counted/observed
    
    Na[,1]=0
    Nj[,1]=0
    Nh[,1]=0
    
    #Adult survival with parametric uncertainty
    aSa = mSa*((mSa*(1-mSa)/SDmSa)-1) #	100*mSa# 	
    bSa = (1-mSa)*((mSa*(1-mSa)/SDmSa)-1) #100*(1-mSa)#
    Sai = matrix(rbeta(r,aSa,bSa),r,1)#parametric uncertainty Survival
    SDmSai = matrix(rinvgauss(r,SDmSa,1),r,1)#r,SDmSa*.9,SDmSa*1.1),r,1)#
    ASai = matrix(0,r,1)# beta distribution shape parameters
    BSai = matrix(0,r,1)# beta distribution shape parameters
    Sat = matrix(0,r,t)# annual variation survival
    
    #Juvenile survival with parametric uncertainty
    aSj = mSj*((mSj*(1-mSj)/SDmSj)-1) 		
    bSj =(1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
    Sji = matrix(rbeta(r,aSj,bSj),r,1)#parametric uncertainty Survival
    SDmSji = matrix(rinvgauss(r,SDmSj,1),r,1)
    ASji = matrix(0,r,1)# beta distribution shape parameters
    BSji = matrix(0,r,1)# beta distribution shape parameters
    Sjt = matrix(0,r,t)# annual variation survival
    
    #Transition from juv to adult with parametric uncertainty
    aTja=mSj*((mSj*(1-mSj)/SDmSj)-1)
    bTja=(1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
    Tja = matrix(rbeta(r*t,aTja,bTja),r,t)
    
    #Observation uncertainty parameters
    SDmPsamp = (mPsamp*0.1)*(mPsamp*0.1)
    aPs= mPsamp*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
    bPs=(1-mPsamp)*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
    Psamp = matrix(rbeta(r*t,aPs,bPs),r,t)
    
    #Productivity and recruitment parameters
    eggs = matrix(0,r,t)
    aPv = 100*Pv
    bPv = 100*(1-Pv)
    Pvt=matrix(rbeta(r*t,aPv,bPv),r,t)
    
    aSe=Se*((Se*(1-Se)/SDSe)-1)
    bSe=(1-Se)*((Se*(1-Se)/SDSe)-1)
    Set=matrix(0,r,t)
    Pbt=matrix(0,r,t) #proportion that breed
    
    PGY = 1
    GY=matrix(rbinom(r*t,1,PGY),r,t) #Good year / bad year dynamics

    #Hatchling survival
    ash=Sh*((Sh*(1-Sh)/SDSh)-1)
    bsh=(1-Sh)*((Sh*(1-Sh)/SDSh)-1)
    Sht=matrix(0,r,t)
    
    #Acclimation effect on Survival rate of captive released snakes is 50% of wild snake survival
    Srt=matrix(0,r,t)
    
    #population growth estimates
    rlam=matrix(0,r,t)
    olam=matrix(0,r,t)
    
    Pext=matrix(0,r,t)
    
    for(i in 1:r){
      # Draws for replicate level means for adult and juvenile survival
      ASai[i] = 100*Sai[i]#Sai[i]*((Sai[i]*(1-Sai[i])/SDmSai[i])-1) 		
      BSai[i] = 100*(1-Sai[i])#(1-Sai[i])*((Sai[i]*(1-Sai[i])/SDmSai[i])-1)
      ASji[i] = 100*Sji[i]# Sji[i]*((Sji[i]*(1-Sji[i])/SDmSji[i])-1) 		
      BSji[i] = 100*(1-Sji[i])#(1-Sji[i])*((Sji[i]*(1-Sji[i])/SDmSji[i])-1)
      
      for(j in 1:t){
        
        #draws for annual demographic rates
        Sat[i,j]=rbeta(1,ASai[i],BSai[i])
        Sjt[i,j]=rbeta(1,ASji[i],BSji[i])
        Srt[i,j]=rbeta(1,100*Sr, 100*(1-Sr)) #randomize the acclimation effect
        Sht[i,j]=rbeta(1,ash,bsh)
        Set[i,j]=rbeta(1,aSe,bSe)
        
        #drawing the number of released hatchlings and juveniles
        if (j>1 && j<nyr) Nrj[i,j]=runif(1,0.8*nrel,1.2*nrel)
        if (j>1 && j<nyr) Nrh[i,j]=runif(1,0.8*nrelh,1.2*nrelh)
        
        #Projection equation for adults
        if (j> 1) Na[i,j]=round(Na[i,j-1]*Sat[i,j-1]+Nj[i,j-1]*Tja[i,j-1]+(Nrj[i,j-1]*Srt[i,j-1]*Sjt[i,j-1]),0) else Na[i,j]=0
        round(Na[i,j],0)
        
        #implement observation error for adult "counts" / monitoring data
        No[i,j]=sum(rbinom(Na[i,j],1,Psamp[i,j]))
        
        #projection equation for Juveniles
        if (j> 1) Nj[i,j]= (Nj[i,j-1]*Sjt[i,j-1])+(Nh[i,j-1]*Sht[i,j-1])+(Nrh[i,j-1]*Srt[i,j-1]*Sht[i,j-1])
        round(Nj[i,j],0)
        
        #Good year / bad year function for probability of breeding
        if (GY[i,j]==1) Pbt[i,j]=1 else Pbt[i,j]=runif(1,.7,.8)
        
        #Population ceiling limits the proportion of females that breed to ~20%
        if (Na[i,j]>Nmax)Pbt[i,j]=runif(1,.15,.25)
        if (Na[i,j]>Nmax*2)Pbt[i,j]=0
        
        #calculate the number of eggs produced each year:
        eggs[i,j] = sum(rpois(round(Na[i,j]*Pbt[i,j],0),meggs))/2
        round(eggs[i,j],0)
        
        #Calculate the number of hatchlings produced each year
        if (j>1) Nh[i,j]=eggs[i,j-1]*Pvt[i,j-1]*Set[i,j-1]
        round(Nh[i,j],0)
        
        #calculate population growth rate
        if(j>5)rlam[i,j]=Na[i,j]/Na[i,j-1]
        if(j>5 && No[i,j-1]>0)olam[i,j]=No[i,j]/No[i,j-1]
        if (Na[i,j]<5) Pext[i,j]=1 else Pext[i,j]=0 
      }
    }
    
    MedNa = apply(Na,2,median)
    lbNa = apply(Na, 2, quantile, probs = c(0.025)) 
    ubNa = apply(Na,2,quantile, probs = c(0.975))
    MedNj = apply(Nj,2,median)
    MedNh = apply(Nh,2,median)
    MedNo = apply(No,2,median)
    
    data.frame(rep = rep(1:r, t),
               year = rep(1:t, each = r),
               Na = c(Na),
               Nj = c(Nj),
               Nh = c(Nh),
               No = c(No),
               lam.real = c(rlam),
               lam.obs = c(olam),
               Pext = c(Pext))
    })
    })
  
   output$pop_projection <- renderPlot({
     withProgress(message = "Plotting outcomes", {
     library(tidyverse)
     library(cowplot)
     library(viridis)
     
     
     maxN <- dat() %>%
       select(rep, year, Na, Nj, Nh) %>%
       gather(stage, N, 3:5) %>%
       group_by(year, stage) %>%
       summarize(uci = quantile(N, probs = 0.975)) %>%
       ungroup() %>%
       summarize(max = max(uci)) %>%
       c()
    

    dat() %>%
       select(rep, year, Na, Nj, Nh) %>%
       gather(stage, N, 3:5) %>%
       mutate(stage = c("Na" = "Adults",
                        "Nj" = "Juveniles",
                        "Nh" = "Hatchlings")[stage],
              stage = fct_relevel(stage, c("Adults", "Juveniles", "Hatchlings"))) %>%
       group_by(year, stage) %>%
       summarize(med = median(N, na.rm = T),
                 lci = quantile(N, probs = 0.025, na.rm = T),
                 uci = quantile(N, probs = 0.975, na.rm = T)) %>%
       ungroup() %>%
       ggplot(aes(x = year)) +
       geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#709d66",
                   alpha = 0.3) +
       geom_line(aes(y = lci, lty = "b"), lwd = 0.5, col = "#4d7321") +
       geom_line(aes(y = uci, lty = "b"), lwd = 0.5, col = "#4d7321") +
       geom_line(aes(y = med, lty = "a"), lwd = 1, col = "#4d7321") +
       scale_linetype_manual(values = c("a" = 1,
                                        "b" = 2),
                             labels = c("Median",
                                        "95% quantiles"),
                             name = "") +
       facet_wrap(~stage, scales = "free") +
       ylim(0, maxN[[1]]) +
       xlab("Year") +
       ylab("Number of individuals") +
       theme(legend.position = "top",
             strip.background = element_rect(fill = "white"),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 14),
             strip.text = element_text(size = 14),
             axis.title = element_text(size = 14)) +
      ggtitle("Population projections")
     })

   })
   
   output$sum_table <- renderTable(align = "c", {
     dat() %>% 
       filter(year == input$tyrs) %>% 
       summarize(`True lambda` = round(median(lam.real, na.rm = T),2),
                 `Observed lambda` = round(median(lam.obs, na.rm = T),2),
                 `Extinction probability` = mean(Pext))
     
     
   })
   
   output$count <- renderPlot({

     
     dat() %>%
       group_by(year) %>% 
       summarize(med = median(No, na.rm = T),
                 lci = quantile(No, probs = 0.025, na.rm = T),
                 uci = quantile(No, probs = 0.975, na.rm = T)) %>%
       ungroup() %>%
       ggplot(aes(x = year)) +
       geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#6caddf",
                   alpha = 0.3) +
       geom_line(aes(y = lci, lty = "b"), lwd = 0.5, col = "#3a55b4") +
       geom_line(aes(y = uci, lty = "b"), lwd = 0.5, col = "#3a55b4") +
       geom_line(aes(y = med, lty = "a"), lwd = 1, col = "#3a55b4") +
       scale_linetype_manual(values = c("a" = 1,
                                        "b" = 2),
                             labels = c("Median",
                                        "95% quantiles"),
                             name = "") +
       xlab("Year") +
       ylab("Number of individuals") +
       theme(legend.position = "top",
             strip.background = element_rect(fill = "white"),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 14),
             strip.text = element_text(size = 14),
             axis.title = element_text(size = 14)) +
       ggtitle("Observed counts")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

