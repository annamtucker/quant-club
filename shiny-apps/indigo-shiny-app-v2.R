# shiny app to prototype reintroduction decisions
# 1 site, 3-stage life cycle

# alternatives:
# - number to release
# - stage to release (hatchling or juvenile)
# - number of years to release for


### 1) do i need to change SD values to variance values, 
### and change the way the SD are calculated in A & B params

### 2) do fecundity and prop viable eggs need SE estimates ?

### 3) check that good year / bad year dynamics make it into the model


require(shiny)
library(shiny)
library(shinythemes)
library(tidyverse)
library(cowplot)
library(viridis)

ui <- fluidPage(
  #theme = shinytheme("yeti"),
   themeSelector(),
   # Application title
   titlePanel("Indigo Snake reintroduction projection; five-stage life cycle"),
   
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
         numericInput("nrelsa",
                      "Number of subadults to release",
                      min = 0, 
                      max = 50,
                      value = 30,
                      step = 1),
         numericInput("nrelj",
                      "Number of juveniles to release",
                      min = 0, 
                      max = 50,
                      value = 0, 
                      step = 1),
         numericInput("nyr",
                      "Length of release program",
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
     column(4, "Adult survival",
            numericInput("mSa",
                         "Mean adult survival",
                         min = 0,
                         max = 1, 
                         value = 0.85,
                         step = 0.01),
            numericInput("SDmSa",
                         "SD of adult survival",
                         min = 0, 
                         max = 2,
                         value = 0.1,
                         step = 0.01),
            numericInput("mSa1",
                         "Mean primiparous adult survival",
                         min = 0,
                         max = 1, 
                         value = 0.8,
                         step = 0.01),
            numericInput("SDmSa1",
                         "SD of primiparous adult survival",
                         min = 0, 
                         max = 2,
                         value = 0.1,
                         step = 0.01),
            numericInput("mSsa",
                         "Mean subadult survival",
                         min = 0,
                         max = 1, 
                         value = 0.7,
                         step = 0.01),
            numericInput("SDmSsa",
                         "SD of subadult survival",
                         min = 0,
                         max = 2, 
                         value = 0.1,
                         step = 0.01),
			numericInput("mSj",
                         "Mean juvenile survival",
                         min = 0,
                         max = 1, 
                         value = 0.6,
                         step = 0.01),
            numericInput("SDmSj",
                         "SD of juvenile survival",
                         min = 0,
                         max = 2, 
                         value = 0.1,
                         step = 0.01),
			numericInput("mSh",
                         "Mean hatchling survival",
                         min = 0,
                         max = 1, 
                         value = 0.5,
                         step = 0.01),
            numericInput("SDmSh",
                         "SD of hatchling survival",
                         min = 0,
                         max = 2, 
                         value = 0.1,
                         step = 0.01),
            numericInput("Sr",
                         "Post-release reduction in survival",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.01)
     ),
     column(4,"Transition probabilities",
            numericInput("mTa1a",
                         "Mean transition probability from primiparous adult to adult",
                         min = 0, 
                         max = 1,
                         value = 0.96,
                         step = 0.01),
            numericInput("SDmTa1a",
                         "SD of transition probability from juvenile to adult",
                         min = 0,
                         max = 1,
                         value = 0.01,
                         step = 0.01),
            numericInput("mTsaa1",
                         "Mean transition probability from subadult to primiparous adult",
                         min = 0, 
                         max = 1,
                         value = 0.80,
                         step = 0.01),
            numericInput("SDmTsaa1",
                         "SD of transition probability from subadult to primiparous adult",
                         min = 0,
                         max = 1,
                         value = 0.05,
                         step = 0.01),
            numericInput("mTjsa",
                         "Mean transition probability from juvenile to subadult",
                         min = 0, 
                         max = 1,
                         value = 0.90,
                         step = 0.01),
            numericInput("SDmTjsa",
                         "SD of transition probability from juvenile to subadult",
                         min = 0,
                         max = 1,
                         value = 0.03,
                         step = 0.01),
            numericInput("mThj",
                         "Mean transition probability from hatchling to juvenile",
                         min = 0, 
                         max = 1,
                         value = 0.98,
                         step = 0.01),
            numericInput("SDmTja",
                         "SD of transition probability from hatchling to juvenile",
                         min = 0,
                         max = 1,
                         value = 0.01,
                         step = 0.01)
            ),
     column(4, "Productivity",
            numericInput("muFa",
                         "Mean fecundity of adult females",
                         min = 0, 
                         max = 20,
                         value = 8.6,
                         step = 0.1),
            numericInput("Pva",
                         "Proportion of viable eggs for adult females",
                         min = 0,
                         max = 1,
                         value = 0.85,
                         step = 0.1),
            numericInput("muFa1",
                         "Mean fecundity of primiparous females",
                         min = 0, 
                         max = 20,
                         value = 8.5,
                         step = 0.1),
            numericInput("Pva1",
                         "Proportion of viable eggs for primiparous females",
                         min = 0,
                         max = 1,
                         value = 0.35,
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
                         step = 0.01),
            numericInput("mSR",
                         "Mean sex ratio",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.1),
            numericInput("SDSR",
                         "SD of sex ratio",
                         min = 0,
                         max = 1,
                         value = 0.04,
                         step = 0.01),
            numericInput("PB",
                         "Proportion of individuals that breed",
                         min = 0,
                         max = 1,
                         value = 0.8,
                         step = 0.1),
            numericInput("SDPB",
                         "SD of proportion that breed",
                         min = 0,
                         max = 1,
                         value = 0.1,
                         step = 0.01)
            )
     ))),
   column(8,
   fluidRow(
     column(12,
            plotOutput("pop_projection", height = "800px")
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
    
    nrelsa = input$nrelsa
    nrelj = input$nrelj
    nyr = input$nyr
    
    mSa = input$mSa
    SDmSa = input$SDmSa^2
    
    mSa1 = input$mSa1
    SDmSa1 = input$SDmSa1^2
    
    mSsa = input$mSsa
    SDmSsa = input$SDmSsa^2
        
    mSj = input$mSj
    SDmSj = input$SDmSj^2
    
    mSh = input$mSh
    SDmSh = input$SDmSh^2

    Sr = input$Sr
    
    mTa1a = input$mTa1a
    SDmTa1a = input$SDmTa1a
    
    mTsaa1 = input$mTsaa1
    SDmTsaa1 = input$SDmTsaa1
    
    muFa = input$muFa    
    Pva = input$Pva
    
    muFa1 = input$muFa1
    Pva1 = input$Pva1
    
    Se = input$Se
    SDSe = input$SDSe^2
    
    mSR = input$mSR
    SDSR = input$SDSR
        
    mPsamp = input$mPsamp
    
    # set up data structures
    
    Na = matrix(0,r,t) #adult abundance
    Na1 = matrix(0,r,t) #adult abundance
    Nsa = matrix(0,r,t) #adult abundance
    Nj = matrix(0,r,t) #Juvenile Abundance
    Nh =  matrix(0,r,t)# No. of hatchlings 
    Nsar = matrix(0,r,t) # No. released head-started two-year old subadults
    Njr = matrix(0,r,t) # No. released one-year old juveinles
    No = matrix(0,r,t) # No. counted/observed
    
    Na[,1]=0
    Na1[,1]=0
    Nsa[,1]=0
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

    #Primiparous adult survival with parametric uncertainty
    aSa1 = mSa1*((mSa1*(1-mSa1)/SDmSa1)-1) #	100*mSa# 	
    bSa1 = (1-mSa1)*((mSa1*(1-mSa1)/SDmSa1)-1) #100*(1-mSa)#
    Sa1i = matrix(rbeta(r,aSa1,bSa1),r,1)#parametric uncertainty Survival
    SDmSa1i = matrix(rinvgauss(r,SDmSa1,1),r,1)#r,SDmSa*.9,SDmSa*1.1),r,1)#
    ASa1i = matrix(0,r,1)# beta distribution shape parameters
    BSa1i = matrix(0,r,1)# beta distribution shape parameters
    Sa1t = matrix(0,r,t)# annual variation survival
 
    #Subadult survival with parametric uncertainty
    aSsa = mSsa*((mSsa*(1-mSsa)/SDmSsa)-1) #	100*mSa# 	
    bSsa = (1-mSsa)*((mSsa*(1-mSsa)/SDmSsa)-1) #100*(1-mSa)#
    Ssai = matrix(rbeta(r,aSsa,bSsa),r,1)#parametric uncertainty Survival
    SDmSsai = matrix(rinvgauss(r,SDmSsa,1),r,1)#r,SDmSa*.9,SDmSa*1.1),r,1)#
    ASsai = matrix(0,r,1)# beta distribution shape parameters
    BSsai = matrix(0,r,1)# beta distribution shape parameters
    Ssat = matrix(0,r,t)# annual variation survival   
       
    #Juvenile survival with parametric uncertainty
    aSj = mSj*((mSj*(1-mSj)/SDmSj)-1) 		
    bSj =(1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
    Sji = matrix(rbeta(r,aSj,bSj),r,1)#parametric uncertainty Survival
    SDmSji = matrix(rinvgauss(r,SDmSj,1),r,1)
    ASji = matrix(0,r,1)# beta distribution shape parameters
    BSji = matrix(0,r,1)# beta distribution shape parameters
    Sjt = matrix(0,r,t)# annual variation survival
    
    #Hatchling survival with parametric uncertainty
    aSh = mSh*((mSh*(1-mSh)/SDmSh)-1) 		
    bSh =(1-mSh)*((mSh*(1-mSh)/SDmSh)-1)
    Shi = matrix(rbeta(r,aSh,bSh),r,1)#parametric uncertainty Survival
    SDmShi = matrix(rinvgauss(r,SDmSh,1),r,1)
    AShi = matrix(0,r,1)# beta distribution shape parameters
    BShi = matrix(0,r,1)# beta distribution shape parameters
    Sht = matrix(0,r,t)# annual variation survival

    #Transition from primiparous to adult female with parametric uncertainty
    aTa1a=mSa1*((mSa1*(1-mSa1)/SDmSa1)-1)
    bTa1a=(1-mSa1)*((mSa1*(1-mSa1)/SDmSa1)-1)
    Ta1a = matrix(rbeta(r*t,aTa1a,bTa1a),r,t)
    
    #Transition from subadult to primiparous female with parametric uncertainty
    aTsaa1=mSsa*((mSsa*(1-mSsa)/SDmSsa)-1)
    bTsaa1=(1-mSsa)*((mSsa*(1-mSsa)/SDmSsa)-1)
    Tsaa1 = matrix(rbeta(r*t,aTsaa1,bTsaa1),r,t)

    #Transition from juv to adult with parametric uncertainty
    aTjsa=mSj*((mSj*(1-mSj)/SDmSj)-1)
    bTjsa=(1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
    Tjsa = matrix(rbeta(r*t,aTjsa,bTjsa),r,t)

    #Transition from hatchling to juvenile with parametric uncertainty
    aThj=mSh*((mSh*(1-mSh)/SDmSh)-1)
    bThj=(1-mSh)*((mSh*(1-mSh)/SDmSh)-1)
    Thj = matrix(rbeta(r*t,aThj,bThj),r,t)
        
    #Observation uncertainty parameters
    SDmPsamp = (mPsamp*0.1)*(mPsamp*0.1)
    aPs= mPsamp*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
    bPs=(1-mPsamp)*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
    Psamp = matrix(rbeta(r*t,aPs,bPs),r,t)
    
    #Productivity and recruitment parameters
    # Adult females
    eggsa = matrix(0,r,t)
    aPva = 100*Pva
    bPva = 100*(1-Pva)
    Pvat=matrix(rbeta(r*t,aPva,bPva),r,t)
    
    # Primiparous females
    eggsa1 = matrix(0,r,t)
    aPva1 = 100*Pva1
    bPva1 = 100*(1-Pva1)
    Pva1t=matrix(rbeta(r*t,aPva1,bPva1),r,t)
    
    # Egg survival
    aSe=Se*((Se*(1-Se)/SDSe)-1)
    bSe=(1-Se)*((Se*(1-Se)/SDSe)-1)
    Set=matrix(0,r,t)
    
    # Sex ratio
    aSR=mSR*((mSR*(1-mSR)/(SDSR))-1)
	bSR=(1-mSR)*((mSR*(1-mSR)/(SDSR))-1)
	SRi = matrix(rbeta(r,aSR,bSR),r,1)			 
	SDmSRi = matrix(rinvgauss(r,aSR^2,1),r,1)
	ASRi = matrix(0,r,1)			
	BSRi = matrix(0,r,1)
	SRt = matrix(0,r,t)				

    # Proportion that breed & good year/bad year dynamics
    Pbt=matrix(0,r,t) 
    PGY = 0.8
    GY=matrix(rbinom(r*t,1,PGY),r,t) 

    
    # Acclimation effect on Survival rate of captive released snakes
    # is 50% of wild snake survival
    Srt=matrix(0,r,t)
    
    # Estimates of population growth and extinction risk
    rlam=matrix(0,r,t)
    olam=matrix(0,r,t)
    Pext=matrix(0,r,t)
    
    for(i in 1:r){
      # Draws for replicate level means for adult and juvenile survival
	ASai[i] = 100*Sai[i]			# Adults 		
	BSai[i] = 100*(1-Sai[i])		

	ASa1i[i] = 100*Sa1i[i]		 	# Primiparous adults	
	BSa1i[i] = 100*(1-Sa1i[i])

	ASsai[i] = 100*Ssai[i]		 	# Subadults	
	BSsai[i] = 100*(1-Ssai[i])

	ASji[i] = 100*Sji[i]			# Juveniles  		
	BSji[i] = 100*(1-Sji[i])

	AShi[i] = 100*Shi[i]		 	# Hatchlings	
	BShi[i] = 100*(1-Shi[i])
      
      for(j in 1:t){
        
        #draws for annual demographic rates
	Sat[i,j]=rbeta(1,ASai[i],BSai[i])			# Adults
	Sa1t[i,j]=rbeta(1,ASa1i[i],BSa1i[i])		# Primiparous adults
	Ssat[i,j]=rbeta(1,ASsai[i],BSsai[i])		# Subadults
	Sjt[i,j]=rbeta(1,ASji[i],BSji[i])			# Juveniles
	Sht[i,j]=rbeta(1,aSh,bSh)					# Hatchlings
	Set[i,j]=rbeta(1,aSe,bSe)					# Egg survival
	Srt[i,j]=rbeta(1,100*Sr, 100*(1-Sr)) 		# Randomize the acclimation effect
        
        #drawing the number of released subadults and juveniles
        if (j>0 && j<nyr+1) Nsar[i,j]=runif(1,0.8*nrelsa,1.2*nrelsa)*rbeta(1,aSR,bSR)
		if (j>0 && j<nyr+1) Njr[i,j]=runif(1,0.8*nrelj,1.2*nrelj)*rbeta(1,aSR,bSR)
        
        #Projection equation for adults
        if (j>1) Na[i,j]=round(Na[i,j-1]*Sat[i,j-1] + 
                                  Na1[i,j-1]*Ta1a[i,j-1]) else Na[i,j]=0
		round(Na[i,j],0)
        
        #implement observation error for adult "counts" / monitoring data
		No[i,j]=sum(rbinom(Na[i,j],1,Psamp[i,j]))
        
        # Projection equation for primiparous adults
		if (j>1) Na1[i,j]= (Na1[i,j-1]*(Sa1t[i,j-1]*(1-Ta1a[i,j-1])))+(Nsa[i,j-1]*Tsaa1[i,j-1])+
		(Nsar[i,j-1]*Ssat[i,j-1]*Tsaa1[i,j-1]*Srt[i,j-1])
		round(Na1[i,j],0)
		
		# Projection equation for subadults
		if (j>1) Nsa[i,j]= (Nsa[i,j-1]*(Ssat[i,j-1]*(1-Tsaa1[i,j-1])))+(Nj[i,j-1]*Tjsa[i,j-1])+				(Njr[i,j-1]*Sjt[i,j-1]*Tjsa[i,j-1]*Srt[i,j-1])
		round(Nsa[i,j],0)
        
        # Projection equation for juveniles
		if (j>1) Nj[i,j]= (Nj[i,j-1]*Sjt[i,j-1])+(Nh[i,j-1]*Thj[i,j-1])
		round(Nj[i,j],0)

        #Good year / bad year function for probability of breeding
        if (GY[i,j]==1) Pbt[i,j]=1 else Pbt[i,j]=runif(1,.7,.8)
        
        #Population ceiling limits the proportion of females that breed to ~20%
        if (Na[i,j]>Nmax)Pbt[i,j]=runif(1,.15,.25)
        if (Na[i,j]>Nmax*2)Pbt[i,j]=0
        
		# Calculate the number of eggs produced each year, while
		# accounting for sex ratio (i.e., dividing by two)
		eggsa[i,j] = sum(rpois(round(Na[i,j]*Pbt[i,j],0),muFa))/2
		eggsa1[i,j] = sum(rpois(round(Na1[i,j]*Pbt[i,j],0),muFa1))/2
		round(eggsa[i,j],0)		
		round(eggsa1[i,j],0)

		# Calculate the number of hatchlings produced each year by accounting for
		# viability (Pva & Pva1) and nest survival (i.e., egg survival; Se) 
		if (j>1) Nh[i,j]= (eggsa[i,j-1]*Pvat[i,j-1]*Set[i,j-1])+(eggsa1[i,j-1]*Pva1t[i,j-1]*Set[i,j-1])
		round(Nh[i,j],0)

		# Implement observation error for hatchling "counts" / monitoring data
		#Noh[i,j]=sum(rbinom(Nh[i,j],1,Psamp[i,j]))

		# Calculate population growth rate
		if(j>5) rlam[i,j]=Na[i,j]/Na[i,j-1]
		if(j>5 && No[i,j-1]>0) olam[i,j]=No[i,j]/No[i,j-1]
		if (Na[i,j]<5) Pext[i,j]=1 else Pext[i,j]=0 
      }
    }
    
    MedNa = apply(Na,2,median)					# Median adult abundance
    MedNa1 = apply(Na1,2,median)				# Median primiparious
    MedNsa = apply(Nsa,2,median)				# Median subadults
    MedNj = apply(Nj,2,median)					# Median juveniles
    MedNh = apply(Nh,2,median)					# Median hatchlings
    
	MedNo = apply(No,2,median)				# Median number of adults observed
	#MedNoh = apply (Noh,2,median)				# Median hatchlings observed
	
	MedNadults = apply(Nsa+Na1+Na,2,median)		# Median subadults+primiparous adults + adults
	MedNhj = apply(Nj+Nh,2,median)				# Median hatchlings+juveniles

    lbNa = apply(Na, 2, quantile, probs = c(0.025)) 
    ubNa = apply(Na,2,quantile, probs = c(0.975))
    
    data.frame(rep = rep(1:r, t),
               year = rep(1:t, each = r),
               Na = c(Na),
               Na1 = c(Na1),
               Nsa = c(Nsa),
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
       select(rep, year, Na, Na1, Nsa, Nj, Nh) %>%
       gather(stage, N, 3:7) %>%
       group_by(year, stage) %>%
       summarize(uci = quantile(N, probs = 0.975)) %>%
       ungroup() %>%
       summarize(max = max(uci)) %>%
       c()
    

    dat() %>%
       select(rep, year, Na, Na1, Nsa, Nj, Nh) %>%
       gather(stage, N, 3:7) %>%
       mutate(stage = c("Na" = "Adults",
       					"Na1" = "Primiparous adults",
       					"Nsa" = "Subadults",
                        "Nj" = "Juveniles",
                        "Nh" = "Hatchlings")[stage],
              stage = fct_relevel(stage, c("Hatchlings", "Juveniles", "Subadults", "Primiparous adults", "Adults"))) %>%
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

