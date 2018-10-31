
#### Indigo Snake reintroduction model - 5/15/18
#### Conor's original script used to make the original Shiny App

### Clear the workspace
rm(list=ls())

#install.packages("statmod", dependencies=TRUE)
library(statmod)

### Model parameters

r = 1000	# number of replicates
t = 30		# number of years

Na = matrix(0,r,t) 	# Adult abundance
Nj = matrix(0,r,t) 	# Juvenile abundance
Nh =  matrix(0,r,t)	# Number of hatchlings 

Na[,1]=0	# Initial abundance of adults
Nj[,1]=0	# Initial abundance of juveniles
Nh[,1]=0	# Initial abundance of hatchlings

Nhr = matrix(0,r,t) # Number of hatchlings released
Njr = matrix(0,r,t) # Number of 4-year olds released
No = matrix(0,r,t) 	# Number of individuals observed/counted
Nmax=500 			# Maximum population size / population ceiling

nrelsa=30 			# Mean no. of juvenile releases
					# Releases are the total no. of releases divided by 2
					# to account for a female-only simulation model
					### Future iterations might use exact number of females
					
nrelj=0			# Mean number of hatchling releases 
				# Releases are total no. of releases divided by 2
				# to account for a female-only simulation
				
nyr=3 			# Duration of the release program (years)


### Adult survival with parametric uncertainty
mSa = 0.90  		
varSa = 0.1		# Variance of mean survival of adults
aSa = mSa*((mSa*(1-mSa)/(varSa^2))-1) 	 	
bSa = (1-mSa)*((mSa*(1-mSa)/(varSa^2))-1) 
Sai = matrix(rbeta(r,aSa,bSa),r,1)			# Parametric uncertainty for adult survival
SDmSai = matrix(rinvgauss(r,aSa^2,1),r,1)
ASai = matrix(0,r,1)				# beta distribution shape parameters
BSai = matrix(0,r,1)				# beta distribution shape parameters
Sat = matrix(0,r,t)					# annual variation survival


#### Juvenile survival with parametric uncertainty
mSj = 0.2675  							# mean survival, from Hyslop
SDmSj = 0.0535*0.0535 					# mean Adult survival SD, from Hyslop
aSj = mSj*((mSj*(1-mSj)/SDmSj)-1) 		
bSj = (1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
Sji = matrix(rbeta(r,aSj,bSj),r,1)		# parametric uncertainty Survival
SDmSji = matrix(rinvgauss(r,SDmSj,1),r,1)
ASji = matrix(0,r,1)					# beta distribution shape parameters
BSji = matrix(0,r,1)					# beta distribution shape parameters
Sjt = matrix(0,r,t)						# annual variation survival

mTja = 0.2675  #mean transition from Juvenile to adult, from Hyslop
SDmTja = 0.0535*0.0535 #mean Adult survival SD, from Hyslop
aTja=mSj*((mSj*(1-mSj)/SDmSj)-1)
bTja=(1-mSj)*((mSj*(1-mSj)/SDmSj)-1)
Tja = matrix(rbeta(r*t,aTja,bTja),r,t)

#Observation uncertainty parameters
mPsamp=0.2  
SDmPsamp = (mPsamp*0.1)*(mPsamp*0.1)
aPs= mPsamp*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
bPs=(1-mPsamp)*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
Psamp = matrix(rbeta(r*t,aPs,bPs),r,t)

#Productivity and recruitment parameters
meggs = 8.2 
eggs = matrix(0,r,t)
Pv = 0.85 #proportion viable eggs
aPv = 100*Pv
bPv = 100*(1-Pv)
Pvt=matrix(rbeta(r*t,aPv,bPv),r,t)
Se = 0.75 #Egg (nest) survival
SDSe = 0.15*0.15
aSe=Se*((Se*(1-Se)/SDSe)-1)
bSe=(1-Se)*((Se*(1-Se)/SDSe)-1)
Set=matrix(0,r,t)
Pbt=matrix(0,r,t) #proportion that breed
PGY = 0.8
GY=matrix(rbinom(r*t,1,PGY),r,t) #Good year / bad year dynamics
SDPb=0.2

#Hatchling survival
Sh = (.50)*1.2 
SDSh = 0.1*0.1
ash=Sh*((Sh*(1-Sh)/SDSh)-1)
bsh=(1-Sh)*((Sh*(1-Sh)/SDSh)-1)
Sht=matrix(0,r,t)

#Acclimation effect on Survival rate of captive released snakes is 50% of wild snake survival
Sr = 0.4 						# is this hard coded incorrectly?
Srt=matrix(0,r,t)

### Population growth & quasi-extinction estimates
rlam=matrix(0,r,t)		# True population growth
olam=matrix(,r,t)		# Observed population growth
Pext=matrix(0,r,t)		# Probability of quasi-extinction


for(i in 1:r){
  # Draws for replicate level means for adult and juvenile survival
ASai[i] = 100*Sai[i]		#Sai[i]*((Sai[i]*(1-Sai[i])/SDmSai[i])-1) 		
BSai[i] = 100*(1-Sai[i])	#(1-Sai[i])*((Sai[i]*(1-Sai[i])/SDmSai[i])-1)
ASji[i] = 100*Sji[i]		# Sji[i]*((Sji[i]*(1-Sji[i])/SDmSji[i])-1) 		
BSji[i] = 100*(1-Sji[i])	#(1-Sji[i])*((Sji[i]*(1-Sji[i])/SDmSji[i])-1)

for(j in 1:t){
#draws for annual demographic rates
Sat[i,j]=rbeta(1,ASai[i],BSai[i])
Sjt[i,j]=rbeta(1,ASji[i],BSji[i])
Srt[i,j]=rbeta(1,100*Sr, 100*(1-Sr)) #randomize the acclimation effect
Sht[i,j]=rbeta(1,ash,bsh)
Set[i,j]=rbeta(1,aSe,bSe)

#drawing the number of released hatchlings and juveniles
if (j>1 && j<nyr) Njr[i,j]=runif(1,0.8*nrelsa,1.2*nrelsa)
if (j>1 && j<nyr) Nhr[i,j]=runif(1,0.8*nrelj,1.2*nrelj)
#Projection equation for adults
if (j> 1) Na[i,j]=round(Na[i,j-1]*Sat[i,j-1]+Nj[i,j-1]*Tja[i,j-1]+(Njr[i,j-1]*Srt[i,j-1]*Sjt[i,j-1]),0) else Na[i,j]=0
round(Na[i,j],0)
#implement observation error for adult "counts" / monitoring data
No[i,j]=sum(rbinom(Na[i,j],1,Psamp[i,j]))
#projection equation for Juveniles
if (j> 1) Nj[i,j]= (Nj[i,j-1]*Sjt[i,j-1])+(Nh[i,j-1]*Sht[i,j-1])+(Nhr[i,j-1]*Srt[i,j-1]*Sht[i,j-1])
round(Nj[i,j],0)
#Good year / bad year function for probability of breeding
if (GY[i,j]==1) Pbt[i,j]=1 else Pbt[i,j]=runif(1,.7,.8)
#Population ceiling limits the propotion of females that breed to ~20%
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

}}

MedNa = apply(Na,2,median)
lbNa = apply(Na, 2, quantile, probs = c(0.025)) 
ubNa = apply(Na,2,quantile, probs = c(0.975))
MedNj = apply(Nj,2,median)
MedNh = apply(Nh,2,median)
MedNo = apply(No,2,median)
plot(ubNa, ylab="abundance", xlab="year")
lines(MedNa,lty=1, lwd=2,col="black")
lines(lbNa,lty=2, lwd=2,col="black")
lines(ubNa,lty=2, lwd=2,col="black")
#lines(MedNo,lty=3, lwd=3,col="blue")
title(main="15 juveniles released for 10 years")
legend("topleft",c("Na", "C.I. Na"), lty=c(1,2), col=c("black","black"))

apply(rlam,2,median,na.rm=TRUE)
median(rlam, na.rm=TRUE)
apply(olam,2,median,na.rm=TRUE)
median(olam,na.rm=TRUE)
PE=(apply(Pext,2,sum))/r
PE[t]
mean(apply(Njr,1,sum))
mean(apply(Nhr,1,sum))
MedNo










########### Indigo snake reintroduction model
########### 10-20-2018 

# this model has been advanced from the above previous model with a few key changes:
# 1) five stages with demographic parameters more closely estimated from the literature
#		- allows for more realistic reintroduction scenarios from Conecuh NF
# 2) matrix model built to estimate sensitivities/elasticities for population model
#		- not a part of the reintroduction model, in a strict sense, but useful 
#		  for understanding how demography contributes to population growth
# 3) sex ratios  modeled with parametric uncertainty
# 4) sex ratio to remove males during reintroductions added into model
# 5) ???



### Clear the workspace
rm(list=ls())

library(statmod)



############## Part 1)
############## Modeling demographic variables for Eastern Indigo Snakes
############## in Conecuh National Forest

# Let's model female populations of Eastern Indigo Snakes (Drymarchon couperi) 
# as a population with five distinct life stages:
# 1) hatchlings/first-year snakes (ages 0-1)
# 2) juveniles (1-2 yr) 
# 3) subadults (2-3 yr)
# 4) first-year adults (3-4 yr)
# 5) adults (4<= yr)

# The survival within each life stage and transition between stages 
# can be conceptualized with a population transition matrix:
# 
#		| 0			0				0				Sa1*Fa1*Ba1		Sa*Fa*Ba	|
#		| Sh*Thj	Sj(1-Tjsa)		0					0				0		|
# A =	| 0			Sj*Tjsa		Ssa(1-Tsaa1)			0	    		0		|, where:
#		| 0			0				Ssa*Tsaa1		Sa1(1-Ta1a)			0		|
#		| 0 		0				0				Sa1*Ta1a			Sa		|
#	
# S is the survival rate at stage i,
	# where i can be h=hatchling, j=juvenile, sa=subadult, a1=primiparous adult, a=adult
# F is the fecundity at stages a1 and a, and 
# B is the likelihood of breeding at stages a1 and a.

### To simulate these parameters, we will use 1000 replications (r) 
### that are projected over 30 years (t)
r = 1000
t = 30

#### Sh -- survival of hatchlings
## This stage is comparable to the hatchling/juvenile stage from Hyslop et al. 2012, where 
## they estimated hatchling survival as 0.49 (three months) and juvenile as 0.59 (9 months)
## within the first year. We modeled this as a conservative estimate of the two; 0.52
mSh = 0.52  		
varSh = 0.1		# Variance of mean
aSh = mSh*((mSh*(1-mSh)/(varSh^2))-1) 	 	
bSh = (1-mSh)*((mSh*(1-mSh)/(varSh^2))-1) 
Shi = matrix(rbeta(r,aSh,bSh),r,1)			# Parametric uncertainty 
SDmShi = matrix(rinvgauss(r,varSh^2,1),r,1)
AShi = matrix(0,r,1)				# beta distribution shape parameters
BShi = matrix(0,r,1)				# beta distribution shape parameters
Sht = matrix(0,r,t)					# annual variation survival


#### Sj -- survival of juveniles 
## This stage is comparable to the subadult stage from Hyslop et al. 2012, where they estimated
## survival as 0.52 (0.20 SE), so we modeled this similarly here
mSj = 0.52  		
varSj = 0.1		# Variance of mean
aSj = mSj*((mSj*(1-mSj)/(varSj^2))-1) 	 	
bSj = (1-mSj)*((mSj*(1-mSj)/(varSj^2))-1) 
Sji = matrix(rbeta(r,aSj,bSj),r,1)			# Parametric uncertainty 
SDmSji = matrix(rinvgauss(r,varSj^2,1),r,1)
ASji = matrix(0,r,1)				# beta distribution shape parameters
BSji = matrix(0,r,1)				# beta distribution shape parameters
Sjt = matrix(0,r,t)					# annual variation survival

#### Ssa -- survival of subadults  
## This stage roughly translates to the first-year adults from 
## Hyslop et al. 2012's adult survival stage (0.74), so we estimated this as 0.70 as a 
## slightly reduced estimate of that
mSsa = 0.70  		
varSsa = 0.1		# Variance of mean 
aSsa = mSsa*((mSsa*(1-mSsa)/(varSsa^2))-1) 	 	
bSsa = (1-mSsa)*((mSsa*(1-mSsa)/(varSsa^2))-1) 
Ssai = matrix(rbeta(r,aSsa,bSsa),r,1)			# Parametric uncertainty 
SDmSsai = matrix(rinvgauss(r,varSsa^2,1),r,1)
ASsai = matrix(0,r,1)				# beta distribution shape parameters
BSsai = matrix(0,r,1)				# beta distribution shape parameters
Ssat = matrix(0,r,t)					# annual variation survival

#### Sa1 -- survival of first-year adults 
## Hyslop et al. 2012 suggested that females transition to 3-4 years at sizes >120 cm 
## The survival x size graph in that manuscript indicates ca. 0.82 survival at that size
mSa1 = 0.80  		
varSa1 = 0.1		# Variance of mean 
aSa1 = mSa1*((mSa1*(1-mSa1)/(varSa1^2))-1) 	 	
bSa1 = (1-mSa1)*((mSa1*(1-mSa1)/(varSa1^2))-1) 
Sa1i = matrix(rbeta(r,aSa1,bSa1),r,1)			# Parametric uncertainty 
SDmSa1i = matrix(rinvgauss(r,varSa1^2,1),r,1)
ASa1i = matrix(0,r,1)				# beta distribution shape parameters
BSa1i = matrix(0,r,1)				# beta distribution shape parameters
Sa1t = matrix(0,r,t)					# annual variation survival

#### Sa -- survival of adults 
## Radiotelemetered individuals in CNF that survive one year proceed to have high 
## survival (0.90) (Stiles 2013). Similarly, large individuals (>150 cm) in south Georgia 
## have high survival (0.85-0.95) (Hyslop et al. 2012)
mSa = 0.85  		
varSa = 0.1		# Variance of mean
aSa = mSa*((mSa*(1-mSa)/(varSa^2))-1) 	 	
bSa = (1-mSa)*((mSa*(1-mSa)/(varSa^2))-1) 
Sai = matrix(rbeta(r,aSa,bSa),r,1)			# Parametric uncertainty 
SDmSai = matrix(rinvgauss(r,varSa^2,1),r,1)
ASai = matrix(0,r,1)				# beta distribution shape parameters
BSai = matrix(0,r,1)				# beta distribution shape parameters
Sat = matrix(0,r,t)					# annual variation survival

### The first four stages should theoretically transition into the next age stage 
### after one year. However, there may be uncertainty in this, and some individuals may
### remain in stages after years, especially as they get older and things are less predictable.
### So, to model this uncertainty, model transition probability declining from 0.99 with each 
### successive stage to primiparous adults, until when they transition straight into adults.
mThj = 0.99  			# Mean transition from hatchling to juvenile
varThj = 0.01		
aThj = mSh*((mSh*(1-mSh)/varSh^2)-1)
bThj = (1-mSh)*((mSh*(1-mSh)/varSh^2)-1)
Thj = matrix(rbeta(r*t,aThj,bThj),r,t)

mTjsa = 0.90 			# Mean transition from juvenile to subadult
varTjsa = 0.03	
aTjsa = mSj*((mSj*(1-mSj)/varSj^2)-1)
bTjsa = (1-mSj)*((mSj*(1-mSj)/varSj^2)-1)
Tjsa = matrix(rbeta(r*t,aTjsa,bTjsa),r,t)

mTsaa1 = 0.80  			# Mean transition from subadult to primiparous adult
varTsaa1 = 0.05	
aTsaa1 = mSsa*((mSsa*(1-mSsa)/varSsa^2)-1)
bTsaa1 = (1-mSsa)*((mSsa*(1-mSsa)/varSsa^2)-1)
Tsaa1 = matrix(rbeta(r*t,aTsaa1,bTsaa1),r,t)

mTa1a = 0.96  			# Mean transition from primiparous adult to adult
varTa1a = 0.01	
aTa1a = mSa1*((mSa1*(1-mSa1)/varSa1^2)-1)
bTa1a = (1-mSa1)*((mSa1*(1-mSa1)/varSa1^2)-1)
Ta1a = matrix(rbeta(r*t,aTa1a,bTa1a),r,t)

### Fa -- fecundity of adults (clutch size)
### mu = 9 (4 - 12, range; Hyslop et al. 2012); mu = 8.65 (6 - 12, range; Auburn data from CGuyer)
muFa = 8.65								## meggs = 9
sdFa = 2								## eggs = matrix(0,r,t)
fecundShape2Fa = log((sdFa^2)/(muFa^2)+1)
fecundShape1Fa = log(muFa)-1/2*fecundShape2Fa
# hist(round(rlnorm(100,fecundShape1Fa,fecundShape2Fa)))	## for visualization purposes
round(rlnorm(1,fecundShape1Fa,fecundShape2Fa))  # Clutch size for matrix
eggsa = matrix(0,r,t)				# eggs from adults

### Pva -- proportion of viable eggs for adults
Pva = 0.85 
aPva = 100*Pva
bPva = 100*(1-Pva)
Pvat=matrix(rbeta(r*t,aPva,bPva),r,t)

### Fa1 -- fecundity (clutch size) of primiparous adults
### A primary reason why we modeled two stages of adults is because, in our experience,
### first-year breeding females lay clutches dominated by inviable eggs. So, we wanted to 
### model these females as having different productivity parameters, relative to 
### older, more experienced individuals 
muFa1 = 8.5								
sdFa1 = 2								
fecundShape2Fa1 = log((sdFa1^2)/(muFa1^2)+1)
fecundShape1Fa1 = log(muFa1)-1/2*fecundShape2Fa1
# hist(round(rlnorm(100,fecundShape1Fa1,fecundShape2Fa1)))	## for visualization purposes
round(rlnorm(1,fecundShape1Fa1,fecundShape2Fa1))  # Clutch size for matrix
eggsa1 = matrix(0,r,t)				# eggs from primiparous adults (a1)

### Pva1 -- proportion of viable eggs for primiparous adults
Pva1 = 0.35 
aPva1 = 100*Pva1
bPva1 = 100*(1-Pva1)
Pva1t=matrix(rbeta(r*t,aPva1,bPva1),r,t)

### Se -- survival of eggs (nests)
Se = 0.75 
varSe = 0.12
aSe=Se*((Se*(1-Se)/varSe^2)-1)
bSe=(1-Se)*((Se*(1-Se)/varSe^2)-1)
Set=matrix(0,r,t)

### SR -- Sex ratio of eggs, assuming 1:1 sex ratio of clutch, to only model females
### a beta-distributed var 0.5 (+/-0.04)
mSR = 0.5
varSR = 0.04
aSR=mSR*((mSR*(1-mSR)/(varSR^2))-1)
bSR=(1-mSR)*((mSR*(1-mSR)/(varSR^2))-1)
SRi = matrix(rbeta(r,aSR,bSR),r,1)			 
SDmSRi = matrix(rinvgauss(r,aSR^2,1),r,1)
ASRi = matrix(0,r,1)				
BSRi = matrix(0,r,1)				
SRt = matrix(0,r,t)				

### Pbt -- proportion of individuals that breed
## Good-year/bad-year dynamics
Pbt = matrix(0,r,t) 
PGY = 0.8
GY = matrix(rbinom(r*t,1,PGY),r,t) 
SDPb=0.2

# Proportion of individuals that breed during any given year
PB = 0.8
varPB = 0.1
aPB = PB*((PB*(1-PB)/varPB^2)-1)
bPB = (1-PB)*((PB*(1-PB)/varPB^2)-1)
rbeta(1,aPB,bPB)


###### Together, these variables create the
###### population transition matrix

stages <- c("Hatchlings","Juveniles","Subadults","Primiparous adults","Adults")
(femaleMatrix = matrix(c(0,0,0,
rbeta(1,aSa1,bSa1)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa1,fecundShape2Fa1))*
rbeta(1,aSe,bSe)*rbeta(1,aPva1,bPva1)*rbeta(1,aSR,bSR), # row 1, col 4: survival, prop breed, 	fecundity, nest survival, egg viability, sex ratio for prim. adults
rbeta(1,aSa,bSa)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa,fecundShape2Fa))*rbeta(1,aSe,bSe)*rbeta(1,aPva,bPva)*rbeta(1,aSR,bSR), # row 1, col 5: survival, proportion breeders, fecundity, nest survival, egg viability, sex ratio for adults
rbeta(1,aSh,bSh),rbeta(1,aSj,bSj)*(1-rbeta(1,aTjsa,bTjsa)),0,0,0,							# row 2
0,rbeta(1,aSj,bSj)*rbeta(1,aSsa,bSsa),rbeta(1,aSsa,bSsa)*(1-rbeta(1,aTsaa1,bTsaa1)),0,0,	# row 3
0,0,rbeta(1,aSsa,bSsa)*rbeta(1,aSa1,bSa1),rbeta(1,aSa1,bSa1)*(1-rbeta(1,aTa1a,bTa1a)),0,	# row 4
0,0,0,rbeta(1,aSa1,bSa1)*rbeta(1,aTa1a,bTa1a),rbeta(1,aSa,bSa)),							# row 5
nrow=5, byrow=TRUE, dimnames=list(stages,stages)))



############## Part 2)
############## Using matrix simulations to understand population growth, 
############## sensitivities, elasticities, and extinction risk in general

#install.packages("popbio")
library(popbio) # Stubben, C.J. and Milligan, B.G.  2007.  Estimating and Analyzing Demographic Models Using the popbio Package in R. Journal of Statistical Software 22:11.

# Use the 'eigen.analysis()' function to estimate all demographic parameters
eigen.analysis(femaleMatrix)

####### Use a for-loop(s) to run comprehensive analysis which computes lambda, stable stage,
####### and mean matrix components using the 'eigen.analysis()' function

r = 1000					#r (replicate) - a 100 replicate simulation

### Lambda
lamb=matrix(0,r)
for (i in 1:r){
	lamb[i,] = eigen.analysis(matrix(c(0,0,0,
rbeta(1,aSa1,bSa1)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa1,fecundShape2Fa1))*
rbeta(1,aSe,bSe)*rbeta(1,aPva1,bPva1)*rbeta(1,aSR,bSR),rbeta(1,aSa,bSa)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa,fecundShape2Fa))*rbeta(1,aSe,bSe)*rbeta(1,aPva,bPva)*rbeta(1,aSR,bSR),rbeta(1,aSh,bSh),rbeta(1,aSj,bSj)*(1-rbeta(1,aTjsa,bTjsa)),0,0,0,0,rbeta(1,aSj,bSj)*rbeta(1,aSsa,bSsa),rbeta(1,aSsa,bSsa)*(1-rbeta(1,aTsaa1,bTsaa1)),0,0,
0,0,rbeta(1,aSsa,bSsa)*rbeta(1,aSa1,bSa1),rbeta(1,aSa1,bSa1)*(1-rbeta(1,aTa1a,bTa1a)),0,
0,0,0,rbeta(1,aSa1,bSa1)*rbeta(1,aTa1a,bTa1a),rbeta(1,aSa,bSa)),
nrow=5, byrow=TRUE, dimnames=list(stages,stages)))$lambda1
}
(t.test(lamb))		#Mean, CI for lambda of population


###### Run population projections to determine population viability 
###### of the Conecuh population, given a simple reintroduction scheme

t = 30				#t (time) - a 20 year simulation
r = 1000			#r (replicate) - a 100 replicate simulation

### Population projection and PVA model for 
### the repatriation of Easter Indigo Snakes in Conecuh National Forest, AL

niFemale = matrix(c(0,0,50,0,0),5,1)	
	# 100 subadult individuals have been released
	# let's assume half are female
hatchFemale = matrix(0,r,t)			#matrix for simulated N of hatchling females
juvFemale = matrix(0,r,t)			#juv females
subadultFemale = matrix(0,r,t)		#subadult females
primadultFemale = matrix(0,r,t)		#primiparous females
adultFemale = matrix(0,r,t)			#adult females

stages <- c("Hatchlings","Juveniles","Subadults","Primiparous adults","Adults")


for(i in 1:r){

ntFemale = matrix(0,5,t)	#empty matrix where we will project population
ntFemale[1:5,1]=niFemale	#insert values of the initial state vector (niFemales)

for(j in 2:t){

(femaleMatrix =matrix(c(0,0,0,
rbeta(1,aSa1,bSa1)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa1,fecundShape2Fa1))*
rbeta(1,aSe,bSe)*rbeta(1,aPva1,bPva1)*rbeta(1,aSR,bSR),rbeta(1,aSa,bSa)*rbeta(1,aPB,bPB)*round(rlnorm(1,fecundShape1Fa,fecundShape2Fa))*rbeta(1,aSe,bSe)*rbeta(1,aPva,bPva)*rbeta(1,aSR,bSR),rbeta(1,aSh,bSh),rbeta(1,aSj,bSj)*(1-rbeta(1,aTjsa,bTjsa)),0,0,0,0,rbeta(1,aSj,bSj)*rbeta(1,aSsa,bSsa),rbeta(1,aSsa,bSsa)*(1-rbeta(1,aTsaa1,bTsaa1)),0,0,
0,0,rbeta(1,aSsa,bSsa)*rbeta(1,aSa1,bSa1),rbeta(1,aSa1,bSa1)*(1-rbeta(1,aTa1a,bTa1a)),0,
0,0,0,rbeta(1,aSa1,bSa1)*rbeta(1,aTa1a,bTa1a),rbeta(1,aSa,bSa)),
nrow=5, byrow=TRUE, dimnames=list(stages,stages)))

if (j>1) ntFemale[,j] = femaleMatrix%*%ntFemale[,j-1]  
		#for each increment in the for loop
		#the corresponding column of ntMale equals the matrix product
		#of the previous step state vector (prev. column) times maleMatrix
		#the transition matrix for males
}

hatchFemale[i,]=ntFemale[1,1:t]
juvFemale[i,]=ntFemale[2,1:t]
subadultFemale[i,]=ntFemale[3,1:t]
primadultFemale[i,]=ntFemale[4,1:t]
adultFemale[i,]=ntFemale[5,1:t]

}

HatchlingFemales=apply(hatchFemale,2,median)	#median number of hatchling females
JuvenileFemales=apply(juvFemale,2,median)		#juvenile females
SubadultFemales=apply(subadultFemale,2,median)		#subadult females
PrimadultFemales=apply(primadultFemale,2,median)	#primiparous females
AdultFemales=apply(adultFemale,2,median)		#adult females

#Median abundance of hatchlings, juveniles, adult females over 30 yr projection
(FemalePopulation=rbind(HatchlingFemales,JuvenileFemales,SubadultFemales,PrimadultFemales,AdultFemales))
t.test(hatchFemale[,c(t)])		#Mean +/- 95% CI
t.test(juvFemale[,c(t)])		#Mean +/- 95% CI
t.test(subadultFemale[,c(t)])	#Mean +/- 95% CI
t.test(primadultFemale[,c(t)])	#Mean +/- 95% CI
t.test(adultFemale[,c(t)])		#Mean +/- 95% CI


#PVA - in what percent of simulations did the female population decline?
(PVA = matrix(c((length(subset(adultFemale[,c(t)], adultFemale[,c(t)] < (0.5*niFemale[3])))/length(adultFemale[,c(t)])),
	(length(subset(adultFemale[,c(t)], adultFemale[,c(t)] > (0.5*niFemale[3]) & adultFemale[,c(t)] < (niFemale[3])))/length(adultFemale[,c(t)])),
	(length(subset(adultFemale[,c(t)], adultFemale[,c(t)] > niFemale[3] & adultFemale[,c(t)] < (1.5*niFemale[3])))/length(adultFemale[,c(t)])),
	(length(subset(adultFemale[,c(t)], adultFemale[,c(t)] > (1.5*niFemale[3])))/length(adultFemale[,c(t)]))),
	dimnames=list(c("<50% decline","50-99% decline","1-50% increase",">50% increase"),"Value")))




############## Part 3)
############## Expanding Conor's models to include five stages and 
############## more reintroduction scenarios

t = 30
r = 1000

### Sr -- survival reduction for captives released into wild
### Individuals being released into the wild may not perform as well as wild-born individuals
### So, we modeled an acclimation effect on the survival of captive-bred snakes being released. 
Sr = 0.5 						
Srt=matrix(0,r,t)
	
### pSamp -- the probability of sampling snakes
### Snakes are difficult to detection, especially species that are largely fossorial
### and/or have large homeranges. This parameter models how detection probability influences
### our ability to monitor population growth 
mPsamp=0.20 
SDmPsamp = (mPsamp*0.1)*(mPsamp*0.1)
aPs= mPsamp*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
bPs=(1-mPsamp)*((mPsamp*(1-mPsamp)/SDmPsamp)-1)
Psamp = matrix(rbeta(r*t,aPs,bPs),r,t)

### Population growth & quasi-extinction estimates
rlam=matrix(0,r,t)		# Real (true) population growth
olam=matrix(,r,t)		# Observed population growth, after sampling (detection) process
Pext=matrix(0,r,t)		# Probability of quasi-extinction

#### Preliminary modeling exercises created a reintroduction strategy where
#### 30 'head-started', 2 yr-old subadult snakes would be released/year for 10 years. 
#### This sought to create a sustainable population with relatively low risk of extinction.
#### However, it is expensive to raise snakes through their second year, and difficult to 
#### raise enough snakes to release them each year for 10 years. So, managers were also interested in
#### in the feasability of releasing younger snakes, such as hatchlings or 1-yr old juveniles, and 
#### whether shorter release periods (e.g., 5 years) or irregular release periods (7 out of 10 years) 
#### would be sufficient to generate viable populations with low risk of extinction.  

#### This provides us three factors apply to our modeling exercise: 
#### (1) annual release size (15 or 30 individuals)
#### (2) snake release age (juvenile or subadult)
#### (3) release duration (5 yr, 10 yr)

#### 2*2*2*2 = 16 release scenarios

cols = c("nrelsa","nrelj","nyr","probRel")
rows = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")

scenarios = matrix(c(30,0,10,1,		#scenario A - 30 subadults, 10 yr, prob. release = 1.0 for each year
					0,30,10,1,		#scenario B - 30 juveniles, 10 yr, p = 1
					30,0,5,1,		#scenario C - 30 subadults, 5 yr, p = 1
					0,30,5,1,		#scenario D - 30 juveniles, 5 yr, p = 1
					15,0,10,1,		#scenario E - 15 subadults, 10 yr, p = 1
					0,15,10,1,		#scenario F - 15 juveniles, 10 yr, p = 1
					15,0,5,1,		#scenario G - 15 subadults, 5 yr, p = 1
					0,15,5,1,		#scenario H - 15 juveniles, 5 yr, p = 1
					30,0,10,0.67,	#scenario I - 30 subadults, 10 yr, prob. release = 0.67 for each year
					0,30,10,0.67,	#scenario J - 30 juveniles, 10 yr, p = 0.67
					30,0,5,0.67,	#scenario K - 30 subadults, 5 yr, p = 0.67
					0,30,5,0.67,	#scenario L - 30 juveniles, 5 yr, p = 0.67
					15,0,10,0.67,	#scenario M - 15 subadults, 10 yr, p = 0.67
					0,15,10,0.67,	#scenario N - 15 juveniles, 10 yr, p = 0.67
					15,0,5,0.67,	#scenario O - 15 subadults, 5 yr, p = 0.67
					0,15,5,0.67),	#scenario P - 15 juveniles, 5 yr, p = 0.67
					nrow=16, ncol=4, byrow=TRUE, dimnames=list(rows,cols))
					
n = length(scenarios[,1])

### Use a for-loop to iteratively calculate demography under different management scenarios

for (h in 1:n){
		
nrelsa=scenarios[h,1] 	# Mean no. of 2-yr old subadults releases (both sexes)
nrelj=scenarios[h,2]	# Mean number of 1 yr-old juveniles releases (both sexes)
nyr=scenarios[h,3] 		# Duration of the release program (years)
prel=scenarios[h,4]		# Probability of releasing snakes in a given year

# prel = matrix(0,r,t) 					# Empty matrix to simulate whether or not a release happens,
rel = matrix(rbinom(r*t,1,prel),r,t) 	# given the probability of a release (prel)

### Define some matrices and parameters for the model loop:
Nh =  matrix(0,r,t)		# Abundance of hatchlings 
Nj = matrix(0,r,t) 		# Juveniles
Nsa = matrix(0,r,t) 	# Subadults
Na1 = matrix(0,r,t) 	# Primiparous adults
Na = matrix(0,r,t) 		# Adults

Nh[,1]=0	# Initial abundance of hatchlings
Nj[,1]=0	# Juveniles
Nsa[,1]=0	# Subadults
Na1[,1]=0	# Primiparous adults
Na[,1]=0	# Adults

Njr = matrix(0,r,t) 	# Number of captive-reared juveniles (1-yr olds) released
Nsar = matrix(0,r,t) 	# Number of captive-reared subadults (2-yr olds) released
Na1r = matrix(0,r,t)	# Number of captive-reared primiparous adults (3-yr olds) released

Noa = matrix(0,r,t) 	# Number of adults observed/counted, given Psamp
Noh = matrix(0,r,t)		# Number of hatchlings observed/counted, given Psamp

Nmax=500 			# Population ceiling for density dependence

###### NEED TO MAKE SURE THIS AFFECTS BOTH PRIMIPAROUS & EXPERIENCED ADULTS

#### Population-projection model that accounts for imperfect detection

for(i in 1:r){			# Draws for replicate-level means for survival at each stage

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

for(j in 1:t){						# Drawing annual demographic rates
	
Sat[i,j]=rbeta(1,ASai[i],BSai[i])			# Adults
Sa1t[i,j]=rbeta(1,ASa1i[i],BSa1i[i])		# Primiparous adults
Ssat[i,j]=rbeta(1,ASsai[i],BSsai[i])		# Subadults
Sjt[i,j]=rbeta(1,ASji[i],BSji[i])			# Juveniles
Sht[i,j]=rbeta(1,aSh,bSh)					# Hatchlings
Set[i,j]=rbeta(1,aSe,bSe)					# Egg survival
Srt[i,j]=rbeta(1,100*Sr, 100*(1-Sr)) 		# Randomize the acclimation effect

# Drawing the number of released subadults and juveniles, and
# account for sex ratio to remove males
if (j>0 && j<nyr+1 && prel == 1) Nsar[i,j]=runif(1,0.8*nrelsa,1.2*nrelsa)*rbeta(1,aSR,bSR)
if (j>0 && j<nyr+1 && prel == 1) Njr[i,j]=runif(1,0.8*nrelj,1.2*nrelj)*rbeta(1,aSR,bSR)
if (j>0 && j<nyr+1 && prel < 1) Nsar[i,j]=runif(1,0.8*nrelsa,1.2*nrelsa)*rbeta(1,aSR,bSR)*rel[i,j]
if (j>0 && j<nyr+1 && prel < 1) Njr[i,j]=runif(1,0.8*nrelj,1.2*nrelj)*rbeta(1,aSR,bSR)*rel[i,j]

# Projection equation for adults
if (j>1) Na[i,j]=(round(Na[i,j-1]*Sat[i,j-1]+Na1[i,j-1]*Ta1a[i,j-1])+
(Na1r[i,j-1]*Sa1t[i,j-1]*Ta1a[i,j-1]*Srt[i,j-1])) else Na[i,j]=0
round(Na[i,j],0)

# Implement observation error for adult "counts" / monitoring data
Noa[i,j]=sum(rbinom(Na[i,j],1,Psamp[i,j]))

# Projection equation for primiparous adults
if (j>1) Na1[i,j]= (Na1[i,j-1]*(Sa1t[i,j-1]*(1-Ta1a[i,j-1])))+(Nsa[i,j-1]*Tsaa1[i,j-1])+	(Nsar[i,j-1]*Ssat[i,j-1]*Tsaa1[i,j-1]*Srt[i,j-1])
round(Na1[i,j],0)

# Projection equation for subadults
if (j>1) Nsa[i,j]= (Nsa[i,j-1]*(Ssat[i,j-1]*(1-Tsaa1[i,j-1])))+(Nj[i,j-1]*Tjsa[i,j-1])+(Njr[i,j-1]*Sjt[i,j-1]*Tjsa[i,j-1]*Srt[i,j-1])
round(Nsa[i,j],0)

# Projection equation for juveniles
if (j>1) Nj[i,j]= (Nj[i,j-1]*Sjt[i,j-1])+(Nh[i,j-1]*Thj[i,j-1])
round(Nj[i,j],0)

# Good-year/bad-year function for probability of breeding
if (GY[i,j]==1) Pbt[i,j]=1 else Pbt[i,j]=runif(1,.7,.8)

# Population ceiling to limit the proportion of females that breed
# to to ~20% when abundance > nmax population size OR to 0% when abundance > 2*nmax
if (Na[i,j]>Nmax) Pbt[i,j]=runif(1,.15,.25)
if (Na[i,j]>Nmax*2) Pbt[i,j]=0

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
Noh[i,j]=sum(rbinom(Nh[i,j],1,Psamp[i,j]))

# Calculate population growth rate
if(j>5) rlam[i,j]=Na[i,j]/Na[i,j-1]
if(j>5 && Noa[i,j-1]>0) olam[i,j]=Noa[i,j]/Noa[i,j-1]
if (Na[i,j]<5) Pext[i,j]=1 else Pext[i,j]=0 

}}

MedNa = apply(Na,2,median)					# Median adult abundance
MedNadults = apply(Nsa+Na1+Na,2,median)		# Median subadults+primiparous adults + adults
MedNhj = apply(Nj+Nh,2,median)				# Median hatchlings+juveniles
MedNoa = apply(Noa,2,median)					# Median number of adults observed
MedNoh = apply (Noh,2,median)

assign(paste0("MedNa", rownames(scenarios)[h]), MedNa) 
assign(paste0("MedNadults", rownames(scenarios)[h]), MedNadults) 
assign(paste0("MedNhj", rownames(scenarios)[h]), MedNhj) 
assign(paste0("MedNoa", rownames(scenarios)[h]), MedNoa) 
assign(paste0("MedNoh", rownames(scenarios)[h]), MedNoh) 

medrlam = apply(rlam, 2, median, na.rm=TRUE)
medolam = apply(olam, 2, median, na.rm=TRUE)
PE = (apply(Pext,2,sum))/r
PEt = PE[t]
muNsar = mean(apply(Nsar,1,sum))
muNjr = mean(apply(Njr,1,sum))

assign(paste0("medrlam", rownames(scenarios)[h]), medrlam) 
assign(paste0("medolam", rownames(scenarios)[h]), medolam) 
assign(paste0("PE", rownames(scenarios)[h]), PE) 
assign(paste0("PEt", rownames(scenarios)[h]), PEt) 
assign(paste0("muNsar", rownames(scenarios)[h]), muNsar) 
assign(paste0("muNjr", rownames(scenarios)[h]), muNjr)

} 


MedNaA = apply(Na,2,median)
lbNaA = apply(Na, 2, quantile, probs = c(0.025)) 	# Lower bound
ubNaA = apply(Na, 2, quantile, probs = c(0.975))	# Upper bound

plot(ubNaA, ylab="Adult abundance", xlab="Year", ylim=c(0,2500), pch=2, col="white")
lines(MedNaA,lty=1, lwd=2,col="black")
lines(lbNaA,lty=2, lwd=2,col="black")
lines(ubNaA,lty=2, lwd=2,col="black")
#lines(MedNo,lty=3, lwd=3,col="blue")
#title(main="15 juveniles released for 10 years")
legend("topleft",inset=0.05, c("No. of adults", "95% CI"), lty=c(1,2), col=c("black","black"))




