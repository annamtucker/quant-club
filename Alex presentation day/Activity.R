
# Lowndes Pig and Deer Activity Data --------------------------------------

library("overlap")
#load("Activity.RData")
save(Activity, file = "Activity.RData")
head(Activity)
range(Activity$Time)
timeRad<-Activity$Time*2*pi
spsNWA<-timeRad[Activity$Area=='NW'&Activity$Sps=='Pig']
spsNWB<-timeRad[Activity$Area=='NW'&Activity$Sps=='Deer']
overlapPlot(spsNWA,spsNWB,xcenter="midnight",main="NW pig and deer overlap")
legend('topleft',c("Pig","Deer"),lty=1,col=c('blue','black'),bg='white')

spsSEA<-timeRad[Activity$Area=='SE'&Activity$Sps=='Pig']
spsSEB<-timeRad[Activity$Area=='SE'&Activity$Sps=='Deer']
overlapPlot(spsSEA,spsSEB,xcenter="midnight",main="SE pig and deer overlap")
legend('topleft',c("Pig","Deer"),lty=1,col=c('blue','black'),bg='white')

pigNW<-timeRad[Activity$Area=="NW" & Activity$Sps=='Pig']
deerNW<-timeRad[Activity$Area=="NW" & Activity$Sps=='Deer']
densityPlot(pigNW,xcenter="midnight",rug=FALSE, main="NW activity",lwd=2,col='blue')
densityPlot(deerNW,xcenter="midnight",add=TRUE,rug=FALSE,lwd=2)
legend('topleft',c("Pig","Deer"),lty=1, col=c('blue','black'),bg='white')

pigSE<-timeRad[Activity$Area=="SE" & Activity$Sps=='Deer']
deerSE<-timeRad[Activity$Area=="SE" & Activity$Sps=='Pig']
densityPlot(deerSE,xcenter="midnight",rug=FALSE, main="SE activity",lwd=2)
densityPlot(pigSE,xcenter="midnight",add=TRUE,rug=FALSE,lwd=2,col='blue')
legend('topleft',c("Pig","Deer"),lty=1, col=c('blue','black'),bg='white')


# Example from overlap package --------------------------------------------

densityPlot(pigObs)
densityPlot(pigObs,extend=NULL,lwd=2)
densityPlot(pigObs, rug=TRUE, main="Simulated data", extend='gold')
densityPlot(tigerObs, add=TRUE, rug=TRUE, col='red')
legend('topleft', c("Tiger", "Pig"), lty=1, col=c('black', 'red'), bg='white')
densityPlot(pigObs,xcenter="m")
abline(v=c(5.5,(18+47/60)-24),lty=3)
densityPlot(pigObs,rug=TRUE,lwd=3)
pigDens<-densityPlot(pigObs, rug=TRUE)
lines(pigDens,lwd=3)
densityPlot(pigObs,xscale=NA,rug=TRUE)
densityPlot(tigerObs,xscale=NA,add=TRUE,rug=TRUE,col='red')

data(kerinci)
str(kerinci)
range(kerinci$Time)
timeRad<-kerinci$Time*2*pi
spsA<-timeRad[kerinci$Zone==3&kerinci$Sps=='tiger']
spsB<-timeRad[kerinci$Zone==3&kerinci$Sps=='tapir']
overlapPlot(spsA,spsB)
overlapPlot(spsA,spsB,xcenter="midnight")
length(spsA)
length(spsB)
(Dhats<-overlapEst(spsA,spsB))
(Dhats<-overlapEst(spsA,spsB,type="Dhat1"))
bsA<-resample(spsA,999)
bsB<-resample(spsB,999)
bs<-bootEst(bsA,bsB,type="Dhat1")
mean(bs)
bootCI(Dhats[1],bs)['norm0',]
bootCI(Dhats[1],bs)['basic0',]

