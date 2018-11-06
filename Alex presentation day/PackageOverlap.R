install.packages("overlap")
library("overlap")

data(kerinci)
head(kerinci)

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
