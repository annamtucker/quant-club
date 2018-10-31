
#### Angeli modified 22 June 2018 quant club

###honey<-load("/Users/nicoleangeli/Documents/GitHub/quant-club/honey.RData")

#QA/QC -----
library(dplyr) 
Honey<-dat
head(Honey)
str(Honey)
names(Honey)
summary(Honey)
plot(Honey[,2:7]) #explore data

### group and return some averages ----

Honey_groups <- Honey %>%
  group_by(state) %>%
  summarise(totalprod = mean(totalprod), priceperlb = mean(priceperlb))

Honey_groups

Honey_groups <-count(honey, state) %>%
  arrange(desc(n))

#names(Honey_groups)<-c('region','totalprod','priceperlb')


# Create data frame
data=data.frame(x=Honey_groups$state, y=log(Honey_groups$totalprod))

# 1 - Custom markers (left)
# note: shape = integer between 0 and 25
# note: stroke exists only for shapes between 1 and 24
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

# 2 - Custom stems (right)
# note: size is the width in mm
# note: style can be in: "blank", "solid", "dashed", "dotted", "dotdash", "longdash","twodash"
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y) , size=1, color="blue", linetype="dotted" ) +
  geom_point()

str(data)
data %>%
  arrange(y) %>%
  mutate(x=factor(x,x)) %>%
  ggplot( aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="orange", size=1) +
  geom_point( color="orange", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("log(Total production)")



##############################
# MAPS ----
##############################
library(maps)


# Load the county data from the maps package
state_map <- map_data("state")
plot(state_map)
str(state_map)

##Put everything into abreviations
#state.abb[grep("New York", state.name)]
names(Honey_groups)<-c('region','totalprod','priceperlb')


Names_states<-state.name[match(Honey_groups$region, state.abb)] #make abbreviations into states for congruence for mapping

nicole_cheats<-function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
          fixed = FALSE, useBytes = FALSE, invert = FALSE) 
{
  if (!is.character(x)) 
    x <- structure(as.character(x), names = names(x))
  .Internal(grep(as.character(pattern), x, ignore.case, value, 
                 perl, fixed, useBytes, invert))
}
nicole_cheats()
states<-cbind(state_map,Names_states) #merge data

honey_plotting<-as.data.frame(cbind(Names_states, Honey_groups$totalprod, Honey_groups$priceperlb))
names(honey_plotting)<-c('state','production','priceperlb')
## factor finding
head(honey_plotting)
str(honey_plotting)
summary(honey_plotting)
plot(honey_plotting)

range(honeyproduction$year)

#### Map by state
library(ggplot2)
library(maps)
#load us map data
all_states <- map_data("state")

### format data to create map merge 
names(honey_plotting)<-c('region','totalprod','priceperlb')

state_honey <- left_join(all_states, honey_plotting, "region")
#state_honey<-as.integer(state_honey$totalprod)

names(Honey_groups)<-c('region','totalprod','priceperlb')
state_honey <- left_join(state_honey, Honey_groups$totalprod, "region")


#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=state_honey, aes(x=long, y=lat, group = group), colour="white", fill="grey10" )

#create your labels
plab<-p+ labs(x = NULL, 
       y = NULL, 
       title = "America's Honey", 
       subtitle = "Total honey production, 1998 - 2012", 
       caption = "Angeli, .../GitHub/quant-club")

#read in data for state centroids
center<-read.csv("state_centroid.csv")
names(center)<-c('region','lat','long') #name columns

#select data you want as numeric
honey_region<-cbind(Names_states,Honey_groups[2:3])
names(honey_region)<-c('region','totalprod','priceperlab') #name it
str(honey_region)  
#create dataset
honey_points<-merge(center, honey_region, by.x = "region", all=TRUE)  ## Region, total prod
str(honey_points)

### Plot by points on states
pHoney_Hawaii<- plab + 
 geom_point(aes(long,lat, size =log(totalprod)), data = honey_points, col="yellow")

honey_pointsNoHA<-honey_points[-c(2,12),] #remove Hawaii and Alaska

pHoney_NoHawaii<- plab + 
  geom_point(aes(long,lat, size = totalprod), data = honey_pointsNoHA, col="yellow") #not log scale

### Subset honey production between 

Honey_AR<-subset(honey_pointsNoHA, region== "Arkansas")
Honey_AR2=honey_pointsNoHA[honey_pointsNoHA =="Arkansas",]
##############################
# Look at your map layers
##############################
p
plab
pHoney_Hawaii
pHoney_NoHawaii
