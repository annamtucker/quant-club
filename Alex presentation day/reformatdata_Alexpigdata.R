#case when - more succinct in dplyr package
#tidyverse
#ctrl+shift+m to read in (pipe), takes one thing and pipes it into function and output of function can be piped, can do a series of operations in a succinct way

# SQUAD sess --------------------------------------------------------------

library(tidyverse)
#install.packages("tidyverse")
library(lubridate)
Lookup2<-Lookup %>% 
  mutate(time=hour(Hour))
#use when times and dates read in as characters
PigData<-Lowndes_WMA_Spring_Turkey_Surveys_2015_2018_pig_data

OccLookup<-PigData %>% 
  select(Time, Date, `Total Pigs`) %>% 
  mutate(hour=hour(Time)) %>% 
  full_join(Lookup2,by=c("hour"="time")) %>% 
  select(Date,Interval) %>% 
  distinct() %>%  #gives only unique rows
  complete(Date,Interval) %>% 
  arrange(Date,Interval) %>% 
  mutate(OccID=c(1:length(Interval)))
  

OccData<-PigData %>% 
  select(Site, Year, Time, Date, `Total Pigs`) %>% 
  mutate(hour=hour(Time)) %>% 
  full_join(Lookup2,by=c("hour"="time")) %>% 
  select(-hour,-Hour) %>%  #got rid of two extra columns
  full_join(OccLookup,by=c("Date","Interval")) %>% 
  group_by(OccID,Site,Year) %>%  #subsets data by grouping variable
  summarise(Pig=ifelse(sum(`Total Pigs`)>0,1,0)) #gives one row per grouping variable (OccID)

OccTable<-table(OccData$Site,OccData$OccID) #can convert to a matrix



