## Plot honey production dataset

## libraries
library(tidyverse)
library(readr)

## data
honeyproduction <- read_csv("data-challenge/20180622_honey/honeyproduction.csv")
##http://www.whypad.com/posts/excel-spreadsheet-of-us-states/583/
## pasted in state names
state_names <-  c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", 
                 "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", 
                 "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
                 "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", 
                 "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
                 "VA", "WA", "WV", "WI", "WY")

## Visual and tabular error checking
summary(honeyproduction)
length(state_names)
levels (as.factor(honeyproduction$state))
which(levels(state_names))


plot(honeyproduction$year, honeyproduction$stocks)
