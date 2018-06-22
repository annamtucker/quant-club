## Plot honey production dataset to explore the relationship between
## stocks and prices and whether the price volatility is mediated by stock 
## volatility- it should be if the process is working.

## libraries
library(tidyverse)
library(cowplot)
library(readr)

## data
honeyproduction <- read_csv("data-challenge/20180622_honey/honeyproduction.csv")

## Visual and tabular error checking
summary(honeyproduction)
levels (as.factor(honeyproduction$state))
length(levels (as.factor(honeyproduction$state)))

## Which states are missing from the data set? Alaska and an 
## assortment of New England states. Why?
missing_states <- which (!(levels(as.factor(state.abb)) %in%  ## state.abb is in base R
      levels (as.factor(honeyproduction$state)) ))
levels(as.factor(state.abb))[c(missing_states)]

## Do some states have missing data? 
unique(honeyproduction$year)
length(unique(honeyproduction$year))
15*44 == nrow(honeyproduction)  ## Yup.
15*44 - nrow(honeyproduction)   ## 34 state and year pairings are absent

## Here I got bored and decided the missing data probably don't matter 
## for a casual exploration.

## How volatile are honey stocks and prices among states and years?
period_price_by_state <- group_by(honeyproduction, state) %>% 
  summarise(period_price = mean(priceperlb), period_sd = sd(priceperlb)) %>% 
  mutate(cv = period_sd/period_price)

period_stock_by_state <- group_by(honeyproduction, state) %>% 
  summarise(period_stock = mean(stocks), period_sd = sd(stocks)) %>% 
  mutate(cv = period_sd/period_stock)

period_price_by_year <- group_by(honeyproduction, year) %>% 
  summarise(period_price = mean(priceperlb), period_sd = sd(priceperlb)) %>% 
  mutate(cv = period_sd/period_price)

period_stock_by_year <- group_by(honeyproduction, year) %>% 
  summarise(period_stock = mean(stocks), period_sd = sd(stocks)) %>% 
  mutate(cv = period_sd/period_stock)

# Plotting ----------------------------------------------------------------
## by state
st_prices <- ggplot(period_price_by_state, aes(x = state, y = period_price)) +
  geom_point()+
  ggtitle("Average price of honey by state, 1998-2012")+ 
           xlab("State")+
           ylab("Average price, $")+
  coord_cartesian(ylim = c(0, 3)) 

st_stocks <- ggplot(period_stock_by_state, aes(x = state, y = period_stock)) +
  geom_point()+
  ggtitle("Average honey stocks by state, 1998-2012")+ 
  xlab("State")+
  ylab("Average stocks")

st_prices_cv <- ggplot(period_price_by_state, aes(x = state, y = cv)) +
  geom_point()+
  ggtitle("Average volatility (c.v.) of honey price by state, 1998-2012")+ 
  xlab("State")+
  ylab("Average price volatility (coef. of variation)")+
  coord_cartesian(ylim = c(0, 1.2)) 

st_stocks_cv <- ggplot(period_stock_by_state, aes(x = state, y = cv)) +
  geom_point()+
  ggtitle("Average stock volatility (c.v.) of honey by state, 1998-2012")+ 
  xlab("State")+
  ylab("Average stock volatility (coef. of variation)")+
  coord_cartesian(ylim = c(0, 1.2)) 

## by year
ann_prices <- ggplot(period_price_by_year, aes(x = year, y = period_price)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  ggtitle("Average honey prices, 1998-2012")+ 
  xlab("Year")+
  ylab("Average honey price, $")+
  coord_cartesian( ylim = c(0, 3)) 

ann_stocks <- ggplot(period_stock_by_year, aes(x = year, y = period_stock)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  ggtitle("Average honey stocks, 1998-2012")+ 
  xlab("Year")+
  ylab("Average honey stock")

ann_prices_cv <- ggplot(period_price_by_year, aes(x = year, y = cv)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  ggtitle("Average volatility (c.v.) of honey prices, 1998-2012")+ 
  xlab("Year")+
  ylab("Average price volatility (coef. of variation)")+
  coord_cartesian( ylim = c(0, 2.1)) 
  
ann_stocks_cv <- ggplot(period_stock_by_year, aes(x = year, y = cv)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  ggtitle("Average volatility (c.v.) of honey stocks, 1998-2012")+ 
  xlab("Year")+
  ylab("Average stock volatility (coef. of variation)")+
  coord_cartesian( ylim = c(0, 2.1)) 

## Assemble layouts
## By state
state_level_plots <- plot_grid(st_prices, st_stocks, st_prices_cv, st_stocks_cv,
          labels = c("A", "B", "C", "D"))

annual_level_plots <- plot_grid(ann_prices, ann_stocks, ann_prices_cv, ann_stocks_cv,
          labels = c("A", "B", "C", "D"))

## save as png with readable aspect ratios
## The patterns here seem idiosyncratic so far, 
## e.g. high prices states do not share high cost-of-living,
## high stock states are CA, ND, SD
## price volatility is consistent, HI is highest and well, it is the
## most remote island on the planet
## Stock volatility also makes no immediate sense and it probability 
## a result of stochastic events. perhaps evident in the colony data
## but I haven't looked. 

save_plot("state_level_plots.png", state_level_plots,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 3)

## Prices have increased while stocks are down.
## Prices are ~1/3 as volatile as stocks, with no real fluctuation 
## across years. The price increase appears modest but adjusting for 
## inlfation would an obvious next step to see if honey stocks have
## kept prices flat.

## US Bureau of Labor Statistics inflation calculator 
## https://www.bls.gov/data/inflation_calculator.htm
## indicates that $1 in 1998 was worth $1.40 in 2012, so the price is
## not completely flat, but pretty close.
save_plot("annual_level_plots.png", annual_level_plots,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 2.3)
