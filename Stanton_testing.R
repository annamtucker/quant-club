## Plot honey production dataset

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

## How volatile are honey stocks and prices?
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

save_plot("annual_level_plots.png", annual_level_plots,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)
