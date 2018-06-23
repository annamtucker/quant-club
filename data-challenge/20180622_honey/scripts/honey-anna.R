require(tidyverse)
require(cowplot)
require(geofacet)

dat = read_csv("data-challenge/20180622_honey/honeyproduction.csv")

dat
table(dat$year)

ggplot(dat, aes(x = year, y = totalprod, col = state)) +
  geom_line() 

# does percent change from 1998-2016 vary by state?
dat %>% 
  filter(year == 1998) %>% 
  select(state, totalprod) %>% 
  rename(prod_98 = totalprod) %>% 
  left_join(dat) %>% 
  group_by(state) %>% 
  mutate(percent_change = totalprod/prod_98) %>% 
  ggplot(aes(x = year, y = percent_change, col = state)) +
  geom_line(aes(lwd = prod_98), alpha = 0.5)

# hard to see patterns--break out by state
dat %>% 
  filter(year == 1998) %>% 
  select(state, totalprod) %>% 
  rename(prod_98 = totalprod) %>% 
  left_join(dat) %>% 
  group_by(state) %>% 
  mutate(percent_change = totalprod/prod_98) %>% 
  ggplot(aes(x = year, y = percent_change)) +
  geom_line(aes(col = prod_98), lwd = 1) +
  facet_geo(~state) +
  viridis::scale_color_viridis()

# did high-producers in 1998 have a different trajectory than low-producers?
dat %>% 
  filter(year == 1998) %>% 
  select(state, totalprod) %>% 
  rename(prod_98 = totalprod) %>% 
  left_join(dat) %>% 
  group_by(state) %>% 
  mutate(percent_change = totalprod/prod_98) %>% 
  ggplot(aes(x = prod_98, y = percent_change)) +
  geom_point(size = 2, alpha = 0.5) 


# which states have the highest-producing colonies?
# average across all years
dat %>% 
  group_by(state) %>% 
  summarize(avgyield = mean(yieldpercol)) %>% 
  ungroup() %>% 
  arrange(avgyield) %>% 
  mutate(state = fct_reorder(state, avgyield)) %>% 
  ggplot(aes(x = state, y = avgyield)) +
  geom_bar(stat= "identity") +
  coord_flip() 

# what's the difference in ranking between 1998 and 2012?
plot98 <- dat %>% 
  filter(year == 1998) %>% 
  mutate(state = fct_reorder(state, yieldpercol)) %>% 
  ggplot(aes(x = state, y = yieldpercol)) +
  geom_bar(stat= "identity") +
  coord_flip() 

plot16 <- dat %>% 
  filter(year == 2012) %>% 
  mutate(state = fct_reorder(state, yieldpercol)) %>% 
  ggplot(aes(x = state, y = yieldpercol)) +
  geom_bar(stat= "identity") +
  coord_flip() 

plot_grid(plot98, plot16, ncol = 2)

# not super easy to see differences
# difference between 2012 and 1998?
dat %>% 
  filter(year %in% c(1998, 2012)) %>% 
  select(state, year, yieldpercol) %>% 
  spread(year, yieldpercol) %>% 
  mutate(diff = ((`2012`-`1998`)/`1998`)*100) %>% 
  arrange(diff) %>% 
  mutate(state = fct_reorder(state, diff)) %>% 
  ggplot(aes(x = state, y = diff)) +
  geom_bar(stat= "identity") +
  ylab("Percent difference in yield per colony\n1998-2012") +
  xlab("State")


# is honey price influenced by production?
dat %>% 
  ggplot(aes(x = totalprod, y = priceperlb)) +
  geom_point(size = 2, alpha = 0.5)

# the scale of total prod and priceperlb is v different
# also the distribution of totalprod is very skewed
hist(dat$totalprod, breaks = 100)

# log-transform total prod
dat %>% 
  mutate(logprod = log(totalprod)) %>% 
  ggplot(aes(x = logprod, y = priceperlb)) +
  geom_point(size = 2, alpha = 0.5)

dat %>% 
  mutate(logprod = log(totalprod)) %>% 
  ggplot(aes(x = logprod, y = priceperlb)) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~year)

# log transform price for normality
mod <- lm(log(priceperlb) ~ log(totalprod) + year, dat)

dat %>% 
  mutate(year.fct = factor(year),
         year.fct = fct_reorder(year.fct, -year),
         pred = exp(predict(mod))) %>% 
  ggplot(aes(x = totalprod/1000, y = priceperlb,
             col = year.fct)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(aes(y = pred), lwd = 1) +
  xlab("Total statewide honey production (lbs x1000)") +
  ylab("Price per pound ($)") +
  viridis::scale_color_viridis(discrete = T, option = "D",
                               name = "Year", direction = -1) +
  scale_x_continuous(trans = "log", breaks = c(100, 1000, 10000, 100000))
