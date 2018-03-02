# Load in libraries
library(ggplot2)
library(dplyr)
library(deldir)

# Parameters to change as you like
iter=5 # Number of iterations (depth)
points=10 # Number of points
radius=0.2 # Factor of expansion/compression

# Angles of points from center
angles=seq(0, 2*pi*(1-1/points), length.out = points)+pi/2

# Initial center
df=data.frame(x=0, y=0)

# Iterate over centers again and again
for (k in 1:iter)
{
  temp=data.frame()
  for (i in 1:nrow(df))
  {
    data.frame(x=df[i,"x"]+radius^(k-1)*cos(angles), 
               y=df[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
  }
  df=temp
}

# Obtain Voronoi regions
df %>%
  select(x,y) %>% 
  deldir(sort=TRUE) %>% 
  .$dirsgs -> data

# Plot regions with geom_segment

data %>% 
  mutate(col = c(1:nrow(data))) %>% 
  ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, col= col), lwd = 1.5) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_fixed() +
  theme(legend.position  = "none",
        panel.background = element_rect(fill="white"),
        panel.border     = element_rect(colour = "black", fill=NA),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank()) +
  scale_color_distiller(palette = "Set1")
  

