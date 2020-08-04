# August 4th, 2020 tidytuesday
# github repo link
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-04/readme.md

# Goal play with gganimate()

# load libraries
library(tidyverse)
library(gganimate)

# Get data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

# Wrangle data and make annimated figure
energy_types %>% 
  pivot_longer(cols=c('2016','2017','2018'),names_to = "year",values_to = "produced") %>% 
  ggplot(aes(x=reorder(country,-produced),y=produced,fill=type))+geom_col(aes(color=year))+ 
  scale_color_brewer()+
  # Here comes the gganimate code
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  ) + 
  ease_aes('cubic-in-out')+
  ggtitle('Year: {closest_state}')