# September 3rd, 2020 tidytuesday
# github repo link
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-01/readme.md

# load libraries
library(tidyverse)
library(gganimate)

# Get data
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')


key_crop_yields1<-key_crop_yields %>% 
  select(-Code) %>% 
  gather(key="Crop",value="Yield",-Entity,-Year)
cropsOverTime<-ggplot(key_crop_yields1,aes(x=Year,y=Yield,group=Crop))+geom_col()+
  theme(plot.title = element_text(size=20),plot.subtitle = element_text(size=18))+
  labs(title="Crop Yield",subtitle = "Annual Yield of: {closest_state}")+
  theme_bw()+
  transition_states(Crop)
animate(cropsOverTime, nframes = 10*length(unique(key_crop_yields1$Crop)),height = 600, width =800)

enitityOverTime<-key_crop_yields %>% 
  gather(key="Crop",value="Yield",-Entity,-Year, -Code) %>% 
  filter(Crop=="Rice (tonnes per hectare)") %>% 
  arrange(Year, Yield) %>% 
  drop_na() %>% 
  mutate(order = 1:n())  
enitityOverTimePlot<-ggplot(enitityOverTime,aes(x=order,y=Yield))+geom_col(aes(fill=Yield))+
  xlab("Entity Code")+scale_fill_viridis_c(option="magma")+
  geom_text(aes(y = Yield, label = Code), vjust = -0.5)+
  scale_x_continuous(breaks=enitityOverTime$order, labels=enitityOverTime$Code)+
  theme(plot.title = element_text(size=20),plot.subtitle = element_text(size=18))+
  labs(title="Annual Rice Yield",subtitle = "Rice Yield by Region in: {closest_state}")+
  theme_bw()+
  transition_states(Year)+
  view_follow(fixed_y=TRUE)
animate(enitityOverTimePlot, nframes = 5*length(unique(enitityOverTime$Year)),height = 600, width =1100)
