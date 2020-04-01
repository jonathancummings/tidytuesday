library(tidytuesdayR)
library(tidyverse)

# Get the Data

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# # Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# # PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub
# 
# # Either ISO-8601 date or year/week works!
# 
# # Install via devtools::install_github("thebioengineer/tidytuesdayR")
# 
# tuesdata <- tidytuesdayR::tt_load('2020-03-31')
# tuesdata <- tidytuesdayR::tt_load(2020, week = 14)
# 
# 
# brewing_materials <- tuesdata$brewing_materials

# Trend in beer production by year
g_beer_total<-beer_taxed %>% group_by(year) %>% 
  filter(type=="Total Removals") %>% 
  summarize(yearly_total=sum(month_current)/1000000)
ggplot(g_beer_total,aes(x=year,y=yearly_total))+geom_line()+
  scale_x_continuous(breaks = seq(2008,2019))+ylab("Barrels Produced (Millions)")

# Determine the distinct beer production types
distinct(beer_taxed,type)
beer_taxed %>% group_by(year,month_current) %>% 
  filter(type=="In barrels and kegs"|type=="In kegs") %>% 
  summarize(kegs=sum(month_current))

# Compare the various forms of beer production by year
grouped_beer_type<-beer_taxed %>% group_by(type,year) %>% # group by type and year
  filter(type=="In bottles and cans"|type=="In barrels and kegs"|type=="For export"|
           type=="For vessels and aircraft"|type=="Consumed on brewery premises"|
           type=="In kegs"|type=="Total Removals") %>% # filter by production modes
  summarize(yearly_total=sum(month_current)/1000000) # produced yearly total in millions

# Kegs are split into two different types (see filter below), this combines them
kegs<-grouped_beer_type %>% group_by(year) %>% 
  filter(type=="In barrels and kegs"|type=="In kegs") %>% 
  summarize(yearly_total=sum(yearly_total)) %>% 
  mutate(type="Kegs")

#Join the new data to the original and keep just the new Kegs data
grouped_beer_type<-grouped_beer_type %>% 
  bind_rows(kegs) %>% 
  filter(type=="In bottles and cans"|type=="For export"|
           type=="For vessels and aircraft"|type=="Consumed on brewery premises"|
           type=="Kegs"|type=="Total Removals")
ggplot(grouped_beer_type,aes(x=year,y=yearly_total,group=type,color=type))+geom_line()+
  scale_x_continuous(breaks = seq(2008,2019))+ylab("Barrels Produced (Millions)")+
 theme(legend.title = element_blank(), 
          legend.text = element_text(size = 6.5),
       axis.text.x=element_text(size=6.5))

proportion_of_beer_production<-grouped_beer_type %>% 
  add_column(Total=rep(g_beer_total$yearly_total,6)) %>% 
  mutate(proportion=yearly_total/Total*100)
ggplot(proportion_of_beer_production,aes(x=year,y=proportion,group=type,color=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(2008,2019))+ylab("Percent of Total Production")+
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 6.5),
        axis.text.x=element_text(size=6.5))