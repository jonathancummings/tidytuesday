# 6/2 tidyTuesday UseR meeting

# Get the Data
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# load library
library(tidyverse)

# Explore the data
unique(marbles$marble_name) # unique marble names
unique(marbles$team_name) # unique marble names

# Examine the data for a single marble
filter(marbles,marble_name=="Yellow")

summary(marbles$number_laps)

# evaluate the average speed per meter of each track
TrackSpeed <- marbles %>%
  filter(str_detect(race,"R")) %>% 
  group_by(site) %>% 
  summarise(track_avg_time_lap=mean(avg_time_lap,na.rm = TRUE),track_length=mean(track_length_m,na.rm = TRUE)) %>%
  mutate(track_speed=track_length/track_avg_time_lap) %>% 
  arrange(-track_speed)
TrackSpeed

# Plot 
PlotTrackSpeed<-ggplot(TrackSpeed,aes(x=reorder(site,track_speed),y=track_speed,fill=site,color=site))+
  geom_col(size=0.8)+coord_flip()+  xlab("Course")+ylab("Track Speed m/s")+theme_dark()+
  scale_fill_manual(values=c("olivedrab","black","midnightblue","darkgreen","orange","magenta","maroon","slategray3"))+
  scale_color_manual(values=c("burlywood4","yellow","skyblue2","royalblue4","orange","deeppink2","yellow2","white"))+
  theme(legend.position = "none")
PlotTrackSpeed
