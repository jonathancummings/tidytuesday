library(tidyverse)
library(waffle)
library(fontawesome)
library(emojifont)

alternatives<-c("A","B","C","D")
cost<-c(10,25,50,5)
yield<-c(5000,5250,6000,7500)
prob.sustainable<-c(0.95,0.8,0.75,0.66)
profit<-c(100,102,98,125)

weights<-c(20,30,40,10)

norm.max<-function(x){(x-min(x))/(max(x)-min(x))}
norm.min<-function(x){1-(x-min(x))/(max(x)-min(x))}

cost.norm<-norm.min(cost)
yield.norm<-norm.max(yield)
prob.sustainable.norm<-norm.max(prob.sustainable)
profit.norm<-norm.max(profit)

cost.w<-cost.norm*weights[1]
yield.w<-yield.norm*weights[2]
prob.sustainable.w<-prob.sustainable.norm*weights[3]
profit.w<-profit.norm*weights[4]


utility<-data.frame(alternatives,cost.w,yield.w,prob.sustainable.w,profit.w) %>% 
  rowwise() %>% 
  mutate(utility=sum(c_across(where(is.numeric)))) 


waffle(utility$utility,rows=10, glyph_size = 2, use_glyph = "male")

extrafont::font_import (path="C:/Users/jcummings/Downloads", pattern = "fa-", prompt =  FALSE)

font_add(family = "FontAwesome5Free-Solid", regular = "C:/Users/jcummings/Downloads/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "C:/Users/jcummings/Downloads/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "C:/Users/jcummings/Downloads/fa-brands-400.ttf")

extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)

# code just to test github linkage
# 