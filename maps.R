rm(list=ls())

library(tidyverse)
library(viridis)
library(ggplot2)
library(ggpubr)
library(maps)
library(gganimate)
both.tot <- read.csv("ineq_pol_female.csv", header=T)
names(table(both.tot$country.x))
head(both.tot)
global <- map_data("world")
head(global)
map.n <- names(table(global$region))
data.n <- names(table(both.tot$country))
data.n[!(data.n %in% map.n)]


#Change in the names of the countries to make them match
global$region[global$region =="Antigua"]<-"Antigua and Barbuda"
global$region[global$region =="Barbuda"]<-"Antigua and Barbuda"
global$region[global$region =="Cape Verde"]<-"Cabo Verde"
both.tot$country[both.tot$country =="Bolivia (Plurinational State of)"]<-"Bolivia"
both.tot$country[both.tot$country =="Brunei Darussalam"]<-"Brunei"
global$region[global$region =="Jersey"]<-"Channel Islands"
global$region[global$region =="Guernsey"]<-"Channel Islands"
global$region[global$region =="Guernsey"]<-"Channel Islands"
both.tot$country[both.tot$country =="China, Taiwan Province of China"]<-"Taiwan"
both.tot$country[both.tot$country =="CÍ‰te d'Ivoire"]<-"Ivory Coast"
global$region[global$region =="Republic of Congo"]<-"Congo"
both.tot$country[both.tot$country =="CuraÍ_ao"]<-"Curacao"
both.tot$country[both.tot$country =="Czechia"]<-"Czech Republic"
both.tot$country[both.tot$country =="Dem. People's Republic of Korea"]<-"North Korea"
both.tot$country[both.tot$country =="Republic of Korea"]<-"South Korea"
both.tot$country[both.tot$country =="Iran (Islamic Republic of)"]<-"Iran"
both.tot$country[both.tot$country =="Lao People's Democratic Republic"]<-"Laos"
both.tot$country[both.tot$country =="Micronesia (Fed. States of)"]<-"Micronesia"
both.tot$country[both.tot$country =="Republic of Moldova"]<-"Moldova"
both.tot$country[both.tot$country =="RÍ©union"]<-"Reunion" 
both.tot$country[both.tot$country =="Russian Federation"]<-"Russia"
global$region[global$region =="Grenadines"]<-"Saint Vincent and the Grenadines"
global$region[global$region =="Saint Vincent"]<-"Saint Vincent and the Grenadines"
both.tot$country[both.tot$country =="Syrian Arab Republic"]<-"Syria" 
both.tot$country[both.tot$country =="State of Palestine"]<-"Palestine"
both.tot$country[both.tot$country =="TFYR Macedonia"]<-"Macedonia" 
global$region[global$region =="Trinidad"]<-"Trinidad and Tobago"
global$region[global$region =="Tobago"]<-"Trinidad and Tobago"
both.tot$country[both.tot$country =="United Kingdom"]<-"UK"
both.tot$country[both.tot$country =="United Republic of Tanzania"]<-"Tanzania"
both.tot$country[both.tot$country =="United States of America"]<-"USA"
both.tot$country[both.tot$country =="United States Virgin Islands"]<-"Virgin Islands"
both.tot$country[both.tot$country =="Venezuela (Bolivarian Republic of)"]<-"Venezuela"
both.tot$country[both.tot$country =="Viet Nam"]<-"Vietnam"


data.maps <- merge(global, both.tot, by.x = "region", by.y = "country", all.x=T)
data.maps <- data.maps[, c(2:5, 1, 6:ncol(data.maps))]
data.maps <- data.maps[order(data.maps$order), ]
colnames(data.maps)[7:12]<- c("xx", "id1", "year", "id2", "country", "iso_code")
nrow(data.maps)
head(data.maps)

mapa_animado_1 <- data.maps %>%
  ggplot(aes(long, lat, 
             group= group, 
             fill= var_c)) +
  geom_polygon(color = "white") +
  theme_void() +
  scale_fill_viridis(option = "B",
                     name= "Variance",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = FALSE,
                       title.position = "top",
                       title.hjust = 0.5,
                       title.vjust = 0.5 )) +
  labs(title="Variance",
       subtitle = "Year: {frame_time}")  +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.position = "bottom") +
  coord_fixed (ratio = 1.3) +
  transition_time(year)

animate(mapa_animado_1, nframes = 100, device = "png",
        renderer = file_renderer("~/variance", prefix = "map-", overwrite = TRUE))

