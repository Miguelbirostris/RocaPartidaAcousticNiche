
#These scripts run several functions related to niche analysis of a shark community in Roca Partida, Revillagigedo Archipelago, Mexico

#Code and models were written by Miguel de Jesus Gomez-Garcia as part of an ongoing publication "Coexistence at an oceanic oasis: vertical niches and partitioning within a shark community at an oceanic pinnacle" 

#This code has not been optimized for speed

#Created by Miguel de Jesus Gomez Garcia
#Created: 15-Apr-2026
#Last edited: 13-May-2026
#First uploaded to GitHub on 13-May-2026

# Load Data ---------------------------------------------------------------

#packages
library(tidyverse)
library(nppen)
library(ggplot2)
library(viridis)
library(dunn.test)
library(mgcv)
library(nppen)
library(EcoSimR)
library(nppen)
library(rerddapXtracto)
library(zoo)
library(foreach)
library(doParallel)



#Original data


RPdata<-read.csv("RP_DailyMean_Acoustic_extractomatic.csv")

summary (RPdata)

## Fast plots / visualizations --------------------------------------------------------

#Species And Depth detections

RPdata$Common.Name <- factor(RPdata$Common.Name, levels = unique(c("Galapagos","Silky","Silvertip","Whitetip")))
RPdata$Date<-as.Date(RPdata$Date)
RPdata$Year<-as.Date(RPdata$Date)

DescPlot <- ggplot(data = RPdata, aes(Date, -Altitude, col = `Common.Name`)) +
  geom_point() +
  ylab("Depth (m)") +
  scale_color_viridis_d(option = "turbo") +
  ggtitle(" ")+ 
  theme_classic(base_size = 30) +
  theme(
    plot.margin = margin(t = 20, r = 35, b = 20, l = 20),
    legend.position = "bottom",
    legend.title.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  ) +
  scale_x_date(
       date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(n.breaks = 19) +
  xlab("Year") +
  labs(color = "Species")
DescPlot

ggsave("Figure1_SpeciesDetections.jpg", plot = DescPlot,
       width = 16, height = 10, dpi = 300)

RPdata$Species<-factor(RPdata$Species, levels=c("Carcharhinus galapagensis","Carcharhinus falciformis","Carcharhinus albimarginatus","Triaenodon obesus"))

DescPlot<-ggplot(data=RPdata, aes(Date,
                                  -Altitude,col=`Species`)) +
  geom_point() + ylab("Depth (m)")+
  scale_color_viridis_d(option="turbo")+
  ggtitle(" ")+  
  theme_classic(base_size = 30) +
  theme(
    plot.margin = margin(t = 20, r = 35, b = 20, l = 20),
    legend.position = "bottom",
    legend.title.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 20)
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(n.breaks = 19) +
  xlab("Year") +
  labs(color = "Species")
DescPlot

ggsave("Figure1b_SpeciesDetections.jpg", plot = DescPlot,
       width = 16, height = 10, dpi = 300)

# ggsave("SpeciesDetections2.jpg", plot =DescPlot, width = 3000, height = 2400, dpi = 300, units = "px")


##Abaccus plot. color =Tag T° 
ggplot(data=RPdata, aes(Date,as.factor(as.character(
  Tag.ID)),col=Temperature)) +
  geom_point() + ylab("Transmitter")+
  ggtitle("Detections Roca Partida")+ theme_classic() + scale_colour_gradient2(low= "white", mid= "yellow1", high="red3", midpoint = 22)

##Abaccus plot. color = SST° 
ggplot(data=RPdata, aes(Date,as.factor(as.character(
  Tag.ID)),col=SST)) +
  geom_point() + ylab("Transmitter")+
  ggtitle("Detections Roca Partida")+ theme_classic() + scale_colour_gradient2(low= "white", mid= "yellow1", high="red3", midpoint = 22)



RPdata$Year<-format(as.Date(RPdata$Date), "%Y")

paste0(RPdata$Date)

##Abaccus plot. color =Species

unique(RPdata$Species)
RPdata$Species<-factor(RPdata$Species, levels=c("Carcharhinus galapagensis","Carcharhinus falciformis","Carcharhinus albimarginatus","Triaenodon obesus"))

AbbacusSP<-ggplot(data=RPdata, aes(Date,as.factor(as.character(
  Tag.ID)),col=Species)) +
  scale_color_viridis_d(option="turbo")+
  geom_point() + ylab("Individual shark")+
  ggtitle("Detections at Roca Partida")+ 
  theme_classic(base_size = 30) +
  theme(
    plot.margin = margin(t = 20, r = 35, b = 20, l = 20),
    legend.position = "bottom",
    legend.title.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 18)
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  xlab("Year") +
  labs(color = "Species")
 
AbbacusSP

ggsave("Figure2_AbaccusSpp.jpg", plot = AbbacusSP,
       width = 16, height = 10, dpi = 300)



##Abaccus plot. color =Common name
AbbacusSP<-ggplot(data=RPdata, aes(Date,as.factor(as.character(
  Tag.ID)),col=Common.Name)) +
  scale_color_viridis_d(option="turbo")+
  geom_point() + ylab("Individual shark")+
  ggtitle("Detections at Roca Partida")+ 
  theme_classic(base_size = 30) +
  theme(
    plot.margin = margin(t = 20, r = 35, b = 20, l = 20),
    legend.position = "bottom",
    legend.title.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 18)
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  xlab("Year") +
  labs(color = "Species")

AbbacusSP

ggsave("Figureb2_AbaccusSpp.jpg", plot = AbbacusSP,
       width = 16, height = 10, dpi = 300)

AbbacusSP

ggsave("AbaccusSpp2.png", plot = AbbacusSP, width = 3600, height = 2400, dpi = 300, units = "px")

#Additional formats

AbbacusSP<-ggplot(data=RPdata, aes(Date,as.integer(as.factor(as.character(
  Tag.ID))),col=Common.Name)) +
  scale_color_viridis_d(option="turbo")+
  geom_point() + ylab("Individual shark")+
  ggtitle("Detections at Roca Partida")+ theme_classic(base_size=15)+
  theme(legend.position="bottom",
      legend.title.position = "bottom",
      legend.title = element_text(hjust = 0.5)
  )+
  scale_x_date(date_breaks="3 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 19)+
  xlab(NULL)+
  labs(color = "Species") 

AbbacusSP

##color =depth
ggplot(data=RPdata, aes(Date,as.factor(as.character(
  Tag.ID)),col=Depth)) +
  geom_point() + ylab("Transmitter")+
  ggtitle("Detections Roca Partida")+ theme_classic() + scale_color_viridis_c(option="mako")


#Data summary and exploration---------------------------------------


#######Data metrics---------------------------------------

RPdata<-as.data.frame(RPdata)
#anova(RPdata)

RPdataS<-RPdata[,c(2,9,13)]

unique(RPdata$Tag.ID)

kruskal.test(RPdata$Depth, RPdata$Tag.ID)  #diff

dunn.test(RPdata$Temperature, RPdata$Tag.ID) #diff
dunn.test(RPdata$Depth, RPdata$Tag.ID)   #diff

kruskal.test(RPdata$Temperature, RPdata$Species) #diff
kruskal.test(RPdata$Depth, RPdata$Species)   #diff


## Gamms ------------------------------------------------

#Edit data to add seasonal component

#Detection per individual gam
df<-RPdata|>
   arrange(Date)|>
  dplyr::mutate(Date = lubridate::floor_date(Date, "day"),
                Date_num = as.numeric(Date), #for modeling
                week=lubridate::week(Date),
                sin=sin(2*pi*week/52), 
                cos=cos(2*pi*week/52),
                cyclic_week = sin+cos 
               )

#Detection models

#weeks as seasonal component
gam_mod<-gam(
  Detections ~ s(Date_num, k = 12) +
    s(week, bs = "cc", k = 12) +
    s(Species, bs = "re"),
  family = nb(),
  data = df
)

#Simple GLM sin+cos transformation
gl_mod<-lm(
  Detections ~ Date_num +
    sin +
    cos + Species,
    data = df
)


#Visualization options

# gam_mod
# vis.gam(gam_mod)
# summary(gam_mod)
# gam.check(gam_mod)
# 
# 
# plot(gam_mod)


#Depth models

#Rearrange DF for depth response variables
df_depths <- RPdata %>%
  filter(Depth < 0) %>%
  mutate(
    Depth = -Depth,
    Date_num = as.numeric(Date), 
    week=lubridate::week(Date),
    sin=sin(2*pi*week/52), 
    cos=cos(2*pi*week/52),
    cyclic_week = sin+cos 
  )


#Gam

gam_depth <- gam(
  Depth ~ 
    s(Date_num, k = 20) +
    s(week, bs = "cc", k = 12) +
    s(Species, bs = "re") +
    ti(week, Species, bs = c("cc", "re")), #Tensor product, includes interactions between season (cyclic week) and species
  weights = Detections, #eh, its hard to incorporate detections, so im using it as an offset here. More detections at a certain depth, more weight to the relationship. Hope it make sence !
  data = df,
  family = gaussian()
)

gam_depth
summary(gam_depth)

# Niche -------------------------------------------------------------------



DepthTable<-as.data.frame(RPdata%>%group_by(Species)%>%summarise(mindepth=min(Depth),meandepth=mean(-Depth),maxdepth=max(-Depth)))


write.csv(DepthTable,"DepthTable.csv")


DetectionSummary<-RPdata%>% group_by(Tag.ID) %>% summarize( 
  Sci.Name = first(Species), Sex = first(Sex), TLcm = first(TLcm), 
  Number.of.Days = n(), 
  FirstDet=first(Date),LastDet=last(Date),
  DaysLiberty=difftime(LastDet ,FirstDet , units = c("days")),
  MeanDepth = mean(Depth),
  Maxdepth=min(Depth),
  Mindepth=max(Depth),
  SD_Depth=sd(Depth),
  MeanTemperature = mean(na.omit(Temperature)), SD_Temperature=sd(na.omit(Temperature))
)

write.csv(DetectionSummary,"SummaryTable.csv")


#Create objects by species


Silky_dat<-RPdata[RPdata$Species=="Carcharhinus falciformis",]
Whitetip_dat<-RPdata[RPdata$Species=="Triaenodon obesus",]
Silvertip_dat<-RPdata[RPdata$Species=="Carcharhinus albimarginatus",]
Galapagos_dat<-RPdata[RPdata$Species=="Carcharhinus galapagensis",]


#Depth and temperature (SST)
Silky_T<-dplyr::select( Silky_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","SST"))
Whitetip_T<-dplyr::select( Whitetip_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","SST"))
Silvertip_T<-dplyr::select( Silvertip_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","SST"))
Galapagos_T<-dplyr::select( Galapagos_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","SST"))


#Depth and month
Silky_M<-dplyr::select( Silky_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","month"))
Whitetip_M<-dplyr::select( Whitetip_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","month"))
Silvertip_M<-dplyr::select( Silvertip_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","month"))
Galapagos_M<-dplyr::select( Galapagos_dat,c("Tag.ID","Date","Species","Latitude","Longitude","Depth","month"))

#Plot heathmaps of depth utilization

#Month labs
mlabs<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")



#Silky

ggplot(data.frame(Silky_M)) + 
  scale_y_continuous(limits = c(-50, 0)) +
  scale_x_discrete(limits = factor(1:12), labels = mlabs) +
  geom_density_2d_filled(aes(month, Depth, fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Density (%)", 
    labels = function(x) {
      # Extract numeric intervals from the factor levels
      as.numeric(sub("^\\((.*),.*$", "\\1", x)) * 100
    }
  ) +
  ylab("Depth (m)") +
  xlab("Month") +
  ggtitle("Carcharhinus falciformis")


#Whitetip
ggplot(data.frame(Whitetip_M)) + 
  scale_y_continuous(limits = c(-80, 0)) +
  scale_x_discrete(limits = factor(1:12), labels = mlabs) +
  geom_density_2d_filled(aes(month, Depth, fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Density (%)", 
    labels = function(x) {
      # Extract numeric intervals from the factor levels
      as.numeric(sub("^\\((.*),.*$", "\\1", x)) * 100
    }
  ) +
  ylab("Depth (m)") +
  xlab("Month") +
  ggtitle("Triaenodon obesus")

#Silvertip

ggplot(data.frame(Silvertip_M)) + 
  scale_y_continuous(limits = c(-100, 0)) +
  scale_x_discrete(limits = factor(1:12), labels = mlabs) +
  geom_density_2d_filled(aes(month, Depth, fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Density (%)", 
    labels = function(x) {
      # Extract numeric intervals from the factor levels
      as.numeric(sub("^\\((.*),.*$", "\\1", x)) * 100
    }
  ) +
  ylab("Depth (m)") +
  xlab("Month") +
  ggtitle("Carcharhinus albimarginatus")

#Galapagos

ggplot(data.frame(Galapagos_M)) + 
  scale_y_continuous(limits = c(-70, 0)) +
  scale_x_discrete(limits = factor(1:12), labels = mlabs) +
  geom_density_2d_filled(aes(month, Depth, fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Density (%)", 
    labels = function(x) {
      # Extract numeric intervals from the factor levels
      as.numeric(sub("^\\((.*),.*$", "\\1", x)) * 100
    }
  ) +
  ylab("Depth (m)") +
  xlab("Month") +
  ggtitle("Carcharhinus galapagensis")


#Facetted heatmap depth utilization

TemporalNiche <- ggplot(data.frame(RPdata)) + 
  scale_y_continuous(limits = c(-100, 0)) +
  scale_x_discrete(limits = factor(1:12), labels = mlabs) +
  geom_density_2d_filled(aes(month, Depth, fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Density (%)", 
    labels = function(x) {
      # Extract numeric intervals from the factor levels
      as.numeric(sub("^\\((.*),.*$", "\\1", x)) * 100
    }
  ) +
  ylab("Depth (m)") +
  xlab("Month") +
  ggtitle("")+
  theme( strip.text = element_text(size = 20),
         axis.title = element_text(size=25),
         axis.text = element_text(size = 20),
         axis.text.x = element_text(angle = 90,hjust = 0.5, vjust=0)
  )+
  facet_wrap(~Common.Name, scales = "free_y" )

TemporalNiche

ggsave("Fig3.Temporal_NicheObserved.png", plot = TemporalNiche, width = 3000, height = 2400, dpi = 300, units = "px")


# NPPEN -------------------------------------------------------------------

# define target points: Environmental data for possible points to be predicted

Silky_Predicted <- data.frame(Depth=rnorm(1000, mean(Silky_T$Depth), sd(Silky_T$Depth)),Temperature=rnorm(1000, mean(Silky_T$SST), sd(Silky_T$SST)))

Silky_Observed<-dplyr::select( as.data.frame(Silky_T),c("Depth","SST"))


Whitetip_Predicted <- data.frame(Depth=rnorm(1000, mean(Whitetip_T$Depth), sd(Whitetip_T$Depth)),Temperature=rnorm(1000, mean(Whitetip_T$SST), sd(Whitetip_T$SST)))

Whitetip_Observed<-dplyr::select( as.data.frame(Whitetip_T),c("Depth","SST"))

Silvertip_Predicted <- data.frame(Depth=rnorm(1000, mean(Silvertip_T$Depth), sd(Silvertip_T$Depth)),Temperature=rnorm(1000, mean(Silvertip_T$SST), sd(Silvertip_T$SST)))

Silvertip_Observed<-dplyr::select( as.data.frame(Silvertip_T),c("Depth","SST"))

Galapagos_Predicted <- data.frame(Depth=rnorm(1000, mean(Galapagos_T$Depth), sd(Galapagos_T$Depth)),Temperature=rnorm(1000, mean(Galapagos_T$SST), sd(Galapagos_T$SST)))

Galapagos_Observed<-dplyr::select( as.data.frame(Galapagos_T),c("Depth","SST"))

Silky_Probability<- nppen(Silky_Observed,Silky_Predicted, fast=T)
Whitetip_Probability<- nppen(Whitetip_Observed,Whitetip_Predicted, fast=TRUE)
Silvertip_Probability<- nppen(Silvertip_Observed,Silvertip_Predicted, fast=TRUE)
Galapagos_Probability<- nppen(Galapagos_Observed,Galapagos_Predicted, fast=TRUE)



Silky_grid <-as.data.frame(Silky_Predicted)
Silky_grid$Probability<-Silky_Probability
Silky_grid <- akima::interp(Silky_grid$Depth,Silky_grid$Temperature , Silky_grid$Probability)
Silky_grid <- data.frame(x = rep(Silky_grid$x, ncol(Silky_grid$z)), 
                         y = rep(Silky_grid$y, each = nrow(Silky_grid$z)), 
                         z = as.numeric(Silky_grid$z))
names(Silky_grid)<-c("Depth", "SST", "Probability")
Silky_grid$Probability[is.na(Silky_grid$Probability)]<-0


Whitetip_grid <-as.data.frame(Whitetip_Predicted)
Whitetip_grid$Probability<-Whitetip_Probability
Whitetip_grid <- akima::interp(Whitetip_grid$Depth,Whitetip_grid$Temperature , Whitetip_grid$Probability)
Whitetip_grid <- data.frame(x = rep(Whitetip_grid$x, ncol(Whitetip_grid$z)), 
                            y = rep(Whitetip_grid$y, each = nrow(Whitetip_grid$z)), 
                            z = as.numeric(Whitetip_grid$z))
names(Whitetip_grid)<-c("Depth", "SST", "Probability")
Whitetip_grid$Probability[is.na(Whitetip_grid$Probability)]<-0

Silvertip_grid <-as.data.frame(Silvertip_Predicted)
Silvertip_grid$Probability<-Silvertip_Probability
Silvertip_grid <- akima::interp(Silvertip_grid$Depth,Silvertip_grid$Temperature , Silvertip_grid$Probability)
Silvertip_grid <- data.frame(x = rep(Silvertip_grid$x, ncol(Silvertip_grid$z)), 
                             y = rep(Silvertip_grid$y, each = nrow(Silvertip_grid$z)), 
                             z = as.numeric(Silvertip_grid$z))
names(Silvertip_grid)<-c("Depth", "SST", "Probability")
Silvertip_grid$Probability[is.na(Silvertip_grid$Probability)]<-0


Galapagos_grid <-as.data.frame(Galapagos_Predicted)
Galapagos_grid$Probability<-Galapagos_Probability
Galapagos_grid <- akima::interp(Galapagos_grid$Depth,Galapagos_grid$Temperature , Galapagos_grid$Probability)
Galapagos_grid <- data.frame(x = rep(Galapagos_grid$x, ncol(Galapagos_grid$z)), 
                             y = rep(Galapagos_grid$y, each = nrow(Galapagos_grid$z)), 
                             z = as.numeric(Galapagos_grid$z))
names(Galapagos_grid)<-c("Depth", "SST", "Probability")
Galapagos_grid$Probability[is.na(Galapagos_grid$Probability)]<-0

summary(Silky_grid)


#Heatmap Visualization of NPPEN outputs 

ggplot(data = Silky_grid, aes(x = SST, y = Depth, z = Probability)) + 
  geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.8) + 
  scale_fill_viridis_d(
    option = "mako", 
    name = "Probability", 
  ) +
  ylab("Depth (m)") +
  xlab("Temperature (°C)") +
  coord_cartesian(ylim = c(min(Silky_grid$Depth), 0)) +
  ggtitle("Carcharhinus falciformis")



ggplot(data = Whitetip_grid, aes(x = SST, y = Depth, z = Probability)) + 
  geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.8) + 
  scale_fill_viridis_d(
    option = "mako", 
    name = "Probability", 
  ) +
  ylab("Depth (m)") +
  xlab("Temperature (°C)") +
  coord_cartesian(ylim = c(min(Whitetip_grid$Depth), 0)) +
  ggtitle("Triaenodon obesus")


ggplot(data = Silvertip_grid, aes(x = SST, y = Depth, z = Probability)) + 
  geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.8) + 
  scale_fill_viridis_d(
    option = "mako", 
    name = "Probability",) +
  ylab("Depth (m)") +
  xlab("Temperature (°C)") +
  coord_cartesian(ylim = c(min(Silvertip_grid$Depth), 0)) +
  ggtitle("Carcharhinus albimarginatus")

ggplot(data = Galapagos_grid, aes(x = SST, y = Depth, z = Probability)) + 
  geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.8) + 
  scale_fill_viridis_d(
    option = "mako", 
    name = "Probability",) +
  ylab("Depth (m)") +
  xlab("Temperature (°C)") +
  coord_cartesian(ylim = c(min(Galapagos_grid$Depth), 0)) +
  ggtitle("Carcharhinus galapagensis")


#Niche overlap


Silky_grid$Species<- "Silky"
Whitetip_grid$Species<- "Whitetip"
Silvertip_grid$Species <- "Silvertip"
Galapagos_grid$Species<- "Galapagos"

#Create merged data frame for facetted ploting 

Npen_DF<-rbind(Silky_grid,Whitetip_grid,Silvertip_grid,Galapagos_grid)

plot(Npen_DF$Depth,Npen_DF$Probability)



#Save depth NPPEN plot

ProbDist<-ggplot(data = Npen_DF, aes(x = -Depth, y = Probability)) +
  geom_point(size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Probability") +
  xlab("Depth (m)") +
  ggtitle(" ") +
  theme_minimal(base_size = 20)

ProbDist

ggsave("Fig_6.NPPEN_ProbDist.jpg", plot = ProbDist, width = 3000, height = 2400, dpi = 300, units = "px")


#Temperature NPPEN plot


ProbDist_temp<-ggplot(data = Npen_DF, aes(x = SST, y = Probability)) +
  geom_point(size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Probability") +
  xlab("Temperature (SST)") +
  ggtitle(" ") +
  theme_minimal(base_size = 20)

ProbDist_temp

ggsave("Fig_6.5.NPPEN_SST_ProbDist.jpg", plot = ProbDist_temp, width = 3000, height = 2400, dpi = 300, units = "px")



#correlation

cor.test(Galapagos_grid$Depth,Galapagos_grid$Probability)
cor.test(Silky_grid$Depth,Silky_grid$Probability)
cor.test(Silvertip_grid$Depth,Silvertip_grid$Probability)
cor.test(Whitetip_grid$Depth,Whitetip_grid$Probability)

#Facetted heatmap depth-temperature utilization

NichePlot<-ggplot(data = Npen_DF[Npen_DF$Depth<0,], aes(x = SST, y = Depth, z = Probability)) + 
  geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.8) +
  scale_fill_viridis_d(
    option = "mako", 
    name = "Probability",) +
  ylab("Depth (m)") +
  xlab("Temperature (°C)") +
  #coord_cartesian(ylim = c(min(Npen_DF$Depth), 0)) +
  ggtitle(" ")+
  theme( strip.text = element_text(size = 20),
         axis.title = element_text(size=25),
         axis.text = element_text(size = 20),
         axis.text.x = element_text(angle = 90,hjust = 0.5, vjust=0)
  )+
  facet_wrap(~Species, scales="free")

NichePlot

# Save the plot as a high-quality PNG image with pixel units
ggsave("Depth_Temperature_NicheB.png", plot = NichePlot, width = 3000, height = 2400, dpi = 300, units = "px")



# Ecosim Tests ------------------------------------------------------------


#Load Algorithms and scripts

# Create a new data frame with species-specific data
Npen_long <- Npen_DF[Npen_DF$Depth < 0,]

Npen_long$Depths <- -as.integer(Npen_long$Depth)

Npen_long <- Npen_long %>%
  group_by(Species, Depths) %>%
  summarise(Probability = mean(Probability), .groups = 'drop')

# Convert Depths to numeric before sorting
Npen_long$Depths <- as.numeric(as.character(Npen_long$Depths))

# Ensure Depths are sorted numerically
Npen_long$Depths <- factor(Npen_long$Depths, levels = sort(unique(Npen_long$Depths)))

Npen_long <- Npen_long %>%
  pivot_wider(names_from = Depths, values_from = Probability, values_fill = list(Probability = 0))

# Sort the column names numerically by converting to numeric, then back to character for proper ordering
colnames(Npen_long)[-1] <- sort(as.numeric(colnames(Npen_long)[-1]))
colnames(Npen_long)[-1] <- as.character(colnames(Npen_long)[-1])

# Reorder columns
Npen_long <- Npen_long %>%
  dplyr::select(Species, everything())

#create a csv. Depending on your computer, you may want to open it and delete the empty  first column
write.csv(Npen_long, "NpenMatrix.csv", row.names = FALSE)


#Niche overlap Overall
Data.File <- "NpenMaatrix.csv"
Output.File <-"Niche Overlap Output.txt"
Algorithm <- "RA3"	#choices are "RA1", "RA2", "RA3", "RA4"; default is "RA3"
Metric <- "Pianka"	#choices are "Pianka", "Czekanowski", 
#"Pianka.var", "Czekanowski.var",
# "Pianka.skew", "Czekanowski.skew"; default is Pianka
N.Reps <- 1000	# 1000 is the typical number of replicates, but any number > 2 will run
Random.Seed <- 625 ## If 0, uses random integer. User can replace 0 with your integer of choice e.g. 107
Plot.Output <- "file" 	#choices are "file", "screen", "none"; default is "screen"
Print.Output <- "file"	#choices are "file", "screen", "none"; default is "screen"
Display.About <- "none" # choices are "screen", "none"; default is "none"
Graphic <- "Niche.Overlap.Plot" # other choices will be added with  specified from user inputs

#Load ecosim functions
source("EcoSimR - Algorithms Source.R")
source("EcoSimR - General Functions Source.R")
source("EcoSimR - Metrics Source.R")
source("EcoSimR - Graphics Source.R")


Param.List <- Get.Params(Data.File,Output.File,Algorithm,Metric,
                         N.Reps,Random.Seed,Plot.Output,Print.Output,Display.About,Graphic)
RandomInteger <- Set.The.Seed(Param.List)


Null.Result <- Null.Model.Engine(Param.List)

Output.Results(Param.List,Null.Result)


## SpeciesPairAnalysis -----------------------------------------------------


#Create species pairs for ecosim analysis

Galapagos_Silky <- Npen_long[Npen_long$Species %in% c("Galapagos", "Silky"), ]
Galapagos_Silvertip <- Npen_long[Npen_long$Species %in% c("Galapagos", "Silvertip"), ]
Galapagos_Whitetip <- Npen_long[Npen_long$Species %in% c("Galapagos", "Whitetip"), ]
Silky_Silvertip <- Npen_long[Npen_long$Species %in% c("Silky", "Silvertip"), ]
Silky_Whitetip <- Npen_long[Npen_long$Species %in% c("Silky", "Whitetip"), ]
Silvertip_Whitetip <- Npen_long[Npen_long$Species %in% c("Silvertip", "Whitetip"), ]

# Save each matrix as a separate CSV file
write.csv(Galapagos_Silky, "Galapagos_Silky.csv", row.names = FALSE)
write.csv(Galapagos_Silvertip, "Galapagos_Silvertip.csv", row.names = FALSE)
write.csv(Galapagos_Whitetip, "Galapagos_Whitetip.csv", row.names = FALSE)
write.csv(Silky_Silvertip, "Silky_Silvertip.csv", row.names = FALSE)
write.csv(Silky_Whitetip, "Silky_Whitetip.csv", row.names = FALSE)
write.csv(Silvertip_Whitetip, "Silvertip_Whitetip.csv", row.names = FALSE)


# List of species pairs
species_pairs <- list(
  c("Galapagos", "Silky"),
  c("Galapagos", "Silvertip"),
  c("Galapagos", "Whitetip"),
  c("Silky", "Silvertip"),
  c("Silky", "Whitetip"),
  c("Silvertip", "Whitetip")
)

# Function to run the analysis for each species pair
run_niche_overlap_analysis <- function(species1, species2) {
  # Set the input and output file names based on the species pair
  Data.File <- paste0(species1, "_", species2, "_Matrix.csv")
  Output.File <- paste0(species1, "_", species2, "_Niche_Overlap_Output.txt")
  
  # Set the parameters for the niche overlap analysis
  Algorithm <- "RA3"  # Example algorithm, you can adjust as needed
  Metric <- "Pianka"  # Example metric, you can adjust as needed
  N.Reps <- 1000      # Number of replicates
  Random.Seed <- 625  # Random seed
  Plot.Output <- "file"   # Output plot to a file
  Print.Output <- "file"  # Print output to a file
  Display.About <- "none" # Don't display about information
  Graphic <- "Niche.Overlap.Plot"  # Define plot type
  
  # Get the parameters for the niche overlap calculation
  Param.List <- Get.Params(Data.File, Output.File, Algorithm, Metric,
                           N.Reps, Random.Seed, Plot.Output, Print.Output, Display.About, Graphic)
  
  # Set the random seed for the model
  RandomInteger <- Set.The.Seed(Param.List)
  
  # Run the null model engine
  Null.Result <- Null.Model.Engine(Param.List)
  
  # Output the results
  Output.Results(Param.List, Null.Result)
}

# Loop over each species pair and run the analysis
for (pair in species_pairs) {
  species1 <- pair[1]
  species2 <- pair[2]
  
  # Subset the Npen_long data for the current species pair and save as CSV
  pair_data <- Npen_long[Npen_long$Species %in% c(species1, species2), ]
  pair_file <- paste0(species1, "_", species2, "_Matrix.csv")
  write.csv(pair_data, pair_file, row.names = FALSE)
  
  # Run the niche overlap analysis for the current species pair
  run_niche_overlap_analysis(species1, species2)
}


# Ecosim Tests With Raw Data ------------------------------------------------------------



# Create a new data frame with species-specific data from raw data

RPdata_subset<-RPdata%>%dplyr::select(Tag.ID,Common.Name,Depth, Detections)%>% rename(Species=Common.Name)

head(RPdata_subset)

RawNiche_long <- RPdata%>%dplyr::select(Depth, Common.Name,Detections)%>% rename(Species=Common.Name)


RawNiche_long <- RPdata_subset<-RawNiche_long[RawNiche_long$Depth < 0,]

RawNiche_long$Depths <- -as.integer(RawNiche_long$Depth)

RawNiche_long <- RawNiche_long %>%
  group_by(Species, Depths) %>%
  summarise(Probability = mean(Detections), .groups = 'drop')

# Convert Depths to numeric before sorting
RawNiche_long$Depths <- as.numeric(as.character(RawNiche_long$Depths))

# Ensure Depths are sorted numerically
RawNiche_long$Depths <- factor(RawNiche_long$Depths, levels = sort(unique(RawNiche_long$Depths)))


#Visualize

dets_bydep <- RawNiche_long |>
  mutate(Depths = as.numeric(as.character(Depths))) |>
  ggplot(aes(x = Depths, y = Probability)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0,250,10)
  ) +
  ylab("Detections") +
  facet_wrap(~Species, scales = "free_y") +
  theme_classic()

ggsave("AverageDetections_Depth.jpg", plot = dets_bydep,
       width = 12, height = 10, dpi = 300)

#Turn into matrix
RawNiche_long <- RawNiche_long %>%
  pivot_wider(names_from = Depths, values_from = Probability, values_fill = list(Probability = 0))

# Sort the column names numerically by converting to numeric, then back to character for proper ordering
colnames(RawNiche_long)[-1] <- sort(as.numeric(colnames(RawNiche_long)[-1]))
colnames(RawNiche_long)[-1] <- as.character(colnames(RawNiche_long)[-1])

# Reorder columns
RawNiche_long <- RawNiche_long %>%
  dplyr::select(Species, everything())

#create a csv. Depending on your computer, you may want to open it and delete the empty  first column
write.csv(RawNiche_long, "RAWMatrix.csv", row.names = FALSE)


#Niche overlap Overall
Data.File <- "RAWMatrix.csv"
Output.File <-"RAW Niche Overlap Output.txt"
Algorithm <- "RA3"	#choices are "RA1", "RA2", "RA3", "RA4"; default is "RA3"
Metric <- "Pianka"	#choices are "Pianka", "Czekanowski", 
#"Pianka.var", "Czekanowski.var",
# "Pianka.skew", "Czekanowski.skew"; default is Pianka
N.Reps <- 5000	# 1000 is the typical number of replicates, but any number > 2 will run
Random.Seed <- 625 ## If 0, uses random integer. User can replace 0 with your integer of choice e.g. 107
Plot.Output <- "file" 	#choices are "file", "screen", "none"; default is "screen"
Print.Output <- "file"	#choices are "file", "screen", "none"; default is "screen"
Display.About <- "none" # choices are "screen", "none"; default is "none"
Graphic <- "Niche.Overlap.Plot" # other choices will be added with  specified from user inputs

Param.List <- Get.Params(Data.File,Output.File,Algorithm,Metric,
                         N.Reps,Random.Seed,Plot.Output,Print.Output,Display.About,Graphic)
RandomInteger <- Set.The.Seed(Param.List)


Null.Result <- Null.Model.Engine(Param.List)

Output.Results(Param.List,Null.Result)


## SpeciesPairAnalysis -----------------------------------------------------



#Create species pairs for ecosim analysis

Galapagos_Silky <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Silky"), ]
Galapagos_Silvertip <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Silvertip"), ]
Galapagos_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Whitetip"), ]
Silky_Silvertip <- RawNiche_long[RawNiche_long$Species %in% c("Silky", "Silvertip"), ]
Silky_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Silky", "Whitetip"), ]
Silvertip_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Silvertip", "Whitetip"), ]

# Save each matrix as a separate CSV file
write.csv(Galapagos_Silky, "Galapagos_Silky.csv", row.names = FALSE)
write.csv(Galapagos_Silvertip, "Galapagos_Silvertip.csv", row.names = FALSE)
write.csv(Galapagos_Whitetip, "Galapagos_Whitetip.csv", row.names = FALSE)
write.csv(Silky_Silvertip, "Silky_Silvertip.csv", row.names = FALSE)
write.csv(Silky_Whitetip, "Silky_Whitetip.csv", row.names = FALSE)
write.csv(Silvertip_Whitetip, "Silvertip_Whitetip.csv", row.names = FALSE)


# Function to run the analysis for each species pair
run_niche_overlap_analysis <- function(species1, species2) {
  # Set the input and output file names based on the species pair
  Data.File <- paste0(species1, "_", species2, "_Matrix.csv")
  Output.File <- paste0(species1, "_", species2, "_Niche_Overlap_Output.txt")
  
  # Set the parameters for the niche overlap analysis
  Algorithm <- "RA3"  # Example algorithm, you can adjust as needed
  Metric <- "Pianka"  # Example metric, you can adjust as needed
  N.Reps <- 5000      # Number of replicates
  Random.Seed <- 625  # Random seed
  Plot.Output <- "file"   # Output plot to a file
  Print.Output <- "file"  # Print output to a file
  Display.About <- "none" # Don't display about information
  Graphic <- "Niche.Overlap.Plot"  # Define plot type
  
  # Get the parameters for the niche overlap calculation
  Param.List <- Get.Params(Data.File, Output.File, Algorithm, Metric,
                           N.Reps, Random.Seed, Plot.Output, Print.Output, Display.About, Graphic)
  
  # Set the random seed for the model
  RandomInteger <- Set.The.Seed(Param.List)
  
  # Run the null model engine
  Null.Result <- Null.Model.Engine(Param.List)
  
  # Output the results
  Output.Results(Param.List, Null.Result)
}

# Create list of species pairs
species_pairs <- list(
  c("Galapagos", "Silky"),
  c("Galapagos", "Silvertip"),
  c("Galapagos", "Whitetip"),
  c("Silky", "Silvertip"),
  c("Silky", "Whitetip"),
  c("Silvertip", "Whitetip")
)


# Loop over each species pair and run the analysis
for (pair in species_pairs) {
  species1 <- pair[1]
  species2 <- pair[2]
  
  # Subset the RawNiche_long data for the current species pair and save as CSV
  pair_data <- RawNiche_long[RawNiche_long$Species %in% c(species1, species2), ]
  pair_file <- paste0(species1, "_", species2, "_Matrix.csv")
  write.csv(pair_data, pair_file, row.names = FALSE)
  
  # Run the niche overlap analysis for the current species pair
  run_niche_overlap_analysis(species1, species2)
}


# Ecosim Tests With Proportions by species + Raw Data ------------------------------------------------------------



# Create a new data frame with species-specific data from raw data

RPdata_subset<-RPdata%>%dplyr::select(Tag.ID,Common.Name,Depth, Detections)%>% rename(Species=Common.Name)

Indiv_depth_use <- RPdata_subset %>%
  filter(Depth < 0) %>%
  mutate(Depths = -as.integer(Depth)) %>%
  
  # Sum detections per individual per depth
  group_by(Tag.ID, Species, Depths) %>%
  summarise(Detections = sum(Detections), .groups = "drop") %>%
  
  # Normalize within individual
  group_by(Tag.ID) %>%
  mutate(
    Total = sum(Detections),
    Prop = Detections / Total
  ) %>%
  ungroup() %>%
  
  # Average across individuals
  group_by(Species, Depths) %>%
  summarise(
    Probability = mean(Prop),
    .groups = "drop"
  )


# Ensure Depths are sorted numerically
Indiv_depth_use$Depths <- factor(Indiv_depth_use$Depths, levels = sort(unique(Indiv_depth_use$Depths)))

#Visualize
prop_bydep <- Indiv_depth_use |>
  mutate(Depths = as.numeric(as.character(Depths))) |>
  ggplot(aes(x = Depths, y = Probability)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0,250,10)
  ) +
  ylab("Probability") +
  facet_wrap(~Species, scales = "free_y") +
  theme_classic()

ggsave("AverageProbability_Depth.jpg", plot = prop_bydep,
       width = 12, height = 10, dpi = 300)


RawNiche_long <- Indiv_depth_use %>%
  pivot_wider(names_from = Depths, values_from = Probability, values_fill = list(Probability = 0))

# Sort the column names numerically by converting to numeric, then back to character for proper ordering
colnames(RawNiche_long)[-1] <- sort(as.numeric(colnames(RawNiche_long)[-1]))
colnames(RawNiche_long)[-1] <- as.character(colnames(RawNiche_long)[-1])

# Reorder columns
RawNiche_long <- RawNiche_long %>%
  dplyr::select(Species, everything())

#create a csv. Depending on your computer, you may want to open it and delete the empty  first column
write.csv(RawNiche_long, "RAWProportionMatrix.csv", row.names = FALSE)


#Niche overlap Overall
Data.File <- "RAWProportionMatrix.csv"
Output.File <-"RAWProportion Niche Overlap Output.txt"
Algorithm <- "RA3"	#choices are "RA1", "RA2", "RA3", "RA4"; default is "RA3"
Metric <- "Pianka"	#choices are "Pianka", "Czekanowski", 
#"Pianka.var", "Czekanowski.var",
# "Pianka.skew", "Czekanowski.skew"; default is Pianka
N.Reps <- 5000	# 1000 is the typical number of replicates, but any number > 2 will run
Random.Seed <- 625 ## If 0, uses random integer. User can replace 0 with your integer of choice e.g. 107
Plot.Output <- "file" 	#choices are "file", "screen", "none"; default is "screen"
Print.Output <- "file"	#choices are "file", "screen", "none"; default is "screen"
Display.About <- "none" # choices are "screen", "none"; default is "none"
Graphic <- "Niche.Overlap.Plot" # other choices will be added with  specified from user inputs

Param.List <- Get.Params(Data.File,Output.File,Algorithm,Metric,
                         N.Reps,Random.Seed,Plot.Output,Print.Output,Display.About,Graphic)
RandomInteger <- Set.The.Seed(Param.List)


Null.Result <- Null.Model.Engine(Param.List)

Output.Results(Param.List,Null.Result)


## SpeciesPairAnalysis -----------------------------------------------------



#Create species pairs for ecosim analysis

Galapagos_Silky <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Silky"), ]
Galapagos_Silvertip <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Silvertip"), ]
Galapagos_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Galapagos", "Whitetip"), ]
Silky_Silvertip <- RawNiche_long[RawNiche_long$Species %in% c("Silky", "Silvertip"), ]
Silky_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Silky", "Whitetip"), ]
Silvertip_Whitetip <- RawNiche_long[RawNiche_long$Species %in% c("Silvertip", "Whitetip"), ]

# Save each matrix as a separate CSV file
write.csv(Galapagos_Silky, "Galapagos_Silky.csv", row.names = FALSE)
write.csv(Galapagos_Silvertip, "Galapagos_Silvertip.csv", row.names = FALSE)
write.csv(Galapagos_Whitetip, "Galapagos_Whitetip.csv", row.names = FALSE)
write.csv(Silky_Silvertip, "Silky_Silvertip.csv", row.names = FALSE)
write.csv(Silky_Whitetip, "Silky_Whitetip.csv", row.names = FALSE)
write.csv(Silvertip_Whitetip, "Silvertip_Whitetip.csv", row.names = FALSE)


# Function to run the analysis for each species pair
run_niche_overlap_analysis <- function(species1, species2) {
  # Set the input and output file names based on the species pair
  Data.File <- paste0(species1, "_", species2, "_Matrix.csv")
  Output.File <- paste0(species1, "_", species2, "_Niche_Overlap_Output.txt")
  
  # Set the parameters for the niche overlap analysis
  Algorithm <- "RA3"  # Example algorithm, you can adjust as needed
  Metric <- "Pianka"  # Example metric, you can adjust as needed
  N.Reps <- 5000      # Number of replicates
  Random.Seed <- 625  # Random seed
  Plot.Output <- "file"   # Output plot to a file
  Print.Output <- "file"  # Print output to a file
  Display.About <- "none" # Don't display about information
  Graphic <- "Niche.Overlap.Plot"  # Define plot type
  
  # Get the parameters for the niche overlap calculation
  Param.List <- Get.Params(Data.File, Output.File, Algorithm, Metric,
                           N.Reps, Random.Seed, Plot.Output, Print.Output, Display.About, Graphic)
  
  # Set the random seed for the model
  RandomInteger <- Set.The.Seed(Param.List)
  
  # Run the null model engine
  Null.Result <- Null.Model.Engine(Param.List)
  
  # Output the results
  Output.Results(Param.List, Null.Result)
}

# Create list of species pairs
species_pairs <- list(
  c("Galapagos", "Silky"),
  c("Galapagos", "Silvertip"),
  c("Galapagos", "Whitetip"),
  c("Silky", "Silvertip"),
  c("Silky", "Whitetip"),
  c("Silvertip", "Whitetip")
)


# Loop over each species pair and run the analysis
for (pair in species_pairs) {
  species1 <- pair[1]
  species2 <- pair[2]
  
  # Subset the RawNiche_long data for the current species pair and save as CSV
  pair_data <- RawNiche_long[RawNiche_long$Species %in% c(species1, species2), ]
  pair_file <- paste0(species1, "_", species2, "_Matrix.csv")
  write.csv(pair_data, pair_file, row.names = FALSE)
  
  # Run the niche overlap analysis for the current species pair
  run_niche_overlap_analysis(species1, species2)
}

