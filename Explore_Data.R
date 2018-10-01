library(readxl)
library(plyr)
library(dplyr)
library(readr)
cogondata <- read_csv("Data/New_Combined.csv")
summary(cogondata)
#3 NA's per plot for tiller counts, biomass, native longest####
cogondata <- cogondata %>% 
  mutate(plotid=paste(pop_id, plot_number,sep = " "))
cogondata$plotid

cogondata$date
?as.Date
as.Date(cogondata$date,format = "%m/%d/%y")
cogondata$date=as.Date(cogondata$date,format = "%m/%d/%y")
n_distinct(cogondata$plotid)
n_distinct(cogondata$pop_id)
as.numeric(cogondata$Denisometer_reading)
cogondata$Denisometer_reading=as.numeric(cogondata$Denisometer_reading)
summary(cogondata)
cogondata <- cogondata %>% 
  mutate(F_Dead=as.numeric(F_Dead),
         F_Live=as.numeric(F_Live),
         F_Other=as.numeric(F_Other),
         F_Litter=as.numeric(F_Litter),
         c_till_longest=rowMeans(cbind(longest1,longest2,longest3,longest4), na.rm=T),
         c_till_all=rowMeans(cbind(tiller1, tiller2, tiller3, tiller4, tiller5, tiller6, tiller7, tiller8, tiller9, tiller10),na.rm=T))
summary(cogondata)
View(cogondata)
plot(cogondata$Denisometer_reading, cogondata$live_tillers)
library(ggplot2)
filter(cogondata,Denisometer_reading>=0) %>% 
  ggplot(aes(Denisometer_reading, live_tillers))+
  geom_point()+
  geom_smooth()+
  theme_classic()

  