---
  title: "Taylor"
output: html_notebook
---
#libraries
library(dplyr)
library(ggplot2)  
  

```{r load data}
# load the dataset
# I had to get rid of the N/As in the densiometer category to make it read it as a number
taylor<-read.csv("Data/New_Combined.csv", header=TRUE)
summary(taylor)

# This takes the average of the soil moisture column grouped by invasion and habitat type 
# N is the number of reps, mean is the mean soil moisture, sd is the standard deviation, se is the standard error
taylor.sm.avg <- plyr::ddply(taylor, c("habitat_Type","invasion"), plyr::summarise,
                             N    = sum(!is.na(average_soil_moisture)),
                             mean = mean(average_soil_moisture, na.rm = T),
                             sd   = sd(average_soil_moisture, na.rm = T),
                             se   = sd / sqrt(N))
# This gets rid of the ones that you haven't entered soil moisture yet
taylor.sm.avg<-filter(taylor.sm.avg, N>0)
# limits2 <- aes(ymax = mean + se, ymin = mean - se)
# graph of the average soil moisture by habitat type and invasion
ggplot(taylor.sm.avg , aes(x=habitat_Type, y=mean, color=invasion)) + 
  geom_errorbar(limits2, color="black", width=0)+
  geom_point(aes(color=invasion), size=4) +
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  ylab(expression ("Soil moisture")) + xlab(expression("Habitat"))+ 
  scale_colour_manual((name="Invasion"), values = c( "#D7191C","#97D0E4")) +theme(legend.position = c(.9, .8))

#another graph just of soil moisture by invasion (i.e. is there higher soil moisture in the invaded areas like with bivens?)
taylor.sm.avg2 <- plyr::ddply(taylor, c("invasion"), plyr::summarise,
                              N    = sum(!is.na(average_soil_moisture)),
                              mean = mean(average_soil_moisture, na.rm = T),
                              sd   = sd(average_soil_moisture, na.rm = T),
                              se   = sd / sqrt(N))

# limits2 <- aes(ymax = mean + se, ymin = mean - se)
ggplot(taylor.sm.avg2 , aes(x=invasion, y=mean, color=invasion)) + 
  geom_errorbar(limits2, color="black", width=0)+
  geom_point(aes(color=invasion), size=4) +
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  ylab(expression ("Soil moisture")) + xlab(expression("Invasion"))
# very slight difference but less than 1% probably not worth thinking about
```


```{r}
# take the mean of the 4 longest columns. names new column longest.avg
taylor<-dplyr::mutate(taylor, longest.avg = (longest1+longest2+longest3+longest4)/4)
# take the mean of the 10 tiller columns. names the new column tiller.avg
taylor<-dplyr::mutate(taylor, tiller.avg = (tiller1+tiller2+tiller3+tiller4+tiller5+tiller6+tiller7+tiller8+tiller9+tiller10)/10)
```

```{r latitude}
# subsets to just the invaded plots
taylor.cogon<-filter(taylor, invasion=="inv")

# figures of latitude vs cogon measurements
ggplot(taylor.cogon , aes(x=latitude, y=tiller.avg)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Latitude")) + ylab(expression("Avg tiller length"))

ggplot(taylor.cogon , aes(x=latitude, y=longest.avg)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Latitude")) + ylab(expression("Longest tiller length"))

ggplot(taylor.cogon , aes(x=latitude, y=live_tillers)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Latitude")) + ylab(expression("Number of live tillers"))

ggplot(taylor.cogon , aes(x=latitude, y=D_Live)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Latitude")) + ylab(expression("Live cogon biomass"))

```


```{r soil moisture}
# figures of soil moisture and cogon measurements
ggplot(taylor.cogon , aes(x=average_soil_moisture, y=live_tillers)) + 
  geom_point(aes(color=habitat_Type),size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Soil moisture")) + ylab(expression("Number of live tillers"))

ggplot(taylor.cogon , aes(x=average_soil_moisture, y=longest.avg)) + 
  geom_point(aes(color=habitat_Type),size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Soil moisture")) + ylab(expression("Longest tiller length"))

ggplot(taylor.cogon , aes(x=average_soil_moisture, y=tiller.avg)) + 
  geom_point(aes(color=habitat_Type),size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Soil moisture")) + ylab(expression("Avg tiller length"))

ggplot(taylor.cogon , aes(x=average_soil_moisture, y=D_Live)) + 
  geom_point(aes(color=habitat_Type),size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Soil moisture")) + ylab(expression("Live cogon biomass"))

```

```{r densiometer}
# figure of canopy cover vs cogon measurements 
ggplot(taylor.cogon , aes(x=Denisometer_reading, y=tiller.avg)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Canopy cover")) + ylab(expression("Avg tiller length"))

ggplot(taylor.cogon , aes(x=Denisometer_reading, y=longest.avg)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Canopy cover")) + ylab(expression("Longest tiller length"))

ggplot(taylor.cogon , aes(x=Denisometer_reading, y=live_tillers)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Canopy cover")) + ylab(expression("Number of live tillers"))

ggplot(taylor.cogon , aes(x=Denisometer_reading, y=D_Live)) + 
  geom_point(aes(color=habitat_Type), size=2) + geom_smooth(method=lm)+
  theme_bw() + theme(panel.grid.major=element_blank())+ theme(panel.grid.minor=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90),
        axis.text.y = element_text(size=12)) +
  theme(axis.title.x = element_text(size = rel(1.3)),
        axis.text.x = element_text(size=13, angle = 45, vjust=.65),
        legend.text=element_text(size = 13),
        legend.title=element_text(size = 15)) + 
  xlab(expression ("Canopy cover")) + ylab(expression("Live cogon biomass"))
```

```{r models}
require(nlme)
# linear model of soil moisture, habitat type, and latitude vs number of live tillers
m1<-lm(live_tillers~ average_soil_moisture+habitat_Type+latitude,  data=taylor.cogon, na.action=na.omit)
anova(m1)
# same but with a mixed model accounting for population
m1<-lme(live_tillers~ average_soil_moisture+habitat_Type+latitude, 
        random = ~ 1|pop_id, data=taylor.cogon, na.action=na.omit)
anova(m1)
# linear model of soil moisture, habitat type, and latitude vs longest tiller length
m1<-lm(longest.avg~ average_soil_moisture+habitat_Type+latitude,  data=taylor.cogon, na.action=na.omit)
anova(m1)
# same but with a mixed model accounting for population
m1<-nlme::lme(longest.avg~ average_soil_moisture+habitat_Type+latitude, 
        random = ~ 1|pop_id, data=taylor.cogon, na.action=na.omit)
anova(m1)
# linear model of soil moisture, habitat type, and latitude vs average tiller length
m1<-lm(tiller.avg~ average_soil_moisture+habitat_Type+latitude,  data=taylor.cogon, na.action=na.omit)
anova(m1)
# same but with a mixed model accounting for population
m1<-nlme::lme(tiller.avg~ average_soil_moisture+habitat_Type+latitude, 
        random = ~ 1|pop_id, data=taylor.cogon, na.action=na.omit)
anova(m1)
#linear model of soil moisture, habitat type, and latitude vs live biomass
m1<-lm(D_Live~ average_soil_moisture+habitat_Type+latitude,  data=taylor.cogon, na.action=na.omit)
anova(m1)
# same but with a mixed model accounting for population
m1<-nlme::lme(D_Live~ average_soil_moisture+habitat_Type+latitude, 
        random = ~ 1|pop_id, data=taylor.cogon, na.action=na.omit)
anova(m1)

```
