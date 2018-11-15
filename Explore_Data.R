library(readxl)
library(plyr)
library(dplyr)
library(readr)
cogondata <- read_csv("Data/New_Combined.csv")
summary(cogondata)
str(cogondata)

# F_xxx = fresh weight
# D_xxx = dry weight
# 'elevation' is from the GPS unit and should not be used

#3 NA's per plot for tiller counts, biomass, native longest####
cogondata <- cogondata %>% 
  mutate(plotid=paste(pop_id, plot_number,sep = " ")) %>% 
  select(-X)
cogondata$plotid

cogondata$date
# ?as.Date
# as.Date(cogondata$date,format = "%m/%d/%y")
cogondata$date=as.Date(cogondata$date,format = "%m/%d/%y")
n_distinct(cogondata$plotid)
n_distinct(cogondata$pop_id)
as.numeric(cogondata$Denisometer_reading)
cogondata$Denisometer_reading=as.numeric(cogondata$Denisometer_reading)
summary(cogondata)



cdata <- cogondata %>% 
  mutate(F_Dead = as.numeric(F_Dead),
         F_Live = as.numeric(F_Live),
         F_Other = as.numeric(F_Other),
         F_Litter = as.numeric(F_Litter),
         c_till_avg_longest = rowMeans(cbind(longest1,longest2,longest3,longest4), na.rm=T),
         c_till_avg_length = rowMeans(cbind(tiller1, tiller2, tiller3, tiller4, tiller5, tiller6, tiller7, tiller8, tiller9, tiller10),na.rm=T),
         native_avg_longest = rowMeans(cbind(nativelongest1, nativelongest2, nativelongest3, nativelongest4), na.rm = T)
         ) %>% 
  select(date, plotid, pop_number:Denisometer_reading, average_soil_moisture:dead_tillers, c_till_avg_longest, c_till_avg_length, native_avg_longest, F_Dead:F_Litter)

summary(cdata)
dim(cdata)
dim(cogondata)
# View(cogondata)

sort(unique(tolower(cdata$habitat_Type)))
sort(unique(cdata$habitat))

str(cdata)
cdata$treatment_info


# Combine "Dry prairie" & "Open Dry Prairie"
# "Hydric"/"River Edge"/"Roadside"/"Lakefront" = "edge"??
# Combine lakefront, river edge, marsh, hydric hammock as "wet"/"hydric"?? 

cdata <- cdata %>% 
  mutate(
    habitat_Type = tolower(habitat_Type),
    gen_hab_type = case_when(
      habitat_Type == "sandhill" | habitat_Type == "sandhill restoration" ~ "sandhill",
      habitat_Type == "lakefront" | habitat_Type == "river edge" | habitat_Type == "roadside" ~ "edge",
      habitat_Type == "pasture" | habitat_Type == "open dry prairie" | habitat_Type == "dry prairie" ~ "open",
      habitat_Type == "upland pine plantation" | habitat_Type == "pine plantation" ~ "plantation",
      habitat_Type == "hydric hammock" | habitat_Type == "hydric pine" ~ "hydric",
      habitat_Type == "flatwoods/sandhill" | habitat_Type == "flatwoods" ~ "flatwoods",
      TRUE ~ habitat_Type
      ),
    total_tillers = rowSums(cbind(live_tillers, dead_tillers), na.rm = T)
  )

unique(cdata$gen_hab_type)

names(cdata)
str(cdata)

ggplot(cdata, aes(habitat_Type, average_soil_moisture)) +
  geom_point(aes(color = invasion))
ggplot(cdata, aes(gen_hab_type, average_soil_moisture)) +
  geom_point(aes(color = invasion))

cdata %>% 
  filter(total_tillers>0, invasion == "inv") %>% 
  group_by(pop_id) %>% 
  summarise(avg_canopy_cover = mean(Denisometer_reading),
            avg_till_density = mean(total_tillers)) %>% 
ggplot(aes(avg_canopy_cover, avg_till_density)) +
  geom_point(aes()) +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(cdata, aes())

lm1_data <- cdata %>% 
  filter(total_tillers>0, invasion == "inv") %>% 
  group_by(pop_id) %>% 
  summarise(avg_canopy_cover = mean(Denisometer_reading),
            avg_till_density = mean(total_tillers))



lm1 <- lm(avg_till_density ~ avg_canopy_cover, data = lm1_data)
summary(lm1)
plot(lm1)
lm_null <- lm(avg_till_density ~ 1, data = lm1_data)
summary(lm_null)
# anova(lm_null, lm1)
AIC(lm_null)
AIC(lm1)

# notiller_na = filter(cdata, invasion == "inv")
hist(cdata$live_tillers)
hist(cdata$dead_tillers)
hist(cdata$total_tillers)


# plot(cogondata$Denisometer_reading, cogondata$live_tillers)



library(ggplot2)

filter(cdata,Denisometer_reading>0) %>% 
  ggplot(aes(Denisometer_reading, live_tillers))+
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  theme_classic()


