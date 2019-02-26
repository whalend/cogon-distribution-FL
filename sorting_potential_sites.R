##### looking for potential sites

library(readr)
library(dplyr)
library(xlsx)
library(kableExtra
        )

potential_sites <- read_csv("Data/potential_serdp_sites.csv")
summary(potential_sites)

potential_sites %>% 
  filter(invasion == "inv") %>% 
  summary(potential_sites)

unique(potential_sites$pop_id)
unique(potential_sites$habitat_type)

sites_trimmed <- potential_sites %>% 
  mutate(relative_location = pop_id) %>% 
  filter(pop_id %in% c("BoulwareSprings1", "Archer1", "ConePark1", "Reddick1", "Reddick2", "SilverShores1", "SilverShores2", "SilverShores3", "SilverRiver1", "Baseline1", "Baseline2", "Baseline3", "OcalaNF2", "OcalaNF3", "SeminoleSF1", "SeminoleSF2", "ColtCreek1", "ColtCreek2", "ColtCreek3", "PolkCounty1", "PolkCounty2", "LakeWales1", "FH13", "RiversEdge1", "Etoniah1", "HawCreek1", "Tomoka1")) %>% 
  select(pop_id, habitat_type, invasion, avg_total_tillers, relative_location)

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("BoulwareSprings1", "Archer1", "ConePark1")] <- "gainesville"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("Reddick1", "Reddick2")] <- "west reddick"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("SilverShores1", "SilverShores2", "SilverShores3", "SilverRiver1", "Baseline1", "Baseline2", "Baseline3")] <- "ocala"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("OcalaNF2", "OcalaNF3", "SeminoleSF1", "SeminoleSF2")] <- "SE ocala nf alexander springs"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("ColtCreek1", "ColtCreek2", "ColtCreek3", "PolkCounty1", "PolkCounty2", "LakeWales1")] <- "lakeland"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("FH13")] <- "appalachicola nf"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("RiversEdge1")] <- "meridian FL/GA border"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("Etoniah1")] <- "palatka etoniah sf"

sites_trimmed$relative_location[sites_trimmed$pop_id %in% c("HawCreek1", "Tomoka1")] <- "daytona"

unique(sites_trimmed$habitat_type)
unique(sites_trimmed$pop_id)

filter(sites_trimmed, habitat_type == "pine plantation")
filter(sites_trimmed, habitat_type == "upland pine plantation")
filter(sites_trimmed, habitat_type == "sandhill")
filter(sites_trimmed, habitat_type == "flatwoods")
filter(sites_trimmed, habitat_type == "hammock")
filter(sites_trimmed, habitat_type == "mixed forest")

summary(sites_trimmed)

sites_trimmed_inv <- sites_trimmed %>% 
  filter(invasion == "inv")

write_csv(sites_trimmed_inv, "Data/trimmed_potential_sites_inv.csv")

kable(sites_trimmed)
#write_xlxs(sites_trimmed, "Data/trimmed_potential_sites.xlsx", col.names = T, row.names = T)

test <- read_csv("Data/New_Combined.csv") 
View(test)
summary(test)
plot(sites_trimmed)
