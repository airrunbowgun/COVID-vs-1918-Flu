setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
library(ggplot2)
library(gganimate)
library(transformr)
library(dplyr)
library(tidyr)
library(lubridate)
## Load polygon shapefile ##
load("Mo.shape.rdata")

### ### ### ### ### ### ### ### ### ###
####     MONTHLY ANIMATIONS        ####
### ### ### ### ### ### ### ### ### ### 
## Note that the .csv file was manually manipulated to produce monthly aggregated
##  counts (because it was easier than fighting R to do it here)

##Load and format monthly death counts (entire series)
dth <- read.csv("monthly_counts.csv")
dth[,1] <- Mo.shape$COUNTYFP
names(dth)[names(dth) == 'ï..FIP'] <- 'COUNTYFP'
dth2 <- gather(dth,key="date",
               value="deaths",-COUNTYFP) ##convert to long format
dth2$deaths <- as.integer(dth2$deaths)

## Join the shape file to the count data
fcd1910 <- full_join(Mo.shape, dth2, by = "COUNTYFP")
## Formatting for the date
fcd1910$date <- rep(seq(from = as.Date("1918-02-01"), to = as.Date("1921-01-01"), by = 'month'), 115)
## Calculate cumulative monthly deaths
fcd1910 <- fcd1910 %>%
  group_by(COUNTYFP) %>%
  mutate("cm_dth" = cumsum(deaths))

## Month by month count animation
counts <- ggplot(data = fcd1910) +
  geom_sf(aes(fill = deaths)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Flu Deaths Month of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 

## Month by month cumulative count animation
cu_count <- ggplot(data = fcd1910) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, state_length = 0.5, wrap = F) +
  labs(title = "Cumulative Flu Deaths: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 

## Save gif animation files
anim_save(filename = "Flu Monthly Death Counts.gif", counts)
anim_save(filename = "Flu Monthly Cumulative Death Counts.gif", cu_count)

## SMR animations
## var1910cl has county population totals
load("var1910cl.rdata")
## initialize data frame
smr.1918 <- as.data.frame(matrix(NA,nrow = 115,ncol = 37 ))
## Calculate SMR for each county for each month
for (i in 2:37){
smr.1918[,i] <- dth[,i]/(var1910cl$TotalPop * sum(dth[,i])/sum(var1910cl$TotalPop))
}
## Attach county FIPs
smr.1918[,1] <- Mo.shape$COUNTYFP
names(smr.1918)[names(smr.1918) == 'V1'] <- 'COUNTYFP'
smr.1918 <- gather(smr.1918,key="date",
               value="SMR",-COUNTYFP) ##convert to long format
## Join the shape file to the smr data
smr.1918 <- full_join(Mo.shape, smr.1918, by = "COUNTYFP")
## Formatting for the date
smr.1918$date <- rep(seq(from = as.Date("1918-02-01"), to = as.Date("1921-01-01"), by = 'month'), 115)

## Month by month SMR animation
anim.smr.mth <- ggplot(data = smr.1918) +
  geom_sf(aes(fill = SMR)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "1918 Flu Monthly SMR: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "SMR") 

## Save gif animation files
anim_save(filename = "Flu Monthly SMR.gif", anim.smr.mth)


### ### ### ### ### ### ### ### ### ###
####     WEEKLY ANIMATIONS        ####
### ### ### ### ### ### ### ### ### ###
## NOTE: There are 157 weeks and only 50 weeks would render at a time so I skipped the first 7 weeks
##    in order to produce 3 50-week gifs (week 8 = 2/17/1918 to week 57 = 1/26/1919)
##                                        week 58 = 2/2/1919 to week 107 = 1/11/1920)
##                                        week 108 = 1/18/1920 to week 157 = 12/26/1920)

## Load polygon shapefile ##
load("Mo.shape.rdata")

## Load the weekly death count data
dth <- read.csv("flu_wk_dth_1918to1921.csv")
dth[,1] <- Mo.shape$COUNTYFP
names(dth)[names(dth) == 'ï..FIP'] <- 'COUNTYFP'
dth2 <- gather(dth,key="date",
               value="deaths",-COUNTYFP) ##convert to long format
## Join the shape file to the count data
fcd1910 <- full_join(Mo.shape, dth2, by = "COUNTYFP")
## Formatting for the date
fcd1910$date <- rep(seq(from = as.Date("1917-12-30"), to = as.Date("1920-12-26"), by = 'week'), 115)
## Calculate cumulative monthly deaths
fcd1910 <- fcd1910 %>%
  group_by(COUNTYFP) %>%
  mutate("cm_dth" = cumsum(deaths))

## 1st 50 week cumulative count animation
## (week 8 = 2/17/1918 to week 57 = 1/26/1919)
anime.1 <- fcd1910[fcd1910$date>"1918-02-16" & fcd1910$date<"1919-01-27",]
anime.w1 <- ggplot(data = anime.1) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 01.gif", anime.w1)

## 2nd 50 week cumulative count animation
## (week 58 = 2/2/1919 to week 107 = 1/11/1920)
anime.2 <- fcd1910[fcd1910$date>"1919-01-27" & fcd1910$date<"1920-01-12",]
anime.w2 <- ggplot(data = anime.2) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 02.gif", anime.w2)

## 3rd 50 week cumulative count animation
## (week 108 = 1/18/1920 to week 157 = 12/26/1920)
anime.3 <- fcd1910[fcd1910$date>"1920-01-17",]
anime.w3 <- ggplot(data = anime.3) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 03.gif", anime.w3)







### ### ### ### ### ### ### ### ### ###
####    SMR overall standardized WEEKLY ANIMATIONS        ####
### ### ### ### ### ### ### ### ### ###
## NOTE: Only 50 weeks would render at a time so I Created 2 50-week gifs 
##       roughly corresponding to flu season(wave 1 = 09/08/1918 to 08/17/1919)
##                                           wave 2 = 09/07/1919 to 08/15/1920)


## Load polygon shapefile ##
load("Mo.shape.rdata")

## Load the weekly death count data
dth <- read.csv("flu_wk_dth_1918to1921.csv")
dth[,1] <- Mo.shape$COUNTYFP
names(dth)[names(dth) == 'ï..FIP'] <- 'COUNTYFP'
## total number of deaths over the 104 weeks
dtot <- sum(dth[,-1])

## var1910cl has county population totals
load("var1910cl.rdata")

### ### ### ### ### ### ### ### ### ###
## E standardize across all time ######
### ### ### ### ### ### ### ### ### ###
E_all <- as.data.frame(matrix(NA,nrow = 115,ncol = 104 ))
## Calculate E for each county for each week
for (i in 1:104){
  E_all[,i] <- (var1910cl$TotalPop * dtot/(104*sum(var1910cl$TotalPop)))
}
dth$`Mo.shape$COUNTYFP`
dth <- cbind(Mo.shape$COUNTYFP,dth[,-1]/E_all)
names(dth)[names(dth) == "Mo.shape$COUNTYFP"] <- 'COUNTYFP'
dth2 <- gather(dth,key="date",
               value="SMR",-COUNTYFP) ##convert to long format
## Join the shape file to the count data
fcd1910 <- full_join(Mo.shape, dth2, by = "COUNTYFP")
## Formatting for the date
fcd1910$date <- rep(seq(from = as.Date("1918-09-08"), to = as.Date("1920-08-29"), by = 'week'), 115)
fctest <- pivot_wider(fcd1910, names_from = COUNTYFP)


## Overall Standardization for each wave
E_w01 <- as.data.frame(matrix(NA,nrow = 115,ncol = 104 ))
## Calculate E for each county for each week
for (i in 1:104){
  E_all[,i] <- (var1910cl$TotalPop * dtot/(104*sum(var1910cl$TotalPop)))
}


## 1st 50 week cumulative count animation
## (week 8 = 2/17/1918 to week 57 = 1/26/1919)
anime.1 <- fcd1910[fcd1910$date>"1918-02-16" & fcd1910$date<"1919-01-27",]
anime.w1 <- ggplot(data = anime.1) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 01.gif", anime.w1)

## 2nd 50 week cumulative count animation
## (week 58 = 2/2/1919 to week 107 = 1/11/1920)
anime.2 <- fcd1910[fcd1910$date>"1919-01-27" & fcd1910$date<"1920-01-12",]
anime.w2 <- ggplot(data = anime.2) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 02.gif", anime.w2)

## 3rd 50 week cumulative count animation
## (week 108 = 1/18/1920 to week 157 = 12/26/1920)
anime.3 <- fcd1910[fcd1910$date>"1920-01-17",]
anime.w3 <- ggplot(data = anime.3) +
  geom_sf(aes(fill = cm_dth)) +
  scale_fill_gradient(low="white", high="red") +
  transition_states(date, wrap = F) +
  labs(title = "Cumulative Flu Deaths Week of: {previous_state}",
       x = "Longitude", y = "Latitude", fill = "Deaths") 
## save the gif animation
anim_save(filename = "Flu Weekly Cumulative Counts 03.gif", anime.w3)