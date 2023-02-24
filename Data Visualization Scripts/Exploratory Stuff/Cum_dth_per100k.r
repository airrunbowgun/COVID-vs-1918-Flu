library(tidyverse)
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files/Combined Data Project")
## read in .csv files downloaded from "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/state/missouri"
cov.dat <- read.csv("covid_deaths_usafacts28Sep.csv")
pop.dat <- read.csv("covid_county_population_usafacts.csv")

## remove other state data & standardize FIP
pop.dat <- pop.dat %>% 
  filter(State == "MO" & County.Name != "Statewide Unallocated" ) %>%
  mutate(countyFIPS = substring(as.character(ï..countyFIPS),3))
## Remove state name (MO) and state ID (29)
pop.dat <- pop.dat[,4:5]
## remove other state data & standardize FIP
cov.dat <- cov.dat %>% 
  filter(State == "MO" & County.Name != "Statewide Unallocated" ) %>%
  mutate(countyFIPS = substring(as.character(countyFIPS),3))

## load rurality group designations
urb.rur <- read.csv("urb_rur_2020.csv")
urb.rur <- mutate(urb.rur[,c(1,3)], countyFIPS = substring(as.character(countyFIPS),3))

## combine all data sets
cov.dat <- left_join(cov.dat,urb.rur, by = "countyFIPS")
cov.dat <- left_join(cov.dat,pop.dat, by = "countyFIPS")

## Rearrange columns and remove state name (MO) and state ID (29)
cov.dat <- cov.dat[,c(1,2,(ncol(cov.dat)-1):ncol(cov.dat),5:(ncol(cov.dat)-2))]

## convert to daily counts from cumulative for standardization
daily <- cov.dat
for (i in 6:(ncol(cov.dat))){
  daily[,i] <- cov.dat[,i]-cov.dat[,(i-1)]
}


## Set up for standardizations ## (!!!need to manually comment out the SMR OR /100,000 BELOW!!!!)
E <- matrix(0,nrow = nrow(daily),ncol=1)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
####    daily deaths per 100,000 standardization     # ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
for (i in 1:nrow(daily)){
  E[i] = (100000/daily$population[i])
}
## Standardize daily counts for each county on each day
for (i in 5:ncol(daily)){
  daily[,i] <- daily[,i]*E
}


## calculate average daily standardized counts of each rurality group
cov.ts <- rowsum(daily[,5:ncol(daily)], group = cov.dat$pop.cat)
# number of counties in each population category
n <- rbind(sum(daily$pop.cat=="Rural"), 
           sum(daily$pop.cat=="Semi"),
           sum(daily$pop.cat=="Urban"))
## divide sums by count of counties in a particular group
for (i in 1:ncol(cov.ts)){
  cov.ts[,i] <- cov.ts[,i]/n
}
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
## convert back to cumulative
for (i in 2:ncol(cov.ts)){
  cov.ts[,i]=cov.ts[,i]+cov.ts[i-1]
}

## format and add date for ggplot
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-22"),
                   to = as.Date("2020-01-22")+nrow(cov.ts)-1, by = 1)
## mean SMR for each rurality type
ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="green", "Semi"="red", 
                                 "Urban"="blue"))