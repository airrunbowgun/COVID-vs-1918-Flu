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
## Standardize daily counts for each county across time
total.deaths <- sum(daily[,5:ncol(daily)])
total.days <- ncol(daily)-4
state.pop <- sum(daily$population)
E <- matrix(0,nrow = nrow(daily),ncol=1)
## 115 standardizations (1 for each county)
for (i in 1:nrow(daily)){
  E[i] = (total.deaths/total.days)*(daily$population[i]/state.pop)
}
## Standardize daily counts for each county on each day
for (i in 5:ncol(daily)){
  daily[,i] <- daily[,i]/E
}

## format and add date for ggplot
cov.county.ts <- as.data.frame(t(daily[,5:ncol(daily)])) 
cov.county.ts$date <- seq(from = as.Date("2020-01-22"),
                   to = as.Date("2020-01-22")+nrow(cov.county.ts)-1, by = 1)
## graph each county daily SMR
library(scales)
for (i in 1:(ncol(cov.county.ts)-1)){
  graph <- ggplot(cov.county.ts, aes(x = date, y = cov.county.ts[,i])) +
    geom_line(size = .75, linetype = 1, color = "red", alpha = .75)+
    scale_x_date(breaks="month", labels=date_format("%b"))+
    ggtitle("7-Day RA COVID Deaths/100,00 for Missouri Statewide")+
    xlab("Date") + ylab("Average Deaths/100,000") +
    ggtitle(paste("COVID SMR for ", daily$County.Name[i], sep = ""))
  plot(graph)
}

## Cumulative SMR for each county
cov.county.ts.cum <- daily
for (i in 5:ncol(cov.county.ts.cum)){
  cov.county.ts.cum[,i] <-  cov.county.ts.cum[,i]+cov.county.ts.cum[,i-1]
}

## format and add date for ggplot
cov.county.ts.cum <- as.data.frame(t(cov.county.ts.cum[,5:ncol(cov.county.ts.cum)])) 
cov.county.ts.cum$date <- seq(from = as.Date("2020-01-22"),
                          to = as.Date("2020-01-22")+nrow(cov.county.ts.cum)-1, by = 1)
## graph each county cumulative SMR
library(scales)
for (i in 1:(ncol(cov.county.ts.cum)-1)){
  graph <- ggplot(cov.county.ts.cum, aes(x = date, y = cov.county.ts.cum[,i])) +
    geom_line(size = .75, linetype = 1, color = "red", alpha = .75)+
    scale_x_date(breaks="month", labels=date_format("%b"))+
    ggtitle("7-Day RA COVID Deaths/100,00 for Missouri Statewide")+
    xlab("Date") + ylab("Average Deaths/100,000") +
    ggtitle(paste("COVID SMR for ", daily$County.Name[i], sep = ""))
  plot(graph)
}
  
## calculate average daily SMRs of each rurality group
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
  ylab("Cumulative SMR") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="green", "Semi"="red", 
                                 "Urban"="blue"))