library(tidyverse)
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files/Combined Data Project")
## read in .csv files downloaded from "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/state/missouri"
cov.dat <- read.csv("covid_deaths_usafacts06Nov.csv")
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

## cumulative time series for each rurality group
cov.ts <- rowsum(cov.dat[,5:ncol(cov.dat)], group = cov.dat$pop.cat)
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-22"),
                   to = as.Date("2020-01-22")+nrow(cov.ts)-1, by = 1)
## population and death totals
r.pop = as.numeric(sum(cov.dat$population[cov.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(cov.dat$population[cov.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(cov.dat$population[cov.dat$pop.cat=="Urban"]))
d.tot = as.numeric(sum(cov.dat[,ncol(cov.dat)]))
p.tot = as.numeric(sum(cov.dat$population))
## SMR standardize
E.r = (r.pop*d.tot)/(p.tot)
E.s = (s.pop*d.tot)/(p.tot)
E.u = (u.pop*d.tot)/(p.tot)
cov.ts$Rural.SMR <- cov.ts$Rural/E.r
cov.ts$Semi.SMR <- cov.ts$Semi/E.s
cov.ts$Urban.SMR <- cov.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
cov.ts$Rural.k100 <- cov.ts$Rural*k100.r
cov.ts$Semi.k100 <- cov.ts$Semi*k100.s
cov.ts$Urban.k100 <- cov.ts$Urban*k100.u

## Raw Cumulative plot
ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
    scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="green", "Semi"="red", 
                                 "Urban"="blue"))
## SMR plot
ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.SMR, col = "Rural.SMR"))+
  geom_line(aes(y=Semi.SMR, col = "Semi.SMR"))+
  geom_line(aes(y=Urban.SMR, col = "Urban.SMR"))+
  ylab("Cumulative SMR") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
    scale_colour_manual("", 
                      breaks = c("Rural.SMR", "Semi.SMR", "Urban.SMR"),
                      values = c("Rural.SMR"="green", "Semi.SMR"="red", 
                                 "Urban.SMR"="blue"))
## /100,000 plot
ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.k100, col = "Rural.k100"))+
  geom_line(aes(y=Semi.k100, col = "Semi.k100"))+
  geom_line(aes(y=Urban.k100, col = "Urban.k100"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.k100", "Semi.k100", "Urban.k100"),
                      values = c("Rural.k100"="green", "Semi.k100"="red", 
                                 "Urban.k100"="blue"))


### ### ### ### ### ### ### 
##    Cases    #############
### ### ### ### ### ### ### 

## read in .csv files downloaded from "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/state/missouri"
cov.dat2 <- read.csv("covid_cases_usafacts28Sep.csv")
pop.dat <- read.csv("covid_county_population_usafacts.csv")

## remove other state data & standardize FIP
pop.dat <- pop.dat %>% 
  filter(State == "MO" & County.Name != "Statewide Unallocated" ) %>%
  mutate(countyFIPS = substring(as.character(ï..countyFIPS),3))
## Remove state name (MO) and state ID (29)
pop.dat <- pop.dat[,4:5]
## remove other state data & standardize FIP
cov.dat2 <- cov.dat2 %>% 
  filter(State == "MO" & County.Name != "Statewide Unallocated" ) %>%
  mutate(countyFIPS = substring(as.character(countyFIPS),3))

## load rurality group designations
urb.rur <- read.csv("urb_rur_2020.csv")
urb.rur <- mutate(urb.rur[,c(1,3)], countyFIPS = substring(as.character(countyFIPS),3))

## combine all data sets
cov.dat2 <- left_join(cov.dat2,urb.rur, by = "countyFIPS")
cov.dat2 <- left_join(cov.dat2,pop.dat, by = "countyFIPS")

## Rearrange columns and remove state name (MO) and state ID (29)
cov.dat2 <- cov.dat2[,c(1,2,(ncol(cov.dat2)-1):ncol(cov.dat2),5:(ncol(cov.dat2)-2))]

## cumulative time series for each rurality group
cov.ts2 <- rowsum(cov.dat2[,5:ncol(cov.dat2)], group = cov.dat2$pop.cat)
rownames(cov.ts2) <- list("Rural", "Semi", "Urban")
cov.ts2 <- as.data.frame(t(cov.ts2)) 
cov.ts2$date <- seq(from = as.Date("2020-01-22"),
                   to = as.Date("2020-01-22")+nrow(cov.ts)-1, by = 1)
## standardize
r.pop2 = as.numeric(sum(cov.dat2$population[cov.dat2$pop.cat=="Rural"]))
s.pop2 = as.numeric(sum(cov.dat2$population[cov.dat2$pop.cat=="Semi"]))
u.pop2 = as.numeric(sum(cov.dat2$population[cov.dat2$pop.cat=="Urban"]))
d.tot2 = as.numeric(sum(cov.dat2[,ncol(cov.dat2)]))
p.tot2 = as.numeric(sum(cov.dat2$population))
E.r2 = (r.pop2*d.tot2)/(p.tot2)
E.s2 = (s.pop2*d.tot2)/(p.tot2)
E.u2 = (u.pop2*d.tot2)/(p.tot2)
cov.ts2$Rural.Scaled <- cov.ts2$Rural/E.r2
cov.ts2$Semi.Scaled <- cov.ts2$Semi/E.s2
cov.ts2$Urban.Scaled <- cov.ts2$Urban/E.u2


### ### ### ### ### ### ### 
## Misc Cumulative Plots ####
### ### ### ### ### ### ### 
ggplot(cov.ts2,aes(x=date))+
  geom_line(aes(y=Rural.Scaled, col = "Rural.Scaled"))+
  geom_line(aes(y=Semi.Scaled, col = "Semi.Scaled"))+
  geom_line(aes(y=Urban.Scaled, col = "Urban.Scaled"))+
  ylab("Standardized Cumulative Cases") +
  scale_colour_manual("", 
                      breaks = c("Rural.Scaled", "Semi.Scaled", "Urban.Scaled"),
                      values = c("Rural.Scaled"="green", "Semi.Scaled"="red", 
                                 "Urban.Scaled"="blue"))
ggplot(cov.ts2,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Cases") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="green", "Semi"="red", 
                                 "Urban"="blue"))
covComb <- data.frame(date = cov.ts$date,
                      Rural.d = cov.ts$Rural.Scaled,
                      Rural.c = cov.ts2$Rural.Scaled,
                      Semi.d = cov.ts$Semi.Scaled,
                      Semi.c = cov.ts2$Semi.Scaled,
                      Urban.d = cov.ts$Urban.Scaled,
                      Urban.c = cov.ts2$Urban.Scaled)

ggplot(covComb,aes(x=date))+
  geom_line(aes(y=Rural.d, col = "Rural.d"))+
  geom_line(aes(y=Semi.d, col = "Semi.d"))+
  geom_line(aes(y=Urban.d, col = "Urban.d"))+
  geom_line(aes(y=Rural.c, col = "Rural.c"))+
  geom_line(aes(y=Semi.c, col = "Semi.c"))+
  geom_line(aes(y=Urban.c, col = "Urban.c"))+
  ylab("Standardized Cases & Deaths") +
  scale_colour_manual("", 
                      breaks = c("Rural.d", "Semi.d", "Urban.d","Rural.c", "Semi.c", "Urban.c"),
                      values = c("Rural.d"="green", "Semi.d"="red","Urban.d"="blue",
                                 "Rural.c"="black", "Semi.c"="orange","Urban.c"="purple"))


p1 <- ggplot(covComb,aes(x=date))+
  geom_line(aes(y=Rural.d, col = "Rural.d")) +
  geom_line(aes(y=Rural.c, col = "Rural.c")) +
  ylab("Standardized Cases & Deaths")

p2 <- ggplot(covComb,aes(x=date))+
  geom_line(aes(y=Semi.d, col = "Semi.d"))+
  geom_line(aes(y=Semi.c, col = "Semi.c"))+
  ylab("Standardized Cases & Deaths")

p3 <- ggplot(covComb,aes(x=date))+
  geom_line(aes(y=Urban.d, col = "Urban.d"))+
  geom_line(aes(y=Urban.c, col = "Urban.c"))+
  ylab("Standardized Cases & Deaths")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow=3)
