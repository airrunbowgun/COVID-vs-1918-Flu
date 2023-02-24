setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
library(ggplot2)
library(dplyr)
library(tidyr)
## Load polygon shapefile (has the FIP index and I might use it for a later visualization##
load("Mo.shape.rdata")

### ### ### ### ### ## # ### ###
####   Death Count Data     ####
### ### ### ### ### ## # ### ###

## Load the weekly death count data
dth <- read.csv("Flu_2_peaks.csv")
dth[,1] <- Mo.shape$COUNTYFP
names(dth)[names(dth) == 'ï..FIP'] <- 'COUNTYFP'

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
####     Create response and predictor data frame    #####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
Flu.dat.all <- dth[,-1]
Flu.dat.all <- cbind(Mo.shape$COUNTYFP,Flu.dat.all)
names(Flu.dat.all)[names(Flu.dat.all) == 'Mo.shape$COUNTYFP'] <- 'COUNTYFP'

## Load 1918 Urban and Rural Classifications
urb_rur <- t(as.data.frame(readxl::read_excel("Urb_Rur1918.xlsx", range = "B1:DL4", col_names = FALSE)))
## insert Urban/Semi factor
Flu.dat.all <- mutate(Flu.dat.all, Urban = urb_rur[,3], Semi = urb_rur[,4], Name = Mo.shape$NAME)

## convert to long format
Flu.dat.all <- gather(Flu.dat.all,key="date",
                      value="deaths",-COUNTYFP,-Urban,-Semi, -Name) ##convert to long format
## add formatted date, flu season (wave), and categorical population density column
Flu.dat.all <- Flu.dat.all %>% arrange(COUNTYFP)
Flu.dat.all$date <- rep(c(seq(from = as.Date("1918-09-08"), to = as.Date("1918-12-29"), by = 'week'),
                          seq(from = as.Date("1920-01-04"), to = as.Date("1920-03-07"), by = 'week')), 115)
Flu.dat.all <- mutate(Flu.dat.all, wave = ifelse(date < "1918-12-30", 'Wave01','Wave02'), 
                      pop.cat = ifelse(Urban == "1", "Urban", ifelse(Semi == "1", "Semi", "Rural")))


## Record max weekly death counts for each county in wave 1, wave 2, and overall 
## Across time ##
max.overall <- Flu.dat.all %>% 
  group_by(COUNTYFP) %>%
  select(COUNTYFP, Name, pop.cat, deaths) %>%
  slice_max(deaths, n=1, with_ties = FALSE) 
wave1.max <- Flu.dat.all %>% 
  group_by(COUNTYFP) %>% 
  filter(wave == "Wave01") %>%
  select(COUNTYFP, Name, pop.cat, date, wave, deaths) %>%
  slice_max(deaths, n=1, with_ties = FALSE)
wave2.max <- Flu.dat.all %>% 
  group_by(COUNTYFP) %>% 
  filter(wave == "Wave02") %>%
  select(COUNTYFP, Name, pop.cat, date, wave, deaths) %>%
  slice_max(deaths, n=1, with_ties = FALSE)

max.deaths <- cbind(max.overall,wave1.max[,6],wave2.max[,6])
colnames(max.deaths) <- list("COUNTYFP", "Name", "pop.cat", "OverallMax", "Wave1.Max", "Wave2.Max")
max.deaths$ratio <- max.deaths$Wave1.Max/max.deaths$Wave2.Max
max.deaths$ln.ratio <- log(max.deaths$ratio)


library(spdep) # for adjacency matrix, lists, and Moran tests & other
## spatial stuffs
##  gotta load this package before joining the Mo.shape shapefile :-/

## attach shapefile to the SMR data
dth.full <- full_join(Mo.shape, max.deaths, by = "COUNTYFP")
## Weighting/adjacency lists/matrices
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.listNorm <- nb2listw(W.nb,style = "W") 
#creates a symmetric proximity matrix from the normalized neighbor list
## for Bayesian CAR fit
W.listNorm.sym <- similar.listw(W.listNorm)
W.norm <- listw2mat(W.listNorm.sym)


##Spatial Correlation Tests
moran.test(dth.full$ln.ratio,randomisation = TRUE,
           W.listNorm)

lslr <- lm(ln.ratio ~ pop.cat, data = dth.full)
anova(lslr)
moran.test(lslr$residuals,randomisation = TRUE,
           W.listNorm)

## log(ratio) visual comparisons
ggplot(dth.full) +
  geom_boxplot(aes(x=pop.cat, y=ln.ratio)) 

### CAR Models for 1918 vs 1920 rurality comparisons (log ratio max weekly SMR) ####
library(spatialreg)
Car.1 <- spautolm(dth.full$ln.ratio ~ pop.cat, data=dth.full, W.listNorm.sym, family="CAR")
summary(Car.1)
moran.test(Car.1$fit$residuals,randomisation = TRUE,
           W.listNorm)

## Bayesian CAR Model
library(CARBayes)
BCAR <- S.CARleroux(formula = ln.ratio ~ pop.cat, family = "gaussian",
                    W = W.norm, burnin = 10000, n.sample = 100000, thin = 5, data = dth.full)



