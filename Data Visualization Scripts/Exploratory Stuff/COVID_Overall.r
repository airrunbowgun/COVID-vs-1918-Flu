setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
library(dplyr)
library(tidyr)
C.dth <- read.csv("covid_deaths_03AUG.csv")

## 14 day (start date = 2/18/20)
cts2 <- C.dth[,-(1:16)]
cot2 <- as.data.frame(matrix(NA,ncol = 39,nrow = 115))
cot2[,1] <- cts2[,14] - cts2[,1]
for (i in 2:39){
  cot2[,i] <- cts2[,14*i] - cts2[,14*(i-1)]
}
which(cot2 < 0)

## 28 day 
cts3 <- C.dth[,-(1:30)]
cot3 <- as.data.frame(matrix(NA,ncol = 19,nrow = 115))
cot3[,1] <- cts3[,28] - cts3[,1]
for (i in 2:19){
  cot3[,i] <- cts3[,28*i] - cts3[,28*(i-1)]
}
which(cot3 < 0)

### ### ### ### ### ### ### ### ### ###
## Single E to standardize across all 14 day time periods (39 of them) ######
### ### ### ### ### ### ### ### ### ###
## 9967 total COVID deaths ##
dtot <- sum(cot2)
## County data file (for population numbers)
load(file = "var2020cl.rdata")
## Initialize data frame to house the standardizations
E <- as.data.frame(matrix(NA, nrow = 115, ncol = 39 ))
## Calculate E for each county for each week
for (i in 1:39){
  E[,i] <- (var2020cl$TotalPop * dtot/(39*sum(var2020cl$TotalPop)))
}

cot2.SMR <- cot2/E

## recombine aggregated counts with the originally loaded data frame
## Keeping a counts data frame JIC
Covid.counts <- cbind(C.dth[,1:3],cot2)
Covid.counts$COUNTYFP <- substring(as.character(C.dth$countyFIPS),3)
Covid.counts <- Covid.counts[,-1]
## SMR data frame for analysis
Covid.SMR <- cbind(C.dth[,1:3], cot2.SMR)
Covid.SMR$COUNTYFP <- substring(as.character(C.dth$countyFIPS),3)
Covid.SMR <- Covid.SMR[,-1]

Covid.SMR <- gather(Covid.SMR,key="date",
                    value="SMR",-COUNTYFP, -County.Name, -pop.cat) ##convert to long format
Covid.SMR <- Covid.SMR %>% arrange(COUNTYFP)
Covid.SMR$date <- rep(seq(from = as.Date("2020-02-17"), 
                          to = as.Date("2021-08-02"), by = '14 day'), 115)

## Checking on the negatives
Covid.SMR[which(Covid.SMR$SMR<0),]
## Focus on the peak dates ~1 Sep 2020 to ~15 Apr 2021 
## Seventeen, 14-day periods from 08/31 to 04/12 inclusive
Covid.SMR.pk <- Covid.SMR %>% filter(date >"2020-08-29" & date < "2021-04-16")

### ### ### ### ### ### ### ### ### ###
###  Visual Exploration ###############
### ### ### ### ### ### ### ### ### ###
## MEAN SMR Time Series visual
rural.ts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Rural") %>%
  group_by(date) %>%
  summarise(mean(SMR))
semi.ts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Semi") %>%
  group_by(date) %>%
  summarise(mean(SMR))
urban.ts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Urban") %>%
  group_by(date) %>%
  summarise(mean(SMR))

ts.all <- rbind(rural.ts,semi.ts,urban.ts)  
ts.all$pop.cat <- c(rep("rural",17),rep("semi",17),rep("urban",17))

## MEDIAN SMR Time Series visual
rural.mts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Rural") %>%
  group_by(date) %>%
  summarise(median(SMR))
semi.mts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Semi") %>%
  group_by(date) %>%
  summarise(median(SMR))
urban.mts <- Covid.SMR.pk %>% 
  filter(pop.cat == "Urban") %>%
  group_by(date) %>%
  summarise(median(SMR))

mts.all <- rbind(rural.mts,semi.mts,urban.mts)  
mts.all$pop.cat <- c(rep("rural",17),rep("semi",17),rep("urban",17))

library(ggplot2)
t1 <- ggplot(ts.all) +
  geom_line(aes(x = date, y = `mean(SMR)`, col = pop.cat)) +
  ggtitle(label = "Covid 14 Day Mean SMR") 
t2 <- ggplot(mts.all) +
  geom_line(aes(x = date, y = `median(SMR)`, col = pop.cat)) +
  ggtitle(label = "Covid 14 Day Median SMR") 
library(gridExtra)
grid.arrange(t1,t2,nrow = 2)


## Peak period visual (SMR)
ggplot(Covid.SMR.pk) +
  geom_boxplot(aes(x = pop.cat, y = SMR))
## Log(SMR)
ggplot(Covid.SMR.pk) +
  geom_boxplot(aes(x = pop.cat, y = log(SMR)))
## Sqrt(SMR)
ggplot(Covid.SMR.pk) +
  geom_boxplot(aes(x = pop.cat, y = sqrt(SMR)))

## time periods with zero median SMR
rural.mts[which(rural.mts$'median(SMR)'==0),]
semi.mts[which(semi.mts$'median(SMR)'==0),]
urban.mts[which(urban.mts$'median(SMR)'==0),]


## Parametric test on SMR differences (invalid due to normality)
covidanova <- aov(SMR ~ pop.cat,data = Covid.SMR.pk)
anova(covidanova)
TukeyHSD(covidanova)


## Nonparametric test on SMR differences associated with rurality
kruskal.test(Covid.SMR.pk$SMR,Covid.SMR.pk$pop.cat)
library(FSA)
dunnTest(SMR~pop.cat,data = Covid.SMR.pk)

## narrow the time span to remove zero weeks from the rural designation
Covid.SMR.zero <- Covid.SMR %>% filter(date >"2020-10-26" & date < "2021-04-19")
kruskal.test(Covid.SMR.zero$SMR,Covid.SMR.zero$pop.cat)
dunnTest(SMR~pop.cat,data = Covid.SMR.zero)
## Peak period visual (SMR)
ggplot(Covid.SMR.zero) +
  geom_boxplot(aes(x = pop.cat, y = SMR))










############################ Copied stuff below here ###################



















## Analysis of Maximum SMR for each county ####
## Record max SMR for each county 
SMR.max <- Covid.SMR.pk %>% 
  group_by(COUNTYFP) %>%
  select(COUNTYFP, County.Name, pop.cat, SMR ) %>%
  slice_max(SMR, n=1, with_ties = FALSE) 
colnames(SMR.max) <- list("COUNTYFP", "Name", "pop.cat", "Max.SMR")
SMR.max <- mutate(SMR.max, log.Max.SMR = log(Max.SMR))

## Spatial Correlation Investigation
library(spdep) # for adjacency matrix, lists, and Moran tests & other
## spatial stuffs
##  gotta load this package before joining the Mo.shape shapefile :-/

## attach shapefile to the SMR data
load(file = "Mo.shape.rdata")
SMRfull <- full_join(Mo.shape, SMR.max, by = "COUNTYFP")

## Weighting/adjacency lists/matrices
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.listNorm <- nb2listw(W.nb,style = "W") 
#creates a symmetric proximity matrix from the normalized neighbor list
## for Bayesian CAR fit
W.listNorm.sym <- similar.listw(W.listNorm)
W.norm <- listw2mat(W.listNorm.sym)

##Spatial Correlation & Normallity Tests
moran.test(SMRfull$Max.SMR,randomisation = TRUE,
           W.listNorm)
nortest::ad.test(SMRfull$Max.SMR)
moran.test(SMRfull$log.Max.SMR,randomisation = TRUE,
           W.listNorm)
nortest::ad.test(SMRfull$log.Max.SMR)

## Peak period visual (SMR)
ggplot(SMR.max) +
  geom_boxplot(aes(x = pop.cat, y = Max.SMR))
## Log(SMR)
ggplot(SMR.max) +
  geom_boxplot(aes(x = pop.cat, y = log.Max.SMR))

## Basic Linear Models
slr <- lm(log.Max.SMR ~ pop.cat, data = SMRfull)
anova(slr)
slr2 <- aov(log.Max.SMR ~ pop.cat, data = SMRfull)
TukeyHSD(slr2)

## CAR ANOVA tests on Rurality ####
## log(max 14 day SMR) ####
library(spatialreg)
Car.1 <- spautolm(SMRfull$log.Max.SMR ~ pop.cat, data=SMRfull, W.listNorm.sym, family="CAR")
summary(Car.1)
moran.test(Car.1$fit$residuals,randomisation = TRUE,
           W.listNorm)
nortest::ad.test(Car.1$fit$residuals)

## Bayesian CAR Model
library(CARBayes)
BCAR <- S.CARleroux(formula = log.Max.SMR ~ pop.cat, family = "gaussian",
                    W = W.norm, burnin = 10000, n.sample = 100000, thin = 5, data = SMRfull)

## residual analysis
moran.test(BCAR$residuals$pearson,randomisation = TRUE,
           W.listNorm)
nortest::ad.test(BCAR$residuals$pearson)
rplt <- data.frame(pop.cat = SMRfull$pop.cat,
                   res = BCAR$residuals$pearson)
ggplot(rplt) +
  geom_boxplot(aes(x = pop.cat,y = res))


## used for the .rmd file presentation
# sum <- summary(Car.1)
# cmlsum2 <- sum$Coef
# (exp(BCAR$summary.results[1:3,1:3])-1)*100
# save(SMRfull, BCAR, cmlsum2, file = "bcar2.rdata")
# 