setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
load("var2020cl.rdata")
load("var1910cl.rdata")
load("fcd2020.rdata")
load("fcd1910.rdata")
## County shape file
library(maps)
library(maptools)
library(ggplot2)
load("Mo.shape.rdata") 
library(dplyr)


### ### ### ### ### ### ### ###
# Covariate Visualizations ####
### ### ### ### ### ### ### ###

# 1910 Covariate Visuals
library(car)
par(mfrow=c(2,4))
Boxplot(var1910cl$Density,id=list(n=1,labels=var1910cl$NAME), main="Density 1910",ylab = "")
boxplot(var1910cl$Literacy, main="Literacy 1910")
boxplot(var1910cl$Avg.Farm.Val, main="Avg.Farm.Val 1910")
boxplot(var1910cl$PCTMale, main="PCTMale 1910")
# 2020 Covariate Visuals
Boxplot(var2020cl$Density,id=list(n=3,labels=var2020cl$COUNTYFP), main="Density 2019",ylab = "")
boxplot(var2020cl$Literacy, main="Literacy 2019")
boxplot(var2020cl$Avg.Farm.Val, main="Avg.Farm.Val 2019")
boxplot(var2020cl$PCTMale, main="PCTMale 2019")

par(mfrow=c(2,3))
# 1910 Covariate Visuals
boxplot(var1910cl$PCPs100k, main="PCPs100k 1910")
boxplot(var1910cl$YO.Ratio, main="YO.Ratio 1910")
Boxplot(var1910cl$Prop.White,id=list(n=2,labels=var1910cl$NAME), main="Prop.White 1910",ylab = "")
# 2020 Covariate Visuals
boxplot(var2020cl$PCPs100k, main="PCPs100k 2019")
boxplot(var2020cl$YO.Ratio, main="YO.Ratio 2019")
Boxplot(var2020cl$Prop.White,id=list(n=2,labels=var2020cl$NAME), main="Prop.White 2019",ylab = "")

## Covariate Correlations ####
library(corrr)
cmat.2020 <- correlate(var2020cl[,4:11]) %>% rearrange()
cmat.2020  %>%
  shave(upper = FALSE) %>%
  rplot(,print_cor = TRUE)
cmat.1910 <- correlate(var1910cl[,4:10]) %>% rearrange()
cmat.1910  %>%
  shave(upper = FALSE) %>% 
  rplot(,print_cor = TRUE)

### ### ### ### ### ### ### ###
## Response Visualizations ####
### ### ### ### ### ### ### ###
## SMR (SCR2020) Distributions
par(mfrow=c(2,3))
hist(var2020cl$SMR, main = "Histogram of 2020 SMR", xlab="SMR")
hist(var2020cl$SCR, main = "Histogram of 2020 SCR", xlab="SCR")
hist(var1910cl$SMR, main = "Histogram of 1918 SMR", xlab="SMR")
Boxplot(var2020cl$SMR, id=list(n=2,labels=var2020cl$NAME), main = "Boxplot of 2020 SMR", ylab="SMR")
Boxplot(var2020cl$SCR, id=list(n=1,labels=var2020cl$NAME),main = "Boxplot of 2020 SCR", ylab="SCR")
Boxplot(var1910cl$SMR, id=list(n=3,labels=var1910cl$NAME), main = "Boxplot of 1918 SMR", ylab="SMR")


### ### ### ### ### ### ### ### ### ### #
## SMR (& SCR2020) County level maps ####
### ### ### ### ### ### ### ### ### ### #
# COVID - SMR
Mo.shape.full20 <- cbind(Mo.shape,var2020cl)
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = SMR)) +
  geom_sf_text(aes(label = round(SMR,2)), colour = "black") +
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "COVID Standardized Mortality Ratio (SMR)",
       x = "Longitude", y = "Latitude", fill = "SMR")
# 1918 - SMR
Mo.shape.full19 <- cbind(Mo.shape,var1910cl)
ggplot(data = Mo.shape.full19) +
  geom_sf(aes(fill = SMR)) +
  geom_sf_text(aes(label = round(SMR,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Standardized Mortality Ratio (SMR) of the 1918 Flu",
       x = "Longitude", y = "Latitude", fill = "SMR")
# COVID - SCR
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = SCR)) +
  geom_sf_text(aes(label = round(SCR,2)), colour = "black") +
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "COVID Standardized Count Ratio (SCR)",
       x = "Longitude", y = "Latitude", fill = "SCR")



## ### ### ### ### ### ### ### ### #
## 2020 SMR & SCR Linear Models ####
### ### ### ### ### ### ### ### ###

# 1st order models (w/o prison indicator)
modd20lm <- lm(SMR ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp, data = var2020cl)
modc20lm <- lm(SCR ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp, data = var2020cl)
logdmod20lm <- lm(log(SMR) ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp, data = var2020cl)
logcmod20lm <- lm(log(SCR) ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp, data = var2020cl)
summary(modd20lm)
summary(modc20lm)
summary(logdmod20lm)
summary(logcmod20lm)

## partial residual plots to look for higher order possibilities
library(car)
par(mfrow=c(3,3))
crPlot(modd20lm,variable = "Density",main="2020 SMR")
crPlot(modd20lm,variable = "Literacy")
crPlot(modd20lm,variable = "Avg.Farm.Val")
crPlot(modd20lm,variable = "PCTMale")
crPlot(modd20lm,variable = "PCPs100k")
crPlot(modd20lm,variable = "YO.Ratio")
crPlot(modd20lm,variable = "Prop.White")
crPlot(modd20lm,variable = "Prop.Hisp")
par(mfrow=c(3,3))
crPlot(modc20lm,variable = "Density",main="2020 SCR")
crPlot(modc20lm,variable = "Literacy")
crPlot(modc20lm,variable = "Avg.Farm.Val")
crPlot(modc20lm,variable = "PCTMale")
crPlot(modc20lm,variable = "PCPs100k")
crPlot(modc20lm,variable = "YO.Ratio")
crPlot(modc20lm,variable = "Prop.White")
crPlot(modc20lm,variable = "Prop.Hisp")

par(mfrow=c(3,3))
crPlot(logdmod20lm,variable = "Density",main="2020 log(SMR)")
crPlot(logdmod20lm,variable = "Literacy")
crPlot(logdmod20lm,variable = "Avg.Farm.Val")
crPlot(logdmod20lm,variable = "PCTMale")
crPlot(logdmod20lm,variable = "PCPs100k")
crPlot(logdmod20lm,variable = "YO.Ratio")
crPlot(logdmod20lm,variable = "Prop.White")
crPlot(logdmod20lm,variable = "Prop.Hisp")
par(mfrow=c(3,3))
crPlot(logcmod20lm,variable = "Density",main="2020 log(SCR)")
crPlot(logcmod20lm,variable = "Literacy")
crPlot(logcmod20lm,variable = "Avg.Farm.Val")
crPlot(logcmod20lm,variable = "PCTMale")
crPlot(logcmod20lm,variable = "PCPs100k")
crPlot(logcmod20lm,variable = "YO.Ratio")
crPlot(logcmod20lm,variable = "Prop.White")
crPlot(logdmod20lm,variable = "Prop.Hisp")

## 2nd order 2020 models
smodd20lm <- lm(SMR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                  poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T), data = var2020cl)
smodc20lm <- lm(SCR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                  poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T), data = var2020cl)
slogdmod20lm <- lm(log(SMR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                     poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T), data = var2020cl)
slogcmod20lm <- lm(log(SCR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                     poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T), data = var2020cl)
summary(smodd20lm)
summary(smodc20lm)
summary(slogdmod20lm)
summary(slogcmod20lm)

# add prison indicator variable to existing data (from "COVID-19 RAPID Data Collection.xlsx" sheet = 'Prison Data')
## !!!! NOTE !!!! Does NOT include St. Louis City which listed 1 inmate in the population ??? !!!
var2020cl$Prison <- rep(0,115)
var2020cl$Prison[c(4,11,14,26,27,32,36,48,59,67,74,68,82,88,95,107,110,112)] <- 1
##quick check to visually verify the indicator jives with the excel file
var2020cl$NAME[which(var2020cl$Prison==1)]

## Reevaluate 2020 1st order models adding in the prison indicator
modd20lm2 <- lm(SMR ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison, data = var2020cl)
modc20lm2 <- lm(SCR ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison, data = var2020cl)
logdmod20lm2 <- lm(log(SMR) ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison, data = var2020cl)
logcmod20lm2 <- lm(log(SCR) ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison, data = var2020cl)
summary(modd20lm2)
summary(modd20lm)

summary(modc20lm2)
summary(modc20lm)

summary(logdmod20lm2)
summary(logdmod20lm)

summary(logcmod20lm2)
summary(logcmod20lm)

## Reevaluate 2020 2nd order models adding in the prison indicator
## 2nd order 2020 models
smodd20lm2 <- lm(SMR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                  poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)
smodc20lm2 <- lm(SCR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                  poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)
slogdmod20lm2 <- lm(log(SMR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                     poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)
slogcmod20lm2 <- lm(log(SCR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                     poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)
summary(smodd20lm2)
summary(smodd20lm)

summary(smodc20lm2)
summary(smodc20lm)

summary(slogdmod20lm2)
summary(slogdmod20lm)

summary(slogcmod20lm2)
summary(slogcmod20lm)


## ### ### ### ### ### ### ###
## 1918 SMR Linear Models ####
### ### ### ### ### ### ### ##
# 1st order
mod19lm <- lm(SMR ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White, data = var1910cl)
logmod19lm <- lm(log(SMR) ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White, data = var1910cl)

# 2nd order
smod19lm <- lm(SMR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                 poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T), data = var1910cl)
slogmod19lm <- lm(log(SMR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                    poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T), data = var1910cl)
summary(mod19lm)
summary(logmod19lm)
summary(smod19lm)
summary(slogmod19lm)



## all possible regression analyses
library(leaps)
lp20logSMR <- regsubsets(log(SMR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                           poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, 
                         data = var2020cl, nvmax = 17, nbest = 1)
lp20SCR <- regsubsets(SCR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                        poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, 
                      data = var2020cl, nvmax = 17, nbest = 1)
lp19SMR <- regsubsets(SMR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                        poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T), data = var1910cl, nvmax = 17, nbest = 1)
lp19logSMR <- regsubsets(log(SMR) ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                        poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T), data = var1910cl, nvmax = 17, nbest = 1)

## summary of model predictor variants
#2020 log(SMR)
sum20logSMR <- summary(lp20logSMR)
data.frame(
  Adj.R2 = which.max(sum20logSMR$adjr2),
  CP = which.min(sum20logSMR$cp),
  BIC = which.min(sum20logSMR$bic)
)
disp20logSMR <- cbind(sum20logSMR$which,Rsquare=sum20logSMR$rsq,AdjRsquare=sum20logSMR$adjr2,
             Cp=sum20logSMR$cp,BIC=sum20logSMR$bic)
#shorten colums names so I can see the whole thing at once
colnames(disp20logSMR) <- list("I","D1","D2","L1","L2","AF1","AF2","PM1","PM2",
                                 "PCP1","PCP2","YO1","YO2","PW1","PW2","PH1","PH2",
                                 "Pris","Rsq","ARsq","Cp","BIC")
#2020 SCR
sum20SCR <- summary(lp20SCR)
data.frame(
  Adj.R2 = which.max(sum20SCR$adjr2),
  CP = which.min(sum20SCR$cp),
  BIC = which.min(sum20SCR$bic)
)
disp20SCR <- cbind(sum20SCR$which,Rsquare=sum20SCR$rsq,AdjRsquare=sum20SCR$adjr2,
                      Cp=sum20SCR$cp,BIC=sum20SCR$bic)
#shorten colums names so I can see the whole thing at once
colnames(disp20SCR) <- list("I","D1","D2","L1","L2","AF1","AF2","PM1","PM2",
                               "PCP1","PCP2","YO1","YO2","PW1","PW2","PH1","PH2",
                               "Pris","Rsq","ARsq","Cp","BIC")
#1918 SMR
sum19SMR <- summary(lp19SMR)
data.frame(
  Adj.R2 = which.max(sum19SMR$adjr2),
  CP = which.min(sum19SMR$cp),
  BIC = which.min(sum19SMR$bic)
)
disp19SMR <- cbind(sum19SMR$which,Rsquare=sum19SMR$rsq,AdjRsquare=sum19SMR$adjr2,
                   Cp=sum19SMR$cp,BIC=sum19SMR$bic)
#shorten colums names so I can see the whole thing at once
colnames(disp19SMR) <- list("I","D1","D2","L1","L2","AF1","AF2","PM1","PM2",
                            "PCP1","PCP2","YO1","YO2","PW1","PW2",
                            "Rsq","ARsq","Cp","BIC")
#1918 log(SMR)
sum19logSMR <- summary(lp19logSMR)
data.frame(
  Adj.R2 = which.max(sum19logSMR$adjr2),
  CP = which.min(sum19logSMR$cp),
  BIC = which.min(sum19logSMR$bic)
)
disp19logSMR <- cbind(sum19logSMR$which,Rsquare=sum19logSMR$rsq,AdjRsquare=sum19logSMR$adjr2,
                   Cp=sum19logSMR$cp,BIC=sum19logSMR$bic)
#shorten colums names so I can see the whole thing at once
colnames(disp19logSMR) <- list("I","D1","D2","L1","L2","AF1","AF2","PM1","PM2",
                            "PCP1","PCP2","YO1","YO2","PW1","PW2",
                            "Rsq","ARsq","Cp","BIC")

disp20logSMR
disp20SCR
disp19SMR
disp19logSMR

### ### ### ### ### ### ### ##
### LEAPS selected models ####
### ### ### ### ### ### ### ##

#1918 SMR
lp1918SMR <- lm(SMR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(PCTMale,2,raw = T) + 
                  poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T), data = var1910cl)
summary(lp1918SMR)
plot(lp1918SMR)
nortest::ad.test(lp1918SMR$residuals)

#1918 log(SMR)
lp1918logSMR <- lm(log(SMR) ~ poly(Literacy,2,raw = T) + PCPs100k + poly(YO.Ratio,2,raw = T) + 
                     poly(Prop.White,2,raw = T), data = var1910cl)
summary(lp1918logSMR)
plot(lp1918logSMR)
nortest::ad.test(lp1918logSMR$residuals)

#2020 log(SMR)
lp2020logSMR <- lm(log(SMR) ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + poly(PCTMale,2,raw = T) + 
                     YO.Ratio + Prop.Hisp + Prison, data = var2020cl)
summary(lp2020logSMR)
plot(lp2020logSMR)
nortest::ad.test(lp2020logSMR$residuals)

#2020 SCR
lp2020SCR <- lm(SCR ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + PCTMale + 
                  poly(YO.Ratio,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)
summary(lp2020SCR)
plot(lp2020SCR)
nortest::ad.test(lp2020SCR$residuals)

## Simplified names for "Best" linear models
FLU.logSMR <- lp1918logSMR
COVID.logSMR <- lp2020logSMR
COVID.SCR <- lp2020SCR


### ### ### ### ### ### ### ### ### #
## Exploring Spatial Dependence #### 
### ### ### ### ### ### ### ### ### #

## Raw Response Analysis ##
## Geostatistical Visual
library(geoR)
smr.centroid <- as.geodata(cbind(fcd2020$centroid.lon,fcd2020$centroid.lat,fcd2020$SMR))
scr.centroid <- as.geodata(cbind(fcd2020$centroid.lon,fcd2020$centroid.lat,fcd2020$SCR))
smr19.centroid <- as.geodata(cbind(fcd1910$centroid.lon,fcd1910$centroid.lat,fcd1910$SMR))
#SMR and SCR semivariance plots
plot(variog4(smr.centroid), main="4 Directional Semivariance")
plot(variog4(smr19.centroid), main="4 Directional Semivariance")
### SCR 4-Direction Semivariance
plot(variog4(scr.centroid),main="4 Directional Semivariance")

## Areal Tests
library(spdep)
# Neighbor matrices
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.matB <- nb2mat(W.nb, style="B") # Binary neighbor matrix (0 = no shared border and 1 shares border)
W.matNorm <- nb2mat(W.nb, style="W") # Row normalized neighbor (0 = no shared border and rows sum to 1)
W.listB <- nb2listw(W.nb,style = "B") ## these lines create listw objects that the tests want 
W.listNorm <- nb2listw(W.nb,style = "W") ## the tests don't like matrices apparently

## tests with normalized neighbors
mtestSMR20n <- moran.test(var2020cl$SMR,randomisation = F,
                         W.listNorm)
mtestSMR19n <- moran.test(var1910cl$SMR,randomisation = F,
                         W.listNorm)
mtestSCRn <- moran.test(var2020cl$SCR,randomisation = F,
                       W.listNorm)
gtestSMR20n <- geary.test(var2020cl$SMR, W.listNorm)
gtestSMR19n <- geary.test(var1910cl$SMR, W.listNorm)
gtestSCRn <- geary.test(var2020cl$SCR, W.listNorm)
mtestSMR20n
gtestSMR20n
mtestSMR19n
gtestSMR19n
mtestSCRn
gtestSCRn
## tests with binary neighbors
mtestSMR20b <- moran.test(var2020cl$SMR,randomisation = F,
                          W.listB)
mtestSMR19b <- moran.test(var1910cl$SMR,randomisation = F,
                          W.listB)
mtestSCRb <- moran.test(var2020cl$SCR,randomisation = F,
                        W.listB)
gtestSMR20b <- geary.test(var2020cl$SMR, W.listB)
gtestSMR19b <- geary.test(var1910cl$SMR, W.listB)
gtestSCRb <- geary.test(var2020cl$SCR, W.listB)
mtestSMR20b
gtestSMR20b
mtestSMR19b
gtestSMR19b
mtestSCRb
gtestSCRb
## tests with normalized neighbors on log transformed SMRs & SCR
Lvar20 <- mutate(var2020cl, LSMR = log(SMR), LSCR = log(SCR))
Lvar19 <- mutate(var1910cl, LSMR = log(SMR))
mtestLSMR20n <- moran.test(Lvar20$LSMR,randomisation = F,
                          W.listNorm)
mtestLSMR19n <- moran.test(Lvar19$LSMR,randomisation = F,
                          W.listNorm)
mtestLSCRn <- moran.test(Lvar20$LSCR,randomisation = F,
                        W.listNorm)
gtestLSMR20n <- geary.test(Lvar20$LSMR, W.listNorm)
gtestLSMR19n <- geary.test(Lvar19$LSMR, W.listNorm)
gtestLSCRn <- geary.test(Lvar20$LSCR, W.listNorm)
mtestLSMR20n
gtestLSMR20n
mtestLSMR19n
gtestLSMR19n
mtestLSCRn
gtestLSCRn

## Areal Correlations of residuals ####

## Simplified names for "Best" linear models
# FLU.logSMR <- lp1918logSMR
# COVID.logSMR <- lp2020logSMR
# COVID.SCR <- lp2020SCR
mtFLUn <- moran.test(FLU.logSMR$residuals,randomisation = F,
                     W.listNorm)
mtCOVlogSMRn <- moran.test(COVID.logSMR$residuals,randomisation = F,
                           W.listNorm)
mtCOV.SCRn <- moran.test(COVID.SCR$residuals,randomisation = F,
                         W.listNorm)

mtFLUn
mtCOVlogSMRn
mtCOV.SCRn


## BAYSIAN CAR Models ####
## Add a spatial component to the FLU log(SMR) and COVID SCR models (SAR) ####
library(spdep) # for adjacency matrix and lists
## Adjaceney matrices and lists
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.matB <- nb2mat(W.nb, style="B") # Binary neighbor matrix (0 = no shared border and 1 shares border)
W.matNorm <- nb2mat(W.nb, style="W") # Row normalized neighbor (0 = no shared border and rows sum to 1)
W.listB <- nb2listw(W.nb,style = "B") ## listw objects that the tests (and spatialreg) want 
W.listNorm <- nb2listw(W.nb,style = "W") 
W.listS <- nb2listw(W.nb,style = "S")
W.listC <- nb2listw(W.nb,style = "C")
#creates a symmetric proximity list from the normalized neighbor list
W.listNorm.sym <- similar.listw(W.listNorm)


library(coda) # mcmc diagnostics
library(CARBayes) # modeling

## COVID SCR model
SCR.CAR <- S.CARleroux(formula = SCR ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + PCTMale + 
                         poly(YO.Ratio,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, family = "gaussian",
                       W = W.matB, burnin = 10000, n.sample = 100000, thin = 5, data = var2020cl)
SCR.CAR$summary.results
mtestSCR <- moran.test(SCR.CAR$residuals$pearson,randomisation = F,
                       W.listB)

SCR.CAR2 <- S.CARleroux(formula = SCR ~ poly(Density,2,raw = T) + poly(Literacy,2,raw = T) + poly(Avg.Farm.Val,2,raw = T) + poly(PCTMale,2,raw = T) + 
                          poly(PCPs100k,2,raw = T) + poly(YO.Ratio,2,raw = T) + poly(Prop.White,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, family = "gaussian",
                       W = W.matB, burnin = 10000, n.sample = 100000, thin = 5, data = var2020cl)
SCR.CAR2$summary.results


# Flu log(SMR) Model
# FLUlogSMR.CAR <- S.CARleroux(formula = log(SDR) ~ .
#                              ,family = "gaussian",W = W.mat, burnin = 10000, n.sample = 100000, thin = 5, data = flu.mod.data)


