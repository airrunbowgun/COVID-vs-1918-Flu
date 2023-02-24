# Variables saved from data_import.R
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
load("var2020cl.rdata")
load("var1910cl.rdata")
load("fcd2020.rdata")
load("fcd1910.rdata")
## County shape file
load("Mo.shape.rdata") 

# add prison indicator variable to existing data (from "COVID-19 RAPID Data Collection.xlsx" sheet = 'Prison Data')
## !!!! NOTE !!!! Does NOT include St. Louis City which listed 1 inmate in the population ??? !!!
var2020cl$Prison <- rep(0,115)
var2020cl$Prison[c(4,11,14,26,27,32,36,48,59,67,74,68,82,88,95,107,110,112)] <- 1

library(maps)
library(maptools)
library(ggplot2)
library(dplyr)

#1918 log(SMR)
FLUlogSMR <- lm(log(SMR) ~ poly(Literacy,2,raw = T) + PCPs100k + poly(YO.Ratio,2,raw = T) + 
                     poly(Prop.White,2,raw = T), data = var1910cl)
#2020 log(SMR)
COVIDlogSMR <- lm(log(SMR) ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + poly(PCTMale,2,raw = T) + 
                     YO.Ratio + Prop.Hisp + Prison, data = var2020cl)
#2020 SCR
COVID.SCR <- lm(SCR ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + PCTMale + 
                  poly(YO.Ratio,2,raw = T) + poly(Prop.Hisp,2,raw = T) + Prison, data = var2020cl)


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

library(spatialreg)
#1918 log(SMR) SAR
FLUlogSMR.SAR <- errorsarlm(log(SMR) ~ poly(Literacy,2,raw = T) + PCPs100k + poly(YO.Ratio,2,raw = T) + 
                            poly(Prop.White,2,raw = T), data = var1910cl, W.listC)
summary(FLUlogSMR.SAR)
mtestFLU <- moran.test(FLUlogSMR.SAR$residuals,randomisation = F,
                       W.listC)
mtestFLU
nortest::ad.test(FLUlogSMR.SAR$residuals) ## Still not passing norm test w/ any type of prox matrix ##

#2020 SCR SAR 
## !!!!!!!(removed Avg.Farm.Val due to singularity issues)!!!!!!!!!
COVID.SCR.SAR <- errorsarlm(SCR ~ poly(Literacy,2,raw = T) + PCTMale +
                               poly(YO.Ratio,2,raw = T) + poly(Prop.Hisp,2,raw = T) + 
                               Prison, data = var2020cl, W.listB)

summary(COVID.SCR.SAR)
mtestCOVID.SCR.SAR <- moran.test(COVID.SCR.SAR$residuals,randomisation = F,
                       W.listNorm)
mtestCOVID.SCR.SAR
nortest::ad.test(COVID.SCR.SAR$residuals)


## Prediction imagery ####

# 2020 COVID SCR SAR
Mo.shape.full20 <- cbind(Mo.shape,COVID.SCR.SAR$fitted.values)
Mo.shape.full20 <- cbind(Mo.shape.full20,COVID.SCR.SAR$residuals)
par(mfrow=c(1,3))
#Predictions
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = COVID.SCR.SAR.fitted.values)) +
  geom_sf_text(aes(label = round(COVID.SCR.SAR.fitted.values,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Predicted COVID SCR",
       x = "Longitude", y = "Latitude", fill = "Predicted SCR")
# Observed SCR
ggplot(data = fcd2020) +
  geom_sf(aes(fill = SCR)) +
  geom_sf_text(aes(label = round(SCR,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Observed COVID SCR",
       x = "Longitude", y = "Latitude", fill = "SCR")
# Standardized Residuals
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = COVID.SCR.SAR.residuals)) +
  geom_sf_text(aes(label = round(COVID.SCR.SAR.residuals,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Residuals COVID SCR",
       x = "Longitude", y = "Latitude", fill = "Standardized Residual")

# 2020 COVID log(SMR)
Mo.shape.full20 <- cbind(Mo.shape,COVIDlogSMR$fitted.values)
Mo.shape.full20 <- cbind(Mo.shape.full20,COVIDlogSMR$residuals)
#Predictions
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = COVIDlogSMR.fitted.values)) +
  geom_sf_text(aes(label = round(COVIDlogSMR.fitted.values,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Predicted COVID log(SMR)",
       x = "Longitude", y = "Latitude", fill = "Predicted log(SMR)")
# Observed log(SMR)
ggplot(data = fcd2020) +
  geom_sf(aes(fill = log(SMR))) +
  geom_sf_text(aes(label = round(log(SMR),2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Observed COVID log(SMR)",
       x = "Longitude", y = "Latitude", fill = "Observed log(SMR)")
# Standardized Residuals
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = COVIDlogSMR.residuals)) +
  geom_sf_text(aes(label = round(COVIDlogSMR.residuals,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Residuals COVID log(SMR)",
       x = "Longitude", y = "Latitude", fill = "Standardized Residual")

#1918 FLU log(SMR) ### !!!! Violates Normality Assumptions (and it sucks in general) !!!!!
Mo.shape.full20 <- cbind(Mo.shape,FLUlogSMR.SAR$fitted.values)
Mo.shape.full20 <- cbind(Mo.shape.full20,FLUlogSMR.SAR$residuals)
#Predictions
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = FLUlogSMR.SAR.fitted.values)) +
  geom_sf_text(aes(label = round(FLUlogSMR.SAR.fitted.values,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Predicted 1918 FLU log(SMR)",
       x = "Longitude", y = "Latitude", fill = "Predicted log(SMR)")
# Observed log(SMR)
ggplot(data = fcd1910) +
  geom_sf(aes(fill = log(SMR))) +
  geom_sf_text(aes(label = round(log(SMR),2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Observed 1918 Flu log(SMR) of the 1918 Flu",
       x = "Longitude", y = "Latitude", fill = "log(SMR)")
# Standardized Residuals
ggplot(data = Mo.shape.full20) +
  geom_sf(aes(fill = FLUlogSMR.SAR.residuals)) +
  geom_sf_text(aes(label = round(FLUlogSMR.SAR.residuals,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Residuals 1918 FLU log(SMR)",
       x = "Longitude", y = "Latitude", fill = "Standardized Residual")

