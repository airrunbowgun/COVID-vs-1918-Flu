# Variables saved from data_import_ver2.R
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
load("models_n_preds.rdata")

# add prison indicator variable to existing data (from "COVID-19 RAPID Data Collection.xlsx" sheet = 'Prison Data')
## !!!! NOTE !!!! Does NOT include St. Louis City which listed 1 inmate in the population ??? !!!
var2020cl$Prison <- rep(0,115)
var2020cl$Prison[c(4,11,14,26,27,32,36,48,59,67,74,68,82,88,95,107,110,112)] <- 1


library(maps)
library(maptools)
library(ggplot2)
library(dplyr)
library(DHARMa)
library(spdep) 
library(MASS)
library(spatialreg)

### ### ### ### ### ### ### ### ### ### ### ### #
### Negative Binomial Model w/ log(E) offset ####
###            Deaths 1918 FLU              ### #
### ### ### ### ### ### ### ### ### ### ### ### #
nb19glm <- glm.nb(Deaths ~ Density + poly(Literacy,2,raw = T) + Avg.Farm.Val  + YO.Ratio,
                  offset(log(E)), data = var1910cl)
summary(nb19glm)

# DHARMa sim residual comparison
nb19simres500 <- simulateResiduals(nb19glm,n=500)
plot(nb19simres500)

## spatial correlation of residuals?
# for adjacency matrix and lists
## Adjaceney matrices and lists
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.listB <- nb2listw(W.nb,style = "B") ## listw objects that the tests (and spatialreg) want 
W.listNorm <- nb2listw(W.nb,style = "W") 
W.listS <- nb2listw(W.nb,style = "S")
W.listC <- nb2listw(W.nb,style = "C")
#creates a symmetric proximity list from the normalized neighbor list
W.listNorm.sym <- similar.listw(W.listNorm)

#moran test
mtest19SMR <- moran.test(nb19glm$residuals,randomisation = F,
                         W.listB)
mtest19SMR

## Prediction imagery FLU SMR ####
#SMR Predictions (scales predicted counts to SMR by dividing by "E" for each county)
Mo.shape.full19 <- cbind(Mo.shape,nb19glm$fitted.values/var1910cl$E)
ggplot(data = Mo.shape.full19) +
  geom_sf(aes(fill = nb19glm.fitted.values.var1910cl.E)) +
  geom_sf_text(aes(label = round(nb19glm.fitted.values.var1910cl.E,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Predicted 1918 FLU SMR",
       x = "Longitude", y = "Latitude", fill = "Predicted SMR")
#SMR Observed
ggplot(data = fcd1910) +
  geom_sf(aes(fill = SMR)) +
  geom_sf_text(aes(label = round(SMR,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Observed 1918 Flu SMR",
       x = "Longitude", y = "Latitude", fill = "Observed SMR")


### ### ### ### ### ### ### ### ### ### ### ### #
###         SAR model for 2020 SCR           ####
### ### ### ### ### ### ### ### ### ### ### ### #

## !!!!!!!(removed Avg.Farm.Val due to singularity issues)!!!!!!!!!
COVID.SCR.SAR <- errorsarlm(SCR ~ poly(Literacy,2,raw = T) + PCTMale +
                              poly(YO.Ratio,2,raw = T) + poly(Prop.Hisp,2,raw = T) + 
                              Prison, data = var2020cl, W.listB)

summary(COVID.SCR.SAR)
mtestCOVID.SCR.SAR <- moran.test(COVID.SCR.SAR$residuals,randomisation = F,
                                 W.listNorm)
mtestCOVID.SCR.SAR
nortest::ad.test(COVID.SCR.SAR$residuals)

## Prediction imagery COVID SCR ####
Mo.shape.full20 <- cbind(Mo.shape,COVID.SCR.SAR$fitted.values)
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

### ### ### ### ### ### ### ### ### ### ### ### #
###           LM for 2020 log(SMR)           ####
### ### ### ### ### ### ### ### ### ### ### ### #
COVIDlogSMR <- lm(log(SMR) ~ poly(Literacy,2,raw = T) + Avg.Farm.Val + poly(PCTMale,2,raw = T) + 
                    YO.Ratio + Prop.Hisp + Prison, data = var2020cl)
summary(COVIDlogSMR)
nortest::ad.test(COVIDlogSMR$residuals)

## Prediction imagery COVID log(SMR) ####
Mo.shape.full20 <- cbind(Mo.shape,COVIDlogSMR$fitted.values)
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
