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

### POISSON Model w/ log(E) offset ####
hist(var1910cl$Deaths)
poi19E <- glm(Deaths ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White,
              offset = log(E),
              family=poisson(link = "log"),
              data = var1910cl)
summary(poi19E)
library(DHARMa)
poi19simres500 <- simulateResiduals(poi19E,n=500)
plot(poi19simres500)


### Negative Binomial Model w/ log(E) offset ####
library(MASS)
## Deaths 1918 FLU
nb19glm <- glm.nb(Deaths ~ Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White,
                offset(log(E)), data = var1910cl)
summary(nb19glm)
nb19simres500 <- simulateResiduals(nb19glm,n=500)
plot(nb19simres500)
step(nb19glm)

nb19step <- glm.nb(formula = Deaths ~ Density + Literacy + Avg.Farm.Val + YO.Ratio + Prop.White, 
       data = var1910cl, weights = offset(log(E)), 
       init.theta = 4.745612655, link = log)
summary(nb19step)
nbstepsimres500 <- simulateResiduals(nb19step,n=500)
plot(nbstepsimres500)



## spatial correlation of residuals?
library(spdep) # for adjacency matrix and lists
## Adjaceney matrices and lists
W.nb <- poly2nb(Mo.shape, row.names = rownames(Mo.shape$FIP))
W.listB <- nb2listw(W.nb,style = "B") ## listw objects that the tests (and spatialreg) want 
W.listNorm <- nb2listw(W.nb,style = "W") 
W.listS <- nb2listw(W.nb,style = "S")
W.listC <- nb2listw(W.nb,style = "C")
#creates a symmetric proximity list from the normalized neighbor list
W.listNorm.sym <- similar.listw(W.listNorm)

mtest19SMR <- moran.test(nb19glm$residuals,randomisation = F,
                       W.listB)
mtest19SMR

mtest19step <- moran.test(nb19step$residuals,randomisation = F,
                         W.listB)
mtest19step

## 1918 Death Model Visual Predictions
#SMR Predictions
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

#Error in predicted counts standardized for total population
Mo.shape.full19 <- cbind(Mo.shape,(abs(var1910cl$Deaths-nb19glm$fitted.values))/var1910cl$Deaths)
ggplot(data = Mo.shape.full19) +
  geom_sf(aes(fill = X.abs.var1910cl.Deaths...nb19glm.fitted.values...var1910cl.Deaths)) +
  geom_sf_text(aes(label = round(X.abs.var1910cl.Deaths...nb19glm.fitted.values...var1910cl.Deaths,2)), colour = "black") + 
  scale_fill_gradient(low="white", high="darkblue") +
  labs(title = "Prediction Error / Total Deaths",
       x = "Longitude", y = "Latitude", fill = "PEpD")


## Deaths 2020 COVID
nb20glm <- glm.nb(Deaths ~  Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison,
                  offset(log(EDeath)), data = var2020cl)
summary(nb20glm)
nb20simres500 <- simulateResiduals(nb20glm,n=500)
plot(nb20simres500)

nb20glm <- glm.nb(Deaths ~  Literacy + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison,
                  offset(log(EDeath)), data = var2020cl)
summary(nb20glm)
nb20simres500 <- simulateResiduals(nb20glm,n=500)
plot(nb20simres500)


## Cases 2020 COVID
nb20cglm <- glm.nb(Cases ~  Density + Literacy + Avg.Farm.Val + PCTMale + PCPs100k + YO.Ratio + Prop.White + Prop.Hisp + Prison,
                  offset(log(ECase)), data = var2020cl)
summary(nb20cglm)
nb20csimres500 <- simulateResiduals(nb20cglm,n=500)
plot(nb20csimres500)





