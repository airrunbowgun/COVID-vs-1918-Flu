## Create clusters using ClustGeo, create graphs, 
## investigate clusters using a random forest model, and use the clusters in a simple model. ##
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse)
library(ClustGeo) # cluster analysis
library(spdep)
## loads shape files, centroids, and predictor variables
load("FLU_data.Rdata")
#manipulate the shape file
Mo.shape.new <- as(Mo.shape, "Spatial")

########################################################################
###### Response Clustering based on total pandemic deaths / 100k #######
########################################################################
FLU.data <- mutate(FLU.data, Deaths.100k = Total.Deaths*100000/TotalPop)

# Get the distance matrix for geography (D1)
Mo.shape.new <- as(Mo.shape, "Spatial") # to convert to SpatialPolygonsDataFrame
centroids.mat <- as.matrix(centroids@coords) # grab the centroid coordinates (which are lat and long)
dist1 <- raster::pointDistance(centroids.mat, centroids.mat, lonlat = TRUE, allpairs = TRUE)
D1 <- dist(dist1) # distance based on centroid lat/lon
D020 <- dist(FLU.data$Deaths.100k) # predictor norms / "distances"

## 3 clusters ####
K <- 3 
## Clusters without proximity constraint
tree204 <- hclustgeo(D020)

part204 <- cutree(tree204, k = K) 

sp::plot(Mo.shape.new,border = "grey", col = part204, main = "TOTAL Flu Deaths / 100k Similarity ONLY")
legend("left", legend=paste("cluster",1:K), fill=1:5, cex=0.8,bty="n",border="white") 
##############################################
###### Contiguity Constrained Clusters #######
## Determine Centroid Constraint Weights
range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
cr204$Qnorm # k=4, optimum 0.15 alpha

## 3plots small alpha adjustments here were creating large differences in the county clusters
tree204.alpha <- hclustgeo(D020,D1,alpha = .15) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Contiguity Constrained 3 Clusters TOTAL 1918 Flu Deaths / 100k")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


## 4 clusters ####
K <- 4 
## Clusters without proximity constraint
tree204 <- hclustgeo(D020)

part204 <- cutree(tree204, k = K) 

sp::plot(Mo.shape.new,border = "grey", col = part204, main = "TOTAL Flu Deaths / 100k Similarity ONLY")
legend("left", legend=paste("cluster",1:K), fill=1:5, cex=0.8,bty="n",border="white") 
##############################################
###### Contiguity Constrained Clusters #######
## Determine Centroid Constraint Weights
range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
cr204$Qnorm # k=4, optimum 0.15 alpha

## 4plots
tree204.alpha <- hclustgeo(D020,D1,alpha = .15) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Contiguity Constrained 4 Clusters TOTAL 1918 Flu Deaths / 100k")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 

# ## 4plots this also had an appreciable amount of change based on alpha choice
# tree204.alpha <- hclustgeo(D020,D1,alpha = .2) 
# part204.alpha <- cutree(tree204.alpha, k = K) 
# sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Contiguity Constrained 4 Clusters TOTAL 1918 Flu Deaths / 100k .2alpha")
# legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


#################################################
##death counts cumulative time series plots ####
################################################
cum.deaths <- flu.deaths_TS[,-(ncol(flu.deaths_TS))] ##remove the date column
for (i in 2:nrow(cum.deaths)){
  cum.deaths[i,] <- cum.deaths[i,]+cum.deaths[i-1,]
}
tcum.deaths <- as.data.frame(t(cum.deaths))
tcum.deaths.100k <- tcum.deaths*100000/FLU.data$TotalPop

###### Response Clustering based on total pandemic deaths / 100k #######
# Get the distance matrix for geography (D1)
Mo.shape.new <- as(Mo.shape, "Spatial") # to convert to SpatialPolygonsDataFrame
centroids.mat <- as.matrix(centroids@coords) # grab the centroid coordinates (which are lat and long)
dist1 <- raster::pointDistance(centroids.mat, centroids.mat, lonlat = TRUE, allpairs = TRUE)
D1 <- dist(dist1) # distance based on centroid lat/lon
D020 <- dist(tcum.deaths.100k) # predictor norms / "distances"

## 3 clusters ####
K <- 3 
## Clusters without proximity constraint
tree204 <- hclustgeo(D020)

part204 <- cutree(tree204, k = K) 

sp::plot(Mo.shape.new,border = "grey", col = part204, main = "Cumulative Time Series Flu Deaths / 100k Similarity ONLY")
legend("left", legend=paste("cluster",1:K), fill=1:5, cex=0.8,bty="n",border="white") 
##############################################
###### Contiguity Constrained Clusters #######
## Determine Centroid Constraint Weights
range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
cr204$Qnorm # k=4, optimum 0.1 alpha

## 3plots small alpha adjustments here were creating large differences in the county clusters
tree204.alpha <- hclustgeo(D020,D1,alpha = .1) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Contiguity Constrained Cumulative Time Series Flu Deaths / 100k")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


## 4 clusters ####
K <- 4 
## Clusters without proximity constraint
tree204 <- hclustgeo(D020)

part204 <- cutree(tree204, k = K) 

sp::plot(Mo.shape.new,border = "grey", col = part204, main = "Cumulative Time Series Flu Deaths / 100k Similarity ONLY")
legend("left", legend=paste("cluster",1:K), fill=1:5, cex=0.8,bty="n",border="white") 
##############################################
###### Contiguity Constrained Clusters #######
## Determine Centroid Constraint Weights
range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
cr204$Qnorm # k=4, optimum 0.15 OR perhaps 0.1 alpha

## 4plots
tree204.alpha <- hclustgeo(D020,D1,alpha = .15) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Contiguity Constrained Cumulative Time Series Flu Deaths / 100k")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 











#################################################################################################################

### time series visualizations of 4-cluster deaths
##add population data and cluster designations
## time series visualization based on the 4 predictor defined clusters ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
flu.deaths_TS <- flu.deaths_TS[,-(ncol(flu.deaths_TS))] ##removes the date column

##confirmed deaths cumulative plot ####
death.cum <- flu.deaths_TS
for (i in 2:nrow(death.cum)){
  death.cum[i,] <- death.cum[i,]+death.cum[i-1,]
}

##add population data and cluster designations
death.TS <- data.frame(cluster = part204.alpha,
                       population = FLU.data$TotalPop,
                       deaths = t(death.cum))
## check each cluster county group's total population (sanity check)
death.TS %>% group_by(cluster) %>% summarise(sum(population))

## cumulative time series for each cluster group (sum each county cumulative counts in a cluster)
flu.ts <- rowsum(death.TS[,3:ncol(death.TS)], group = death.TS$cluster)
rownames(flu.ts) <- list("C1", "C2", "C3", "C4")
flu.ts <- as.data.frame(t(flu.ts)) 
flu.ts$date <- seq(from = as.Date("1918-01-01"),
                   to = as.Date("1920-12-31"), by = 1)
## population and death totals for scaling
pop1 = as.numeric(sum(death.TS$population[death.TS$cluster==1]))
pop2 = as.numeric(sum(death.TS$population[death.TS$cluster==2]))
pop3 = as.numeric(sum(death.TS$population[death.TS$cluster==3]))
pop4 = as.numeric(sum(death.TS$population[death.TS$cluster==4]))
p.tot = as.numeric(sum(death.TS$population))
## per 100,000 standardize
k100.1 = (100000/pop1)
k100.2 = (100000/pop2)
k100.3 = (100000/pop3)
k100.4 = (100000/pop4)
flu.ts$C1.100k <- flu.ts$C1*k100.1
flu.ts$C2.100k <- flu.ts$C2*k100.2
flu.ts$C3.100k <- flu.ts$C3*k100.3
flu.ts$C4.100k <- flu.ts$C4*k100.4

## Raw Cumulative plot
flu.deaths.Raw <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=C1, col = "C1"))+
  geom_line(aes(y=C2, col = "C2"))+
  geom_line(aes(y=C3, col = "C3"))+
  geom_line(aes(y=C4, col = "C4"))+
  ylab("Cumulative 1918 Flu Deaths by RESPONSE Cluster") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1", "C2", "C3", "C4"),
                      values = c("C1"="black", "C2"="red", 
                                 "C3"="blue", "C4"="orange"))
## /100,000 plot
flu.deaths.100k <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Cumulative 1918 Flu Deaths / 100,000 by RESPONSE Cluster") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="green", "C4.100k"="blue"))














#### this stuff below might just be cut/paste from the COVID file(s) (NEED TO CHECK IT OUT!! 2/21/22)





## Random forest variable importance plots for the 4 cluster socioeconomic categories:
newdata <- data.frame(X20.scale, part204.alpha)
rf <- randomForest(x = X20.scale, y = as.factor(part204.alpha), data = newdata, importance = TRUE, mtry = 4, ntree = 1000)
varImpPlot(rf, main = "1910 Variable Importance Plots")

## reduced time dimension (101 x 7 day periods) of the / 100000 counts
cov.ts.reduced <- cov.ts[-c(1:2),]
weekly.cases <- matrix(,nrow=101,ncol=4)
for (i in 1:101){
  weekly.cases[i,] = colSums(cov.ts.reduced[(7*i-6):(7*i),6:9])
}
weekly.cases <- as.data.frame(weekly.cases)
weekly.cases$dates <- seq(from = as.Date("2020-01-03"),
                          to = as.Date("2021-12-09"), by = 7)
colnames(weekly.cases) <- list("C1.100k","C2.100k","C3.100k","C4.100k","date")

## /100,000 plot
ggplot(weekly.cases,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Probable + Confirmed Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="orange", "C4.100k"="blue")) +
  labs(title = "Weekly Aggregated Probable + Confirmed Cases /100k by Kmeans Response Cluster")

## cumulative plot
##confirmed cases cumulative plot ####
cases.cum <- cov.ts.reduced[,-5]
for (i in 2:nrow(cases.cum)){
  cases.cum[i,] <- cases.cum[i,]+cases.cum[i-1,]
}
cases.cum$date <- cov.ts.reduced$date
## /100,000 cumulative plot
ggplot(cases.cum,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Probable + Confirmed Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="orange", "C4.100k"="blue")) +
  labs(title = "Cumulative Probable + Confirmed Cases /100k by Kmeans Response Cluster")

