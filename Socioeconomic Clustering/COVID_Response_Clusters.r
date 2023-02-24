## Create clusters using ClustGeo, create graphs, 
## investigate clusters using a random forest model, and use the clusters in a simple model. ##
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse)
library(spdep)
## loads shape files, centroids, and predictor variables
load("COVID_data.Rdata")
#manipulate the shape file
Mo.shape.new <- as(Mo.shape, "Spatial")
Date <- conf.deaths_TS$Date

## Confirmed Deaths ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
conf.deaths_TS <- mutate(conf.deaths_TS, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                         Clay = Clay + round(`Kansas City` * (128631/495377),0),       ## % of KC in Clay county
                         Jackson = Jackson + round(`Kansas City` * (319781/495377),0), ## % of KC in Jackson county
                         Platte = Platte + round(`Kansas City` * (46965/495377),0),    ## % of KC in Platte county
                         Jasper = Jasper + round(Joplin * 0.9,0),   ## 90% of Joplin in Jasper county
                         Newton = Newton + round(Joplin * 0.1,0))   ## 10% of Joplin in Newton county
conf.deaths_TS <- conf.deaths_TS[,1:(ncol(conf.deaths_TS)-4)] ##removes Indep, KC, and Joplin (and the date column)
##scale the count time series
CD.TS.scaled <- scale(conf.deaths_TS, center = F, scale = T)
# number of clusters
K=2
#kmeans
km2 <- kmeans(t(CD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km2$cluster, main = "Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=3
#kmeans
km3 <- kmeans(t(CD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km3$cluster, main = "Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=4
#kmeans
km4 <- kmeans(t(CD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km4$cluster, main = "Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


## Probable + Confirmed Deaths ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
prob_conf.deaths_TS <- mutate(prob_conf.deaths_TS, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                         Clay = Clay + round(`Kansas City` * (128631/495377),0),       ## % of KC in Clay county
                         Jackson = Jackson + round(`Kansas City` * (319781/495377),0), ## % of KC in Jackson county
                         Platte = Platte + round(`Kansas City` * (46965/495377),0),    ## % of KC in Platte county
                         Jasper = Jasper + round(Joplin * 0.9,0),   ## 90% of Joplin in Jasper county
                         Newton = Newton + round(Joplin * 0.1,0))   ## 10% of Joplin in Newton county
prob_conf.deaths_TS <- prob_conf.deaths_TS[,1:(ncol(prob_conf.deaths_TS)-4)] ##removes Indep, KC, and Joplin (and the date column)
##scale the count time series
PCD.TS.scaled <- scale(prob_conf.deaths_TS, center = F, scale = T)
# number of clusters
K=2
#kmeans
km2 <- kmeans(t(PCD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km2$cluster, main = "Probable + Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=3
#kmeans
km3 <- kmeans(t(PCD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km3$cluster, main = "Probable + Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=4
#kmeans
km4 <- kmeans(t(PCD.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km4$cluster, main = "Probable + Confirmed Death County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


### Confirmed + probable Cases ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
prob_conf.cases_TS <- mutate(prob_conf.cases_TS, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                         Clay = Clay + round(`Kansas City` * (128631/495377),0),       ## % of KC in Clay county
                         Jackson = Jackson + round(`Kansas City` * (319781/495377),0), ## % of KC in Jackson county
                         Platte = Platte + round(`Kansas City` * (46965/495377),0),    ## % of KC in Platte county
                         Jasper = Jasper + round(Joplin * 0.9,0),   ## 90% of Joplin in Jasper county
                         Newton = Newton + round(Joplin * 0.1,0))   ## 10% of Joplin in Newton county
prob_conf.cases_TS <- prob_conf.cases_TS[,1:(ncol(prob_conf.cases_TS)-4)] ##removes Indep, KC, and Joplin (and the date column)
##scale the count time series
PCC.TS.scaled <- scale(prob_conf.cases_TS, center = F, scale = T)
par(mfrow=c(1,3))
# number of clusters
K=2
#kmeans
km2 <- kmeans(t(PCC.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km2$cluster, main = "Probable + Confirmed Cases County Clusters")
#legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=3
#kmeans
km3 <- kmeans(t(PCC.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km3$cluster, main = "Probable + Confirmed Cases County Clusters")
#legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=4
#kmeans
km4 <- kmeans(t(PCC.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km4$cluster, main = "Probable + Confirmed Cases County Clusters")
#legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 
# number of clusters
K=5
#kmeans
km5 <- kmeans(t(PCC.TS.scaled),centers =  K, nstart = 20)
#plot of county clusters
sp::plot(Mo.shape.new,border = "grey", col = km5$cluster, main = "Probable + Confirmed Cases County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


#################################################################################################################

### time series visualizations of 4-cluster probable + confirmed cases
##add population data and cluster designations
cases.TS <- data.frame(cluster = km4$cluster,
                       population = COVID.data$TotalPop,
                       cases = t(prob_conf.cases_TS))
## cumulative time series for each cluster group
cov.ts <- rowsum(cases.TS[,3:ncol(cases.TS)], group = cases.TS$cluster)
rownames(cov.ts) <- list("C1", "C2", "C3", "C4")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
## population and death totals for scaling
pop1 = as.numeric(sum(cases.TS$population[cases.TS$cluster==1]))
pop2 = as.numeric(sum(cases.TS$population[cases.TS$cluster==2]))
pop3 = as.numeric(sum(cases.TS$population[cases.TS$cluster==3]))
pop4 = as.numeric(sum(cases.TS$population[cases.TS$cluster==4]))
p.tot = as.numeric(sum(cases.TS$population))
## per 100,000 standardize
k100.1 = (100000/pop1)
k100.2 = (100000/pop2)
k100.3 = (100000/pop3)
k100.4 = (100000/pop4)
cov.ts$C1.100k <- cov.ts$C1*k100.1
cov.ts$C2.100k <- cov.ts$C2*k100.2
cov.ts$C3.100k <- cov.ts$C3*k100.3
cov.ts$C4.100k <- cov.ts$C4*k100.4

## Raw Cumulative plot
pccRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1, col = "C1"))+
  geom_line(aes(y=C2, col = "C2"))+
  geom_line(aes(y=C3, col = "C3"))+
  geom_line(aes(y=C4, col = "C4"))+
  ylab("Probable + Confirmed Cases") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1", "C2", "C3", "C4"),
                      values = c("C1"="black", "C2"="red", 
                                 "C3"="blue", "C4"="orange"))
## /100,000 plot
pcc100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Probable + Confirmed Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="orange", "C4.100k"="blue"))

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

