## Create clusters using ClustGeo, create graphs, 
## investigate clusters using a random forest model, and use the clusters in a simple model. ##
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse) 
library(ClustGeo) # cluster analysis
library(spdep) # adjacency matrix
library(randomForest) # random forest for importance plots

## loads shape files, centroids, and predictor variables
load("COVID_data.Rdata")

X20 <- COVID.data[,6:12] # select & scale 2020 predictor variables
X20.scale <- scale(X20)

###### Clustering #######
# Get the distance matrix for geography (D1)
Mo.shape.new <- as(Mo.shape, "Spatial") # to convert to SpatialPolygonsDataFrame
centroids.mat <- as.matrix(centroids@coords) # grab the centroid coordinates (which are lat and long)
dist1 <- raster::pointDistance(centroids.mat, centroids.mat, lonlat = TRUE, allpairs = TRUE)
D1 <- dist(dist1) # distance based on centroid lat/lon
D020 <- dist(X20.scale) # predictor norms / "distances"

## 4 clusters ####
K <- 4 
## 4plots
tree204.alpha <- hclustgeo(D020,D1,alpha = .22) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "Socio-Economic County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


## time series visualization based on the 4 predictor defined clusters ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
conf.deaths_TS <- mutate(conf.deaths_TS, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                     Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                     Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                     Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                     Jasper = Jasper + Joplin * 0.9,   ## 90% of Joplin in Jasper county
                     Newton = Newton + Joplin * 0.1)   ## 10% of Joplin in Newton county
conf.deaths_TS <- conf.deaths_TS[,1:(ncol(conf.deaths_TS)-4)] ##removes Indep, KC, and Joplin (and the date column)

##confirmed deaths cumulative plot ####
death.cum <- conf.deaths_TS
for (i in 2:nrow(death.cum)){
  death.cum[i,] <- death.cum[i,]+death.cum[i-1,]
}

##add population data and cluster designations
death.TS <- data.frame(cluster = part204.alpha,
                      population = COVID.data$TotalPop,
                      deaths = t(death.cum))
## check each cluster county group's total population (sanity check)
death.TS %>% group_by(cluster) %>% summarise(sum(population))

## cumulative time series for each cluster group (sum each county cumulative counts in a cluster)
cov.ts <- rowsum(death.TS[,3:ncol(death.TS)], group = death.TS$cluster)
rownames(cov.ts) <- list("C1", "C2", "C3", "C4")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
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
cov.ts$C1.100k <- cov.ts$C1*k100.1
cov.ts$C2.100k <- cov.ts$C2*k100.2
cov.ts$C3.100k <- cov.ts$C3*k100.3
cov.ts$C4.100k <- cov.ts$C4*k100.4

## Raw Cumulative plot
cdRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1, col = "C1"))+
  geom_line(aes(y=C2, col = "C2"))+
  geom_line(aes(y=C3, col = "C3"))+
  geom_line(aes(y=C4, col = "C4"))+
  ylab("Cumulative Confirmed Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1", "C2", "C3", "C4"),
                      values = c("C1"="black", "C2"="red", 
                                 "C3"="blue", "C4"="orange"))
## /100,000 plot
cd100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Cumulative Confirmed Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="orange", "C4.100k"="blue"))
## to facilitate cut/paste of the death count code to use for cases w/o changing variable names (I was in a hurry)
CD.ts <- cov.ts

##  !!!!!!!!!!!!    ### identical code pasted here just uses cases vs. deaths ## !!!!!!!!!!!!!!!!!!!!!!!!!!
##  KEEPS ALL VARIABLE NAMES AFTER INSERTING THE CONFIRMED CASES IN THE NEXT ACTIVE LINE !!!!!!!!!!!!!!!!!!!!
## time series visualization based on the 4 predictor defined clusters ####
## distribute Independence, KC, and Joplin counts to respective counties based on population percentages (!!!SEE EXCEL SOURCE FILE NOTES FOR DETAILS!!!!)
conf.deaths_TS <- mutate(conf.cases_TS, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                         Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                         Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                         Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                         Jasper = Jasper + Joplin * 0.9,   ## 90% of Joplin in Jasper county
                         Newton = Newton + Joplin * 0.1)   ## 10% of Joplin in Newton county
conf.deaths_TS <- conf.deaths_TS[,1:(ncol(conf.deaths_TS)-4)] ##removes Indep, KC, and Joplin (and the date column)

##confirmed deaths cumulative plot ####
death.cum <- conf.deaths_TS
for (i in 2:nrow(death.cum)){
  death.cum[i,] <- death.cum[i,]+death.cum[i-1,]
}

##add population data and cluster designations
death.TS <- data.frame(cluster = part204.alpha,
                       population = COVID.data$TotalPop,
                       deaths = t(death.cum))
## check each cluster county group's total population (sanity check)
death.TS %>% group_by(cluster) %>% summarise(sum(population))

## cumulative time series for each cluster group (sum each county cumulative counts in a cluster)
cov.ts <- rowsum(death.TS[,3:ncol(death.TS)], group = death.TS$cluster)
rownames(cov.ts) <- list("C1", "C2", "C3", "C4")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
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
cov.ts$C1.100k <- cov.ts$C1*k100.1
cov.ts$C2.100k <- cov.ts$C2*k100.2
cov.ts$C3.100k <- cov.ts$C3*k100.3
cov.ts$C4.100k <- cov.ts$C4*k100.4



## BEGAN CHANGING VARIABLE NAMES HERE ## !!!!!!!!!!!!!!!!!
## Raw Cumulative plot
ccRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1, col = "C1"))+
  geom_line(aes(y=C2, col = "C2"))+
  geom_line(aes(y=C3, col = "C3"))+
  geom_line(aes(y=C4, col = "C4"))+
  ylab("Cumulative Confirmed Cases") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1", "C2", "C3", "C4"),
                      values = c("C1"="black", "C2"="red", 
                                 "C3"="blue", "C4"="orange"))
## /100,000 plot
cc100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "C1.100k"))+
  geom_line(aes(y=C2.100k, col = "C2.100k"))+
  geom_line(aes(y=C3.100k, col = "C3.100k"))+
  geom_line(aes(y=C4.100k, col = "C4.100k"))+
  ylab("Cumulative Confirmed Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="orange", "C4.100k"="blue"))



## to facilitate cut/paste of the death count code to use for cases w/o changing variable names (I was in a hurry)
CC.ts <- cov.ts
# update the plot legend & title for Lisa's presentation (mid March 2022)
## Cumulative Deaths /100,000 plot
## /100,000 plot by socio cluster (titled as requested)
p1 <- ggplot(CD.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "North"))+
  geom_line(aes(y=C2.100k, col = "South"))+
  geom_line(aes(y=C3.100k, col = "Urban"))+
  geom_line(aes(y=C4.100k, col = "Bootheel"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("North", "South", "Urban", "Bootheel"),
                      values = c("North"="#000000", "South"="#E69F00", 
                                 "Urban"="#56B4E9", "Bootheel"="#009E73")) +
  labs(title = "COVID Cumulative Deaths 2020 to 2022",
       subtitle = "by socioeconomic cluster")

## Cumulative cases /100,000 plot
## /100,000 plot by socio cluster (titled as requested)
p2 <- ggplot(CC.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "North"))+
  geom_line(aes(y=C2.100k, col = "South"))+
  geom_line(aes(y=C3.100k, col = "Urban"))+
  geom_line(aes(y=C4.100k, col = "Bootheel"))+
  ylab("Cumulative Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("North", "South", "Urban", "Bootheel"),
                      values = c("North"="#000000", "South"="#E69F00", 
                                 "Urban"="#56B4E9", "Bootheel"="#009E73")) +
  labs(title = "COVID Cumulative Cases 2020 to 2022",
       subtitle = "by socioeconomic cluster")


## combine cluster data to Mo.shape file
Mo.shape.combined <- Mo.shape %>% mutate(Cluster = recode(part204.alpha, "1" = "North",
                                                          "2" = "South",
                                                          "3" = "Urban",
                                                          "4" = "Bootheel"))
p3 <- ggplot(data = Mo.shape.combined) +
  geom_sf(aes(fill = Cluster)) +
  scale_fill_manual(values = c("North"="#000000", "South"="#E69F00", 
                               "Urban"="#56B4E9", "Bootheel"="#009E73")) +
  labs(title = "COVID Socioeconomic County Clusters") +
  theme(legend.title= element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grid.arrange(p3,p1,ncol=2)
grid.arrange(p3,p2,ncol=2)












## Random forest variable importance plots for the 4 cluster socioeconomic categories:
newdata <- data.frame(X20.scale, part204.alpha)
rf <- randomForest(x = X20.scale, y = as.factor(part204.alpha), data = newdata, importance = TRUE, mtry = 4, ntree = 1000)
varImpPlot(rf, main = "2020 Variable Importance Plots")






#########################################################
#########################################################

## Below are some experimental stuffs I was doing to look at different county specific standardizations


#######################################################

## cumulative time series for each county by cluster group
cum.death.ts <- pivot_longer(mutate(death.TS, COUNTYFP = Mo.shape$COUNTYFP),cols = starts_with("deaths"), values_to = "cumulative.counts") 
cum.death.ts <- cum.death.ts %>% select(COUNTYFP, cluster, population, cumulative.counts) %>% 
  mutate("deaths.100k" = cumulative.counts*100000/population, 
         "date" = rep(seq(from = as.Date("2020-01-01"),
                          to = as.Date("2020-01-01")+ncol(death.TS)-3, by = 1), 115))

cl <- cum.death.ts %>%
  filter(cluster == 1) %>%
  group_by(date) %>%
  summarise(sum(deaths.100k))



## Raw Cumulative plot
ggplot(filter(cum.death.ts, cluster == 1),aes(x=date)) +
  geom_line(aes(y=cumulative.counts, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 1 County Cumulative Deaths",y = "Cumulative Confirmed Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

## /100k Cumulative plot
c1p <- ggplot(filter(cum.death.ts, cluster == 1),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 1 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c2p <- ggplot(filter(cum.death.ts, cluster == 2),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 2 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c3p <- ggplot(filter(cum.death.ts, cluster == 3),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 3 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c4p <- ggplot(filter(cum.death.ts, cluster == 4),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 4 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

library(gridExtra)
grid.arrange(c1p,c2p,c3p,c4p,ncol = 2)
grid.arrange(c1p,c2p,c3p,c4p,ncol = 4)
ggplot(cum.death.ts,aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = as.factor(cluster))) +
  labs(title = "County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k", colour = "Predictor Cluster") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

