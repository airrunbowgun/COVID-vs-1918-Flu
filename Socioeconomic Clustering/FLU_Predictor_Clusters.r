## Create clusters using ClustGeo, create graphs, 
## investigate clusters using a random forest model, and use the clusters in a simple model. ##
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse) 
library(ClustGeo) # cluster analysis
library(spdep) # adjacency matrix
library(randomForest) # random forest for importance plots

## loads shape files, centroids, death counts, (total and daily) and predictor variables
load("FLU_data.Rdata")

X20 <- FLU.data[,6:11] # select & scale 1910 predictor variables
X20.scale <- scale(X20)

###### Clustering #######
# Get the distance matrix for geography (D1)
Mo.shape.new <- as(Mo.shape, "Spatial") # to convert to SpatialPolygonsDataFrame
centroids.mat <- as.matrix(centroids@coords) # grab the centroid coordinates (which are lat and long)
dist1 <- raster::pointDistance(centroids.mat, centroids.mat, lonlat = TRUE, allpairs = TRUE)
D1 <- dist(dist1) # distance based on centroid lat/lon
D020 <- dist(X20.scale) # predictor norms / "distances"

##############################################
###### Contiguity Constrained Clusters #######
## Determine Centroid Constraint Weights
## 4 clusters ####
K <- 4 

range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
cr204$Qnorm # k=4, optimum 0.15 alpha

## 4plots
tree204.alpha <- hclustgeo(D020,D1,alpha = .15) 
part204.alpha <- cutree(tree204.alpha, k = K) 
sp::plot(Mo.shape.new,border = "grey", col = part204.alpha, main = "1918 FLU Socio-Economic County Clusters")
legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 


## time series visualization based on the 4 predictor defined clusters ####
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
  ylab("Cumulative 1918 Flu Deaths") +
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
  ylab("Cumulative 1918 Flu Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("C1.100k", "C2.100k", "C3.100k", "C4.100k"),
                      values = c("C1.100k"="black", "C2.100k"="red", 
                                 "C3.100k"="green", "C4.100k"="blue"))

## Random forest variable importance plots for the 4 cluster socioeconomic categories:
newdata <- data.frame(X20.scale, part204.alpha)
rf <- randomForest(x = X20.scale, y = as.factor(part204.alpha), data = newdata, importance = TRUE, mtry = 4, ntree = 1000)
vimp.plot <- varImpPlot(rf, main = "1910 Variable Importance Plots")
varImpPlot(rf, main = "1910 Variable Importance Plots")

#########################################################
## Exploratory Predictor Analysis By Clusters     #######
#########################################################
Flu.preds <- FLU.data %>% 
  select(!(Density)) %>%
  mutate(Cluster = as.factor(part204.alpha)) %>%
  group_by(Cluster)
  
ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=Avg.Farm.Val)) +
  labs(title = "1910 Farm Value Boxplots by Predictor Cluster")

ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=Literacy)) +
  labs(title = "1910 Literacy Boxplots by Predictor Cluster")
  
ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=PCPs100k)) +
  labs(title = "1910 PCPs/100k Population Boxplots by Predictor Cluster")

ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=Prop.White)) +
  labs(title = "1910 % White Boxplots by Predictor Cluster")  
  
ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=PCTMale)) +
  labs(title = "1910 % Male Boxplots by Predictor Cluster")    

ggplot(Flu.preds) +
  geom_boxplot(aes(x=Cluster,y=YO.Ratio)) +
  labs(title = "1910 Old/Young Age Ratio Boxplots by Predictor Cluster")    
  
Means <- Flu.preds  %>%
  group_by(Cluster) %>%
  summarise(mean(Literacy),mean(Avg.Farm.Val),mean(PCTMale),mean(PCPs100k),mean(YO.Ratio),mean(Prop.White))
as.data.frame(Means)




#######################################################
## Predictor Cluster Time Series Plots         ########
#######################################################

## cumulative time series for each county by cluster group
cum.death.ts <- pivot_longer(mutate(death.TS, COUNTYFP = Mo.shape$COUNTYFP, NAME = Mo.shape$NAME),cols = starts_with("deaths"), values_to = "cumulative.counts") 
cum.death.ts <- cum.death.ts %>% select(COUNTYFP, ,NAME, cluster, population, cumulative.counts) %>% 
  mutate("deaths.100k" = cumulative.counts*100000/population, 
         "date" = rep(seq(from = as.Date("1918-01-01"),
                          to = as.Date("1920-12-31"), by = 1), 115))

## Raw Cumulative plot
ggplot(filter(cum.death.ts, cluster == 1),aes(x=date)) +
  geom_line(aes(y=cumulative.counts, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 1 County Cumulative Deaths",y = "Cumulative Flu Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

## /100k Cumulative plot
c1p <- ggplot(filter(cum.death.ts, cluster == 1),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 1 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c2p <- ggplot(filter(cum.death.ts, cluster == 2),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 2 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c3p <- ggplot(filter(cum.death.ts, cluster == 3),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 3 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c4p <- ggplot(filter(cum.death.ts, cluster == 4),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = COUNTYFP, group = COUNTYFP),show.legend = F) +
  labs(title = "Cluster 4 County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")


library(gridExtra)
grid.arrange(c1p,c2p,c3p,c4p,ncol = 2)
grid.arrange(c1p,c2p,c3p,c4p,ncol = 4)
ggplot(cum.death.ts,aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = as.factor(cluster))) +
  labs(title = "County Cumulative Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k", colour = "Predictor Cluster") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

## extract the top 3 county cumulative death time series within each cluster
top3.counties <- cum.death.ts %>% 
  filter(date == "1920-12-31") %>%
  arrange(desc(deaths.100k)) %>%
  group_by(cluster) %>%
  slice(1:3)

##
## plot each of the top 3 county time series by cluster: ####
##
## select the top 3 county time series from each of the 4 clusters
top3.ts <- cum.death.ts %>%
  filter(COUNTYFP %in% top3.counties$COUNTYFP) %>%
  mutate(cluster = as.factor(cluster))
names <- data.frame(names = c("Dunklin","Butler","Jackson","Carter","STL","Greene","Barton"),
                    yloc = c(2500,2000,1650,1490, 1310, 1200, 900))

## /100k Cumulative plot
## all plotted together
ggplot(top3.ts,aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = cluster, group = NAME)) +
  labs(title = "Highest 3 County Cumulative Flu Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  geom_text(data = names, aes(x = as.Date("1921-01-01"), y = yloc, label = names),check_overlap = T) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")


c1p3 <- ggplot(filter(top3.ts, cluster == 1),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = NAME, group = NAME)) +
  labs(title = "Cluster 1 Highest 3 County Cumulative Flu Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c2p3 <- ggplot(filter(top3.ts, cluster == 2),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = NAME, group = NAME)) +
  labs(title = "Cluster 2 Highest 3 County Cumulative Flu Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c3p3 <- ggplot(filter(top3.ts, cluster == 3),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = NAME, group = NAME)) +
  labs(title = "Cluster 3 Highest 3 County Cumulative Flu Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")

c4p3 <- ggplot(filter(top3.ts, cluster == 4),aes(x=date)) +
  geom_line(aes(y=deaths.100k, colour = NAME, group = NAME)) +
  labs(title = "Cluster 4 Highest 3 County Cumulative Flu Deaths / 100K",y = "Cumulative Confirmed Deaths / 100k") +
  ylim(c(0,2550)) +
  scale_x_date(breaks = "3 months", date_labels = "%b %y")


library(gridExtra)
grid.arrange(c1p3,c2p3,c3p3,c4p3,ncol = 1)

## Highlight each of the top 3 counties in each cluster


t3.indicator <- ifelse(FLU.data$COUNTYFP %in% top3.counties$COUNTYFP,"T3"," ")
Mo.shape.obs <- mutate(Mo.shape, Cluster = as.factor(part204.alpha), Top3 = t3.indicator)
ggplot(data = Mo.shape.obs) +
  geom_sf(aes(fill = Cluster)) +
  geom_sf_text(aes(label = Top3), colour = "black") +
  labs(title = "Predictor Clusters with Top 3 County Deaths/100k Indicated",
       x = "Longitude", y = "Latitude", fill = "Cluster")

# sp::plot(Mo.shape.new,border = "grey", col = t3.indicator, main = "1918 FLU Socio-Economic County Clusters")
# legend("left", legend=paste("cluster",1:K), fill=1:K, cex=0.8,bty="n",border="white") 

# update the plot legend & title for Lisa's presentation (mid March 2022)
## Cumulative Deaths /100,000 plot
## /100,000 plot by socio cluster (titled as requested)
p1 <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=C1.100k, col = "North"))+
  geom_line(aes(y=C2.100k, col = "Central"))+
  geom_line(aes(y=C3.100k, col = "Southwest"))+
  geom_line(aes(y=C4.100k, col = "Southeast"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("North", "Central", "Southwest", "Southeast"),
                      values = c("North"="#000000", "Central"="#E69F00", 
                                 "Southwest"="#56B4E9", "Southeast"="#009E73")) +
  labs(title = "1918 Flu Cumulative Deaths 1918 to 1921",
       subtitle = "by socioeconomic cluster")
## 4 clusters ####
K <- 4 
range.alpha <- seq(0,1,0.05)
### 1918 Flu ### ("D0" is the variable constraint, "D1" is the centroid proximity)
cr204 <- choicealpha(D020,D1,range.alpha,K,graph=TRUE)
## 4plots
tree204.alpha <- hclustgeo(D020,D1,alpha = .15) 
part204.alpha <- cutree(tree204.alpha, k = K) 
Mo.shape.combined <- Mo.shape %>% mutate(Cluster = recode(part204.alpha, "1" = "North",
                                                          "2" = "Central",
                                                          "3" = "Southwest",
                                                          "4" = "Southeast"))
p2 <- ggplot(data = Mo.shape.combined) +
  geom_sf(aes(fill = Cluster)) +
  scale_fill_manual(values = c("North"="#000000", "Central"="#E69F00", 
                               "Southwest"="#56B4E9", "Southeast"="#009E73")) +
  labs(title = "1918 Flu Socioeconomic County Clusters") +
  theme(legend.title= element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grid.arrange(p2,p1,ncol=2)