setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse)
library(gridExtra)
## Rdata file with predictor variables, total counts "FLU.data", daily counts time series from 1/1/1918 to 12/31/1920 "flu.death_TS"
## and shape files w/ centroids
load("FLU_data.Rdata")

##death counts cumulative plot ####
cum.deaths <- flu.deaths_TS[,-(ncol(flu.deaths_TS))] #remove the date column
for (i in 2:nrow(cum.deaths)){
  cum.deaths[i,] <- cum.deaths[i,]+cum.deaths[i-1,]
}
##add population data and rurality designations
cum.deaths.dat <- data.frame( pop.cat = FLU.data$Pop.Cat,
                      population = FLU.data$TotalPop,
                      cts = t(cum.deaths))
## cumulative time series for each rurality group
flu.ts <- rowsum(cum.deaths.dat[,3:ncol(cum.deaths.dat)], group = cum.deaths.dat$pop.cat)
rownames(flu.ts) <- list("Rural", "Semi", "Urban")
flu.ts <- as.data.frame(t(flu.ts)) 
flu.ts$date <- seq(from = as.Date("1918-01-01"),
                   to = as.Date("1920-12-31"), by = 1)
## population and death totals
r.pop = as.numeric(sum(cum.deaths.dat$population[cum.deaths.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(cum.deaths.dat$population[cum.deaths.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(cum.deaths.dat$population[cum.deaths.dat$pop.cat=="Urban"]))
cases.tot = as.numeric(sum(cum.deaths.dat[,ncol(cum.deaths.dat)]))
p.tot = as.numeric(sum(cum.deaths.dat$population))
## SMR standardize
E.r = (r.pop*cases.tot)/(p.tot)
E.s = (s.pop*cases.tot)/(p.tot)
E.u = (u.pop*cases.tot)/(p.tot)
flu.ts$Rural.SMR <- flu.ts$Rural/E.r
flu.ts$Semi.SMR <- flu.ts$Semi/E.s
flu.ts$Urban.SMR <- flu.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
flu.ts$Rural.100k <- flu.ts$Rural*k100.r
flu.ts$Semi.100k <- flu.ts$Semi*k100.s
flu.ts$Urban.100k <- flu.ts$Urban*k100.u

## Raw Cumulative plot
cum.death.Raw <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative 1918 Flu Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="black", "Semi"="red", 
                                 "Urban"="blue"))
## Cumulative Deaths /100,000 plot
cum.death.100k <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural.100k"))+
  geom_line(aes(y=Semi.100k, col = "Semi.100k"))+
  geom_line(aes(y=Urban.100k, col = "Urban.100k"))+
  ylab("Cumulative 1918 Flu Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.100k", "Semi.100k", "Urban.100k"),
                      values = c("Rural.100k"="black", "Semi.100k"="red", 
                                 "Urban.100k"="blue"))

## Cumulative SMR plot
cum.death.SMR <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=Rural.SMR, col = "Rural.SMR"))+
  geom_line(aes(y=Semi.SMR, col = "Semi.SMR"))+
  geom_line(aes(y=Urban.SMR, col = "Urban.SMR"))+
  ylab("Cumulative 1918 Flu SMR") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.SMR", "Semi.SMR", "Urban.SMR"),
                      values = c("Rural.SMR"="black", "Semi.SMR"="red", 
                                 "Urban.SMR"="blue"))

## update the plot legend & title for Lisa's presentation (mid March 2022)
## Cumulative Deaths /100,000 plot
p1 <- ggplot(flu.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural"))+
  geom_line(aes(y=Semi.100k, col = "Semi"))+
  geom_line(aes(y=Urban.100k, col = "Urban"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="#000000", "Semi"="#E69F00", 
                                 "Urban"="#56B4E9")) +
  labs(title = "1918 Flu Cumulative Deaths 1918 to 1921",
       subtitle = "by rurality designation")

Mo.shape.combined <- Mo.shape %>% mutate(Rurality = FLU.data$Pop.Cat)
p2 <- ggplot(data = Mo.shape.combined) +
  geom_sf(aes(fill = Rurality)) +
  scale_fill_manual(values = c("Rural"="#000000", "Semi"="#E69F00", 
                               "Urban"="#56B4E9")) +
  labs(title = "1918 Flu County Rurality Designations") +
  theme(legend.title= element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grid.arrange(p2,p1,ncol=2)