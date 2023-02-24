setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files")
library(tidyverse)
library(gridExtra)
library(readxl)
load("COVID_data.Rdata")
## load 118 county division populations
pops <- t(read_excel("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Excel_Files/CasesDeaths-MO dashboard-211209-verified.xlsx",
                      sheet = "av conf cases", range = "B4:DO4", col_names = F))

##confirmed cases cumulative plot ####
cc.cum <- conf.cases_TS[,-(ncol(conf.cases_TS))]
for (i in 2:nrow(cc.cum)){
  cc.cum[i,] <- cc.cum[i,]+cc.cum[i-1,]
}
##add population data and rurality designations (tacks on the last 3 urbans for the KC etc adds)
cc.dat <- data.frame( pop.cat = c(COVID.data$Pop.Cat,"Urban","Urban","Urban"),
                      population = pops,
                      cts = t(cc.cum))
## cumulative time series for each rurality group
cov.ts <- rowsum(cc.dat[,3:ncol(cc.dat)], group = cc.dat$pop.cat)
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
## population and death totals
r.pop = as.numeric(sum(cc.dat$population[cc.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(cc.dat$population[cc.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(cc.dat$population[cc.dat$pop.cat=="Urban"]))
cases.tot = as.numeric(sum(cc.dat[,ncol(cc.dat)]))
p.tot = as.numeric(sum(cc.dat$population))
## SMR standardize
E.r = (r.pop*cases.tot)/(p.tot)
E.s = (s.pop*cases.tot)/(p.tot)
E.u = (u.pop*cases.tot)/(p.tot)
cov.ts$Rural.SMR <- cov.ts$Rural/E.r
cov.ts$Semi.SMR <- cov.ts$Semi/E.s
cov.ts$Urban.SMR <- cov.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
cov.ts$Rural.100k <- cov.ts$Rural*k100.r
cov.ts$Semi.100k <- cov.ts$Semi*k100.s
cov.ts$Urban.100k <- cov.ts$Urban*k100.u

## Raw Cumulative plot
ccRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Confirmed Cases") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="black", "Semi"="red", 
                                 "Urban"="blue"))
## /100,000 plot
cc100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural.100k"))+
  geom_line(aes(y=Semi.100k, col = "Semi.100k"))+
  geom_line(aes(y=Urban.100k, col = "Urban.100k"))+
  ylab("Cumulative Confirmed Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.100k", "Semi.100k", "Urban.100k"),
                      values = c("Rural.100k"="black", "Semi.100k"="red", 
                                 "Urban.100k"="blue"))
## for the later plots for Lisa's presentation (added Mid March 22)
cc.plot <- cov.ts


##confirmed deaths cumulative plot ####
cd.cum <- conf.deaths_TS[,-(ncol(conf.deaths_TS))]
for (i in 2:nrow(cd.cum)){
  cd.cum[i,] <- cd.cum[i,] + cd.cum[i-1,]
}
##add population data and rurality designations (tacks on the last 3 urbans for the KC etc adds)
cd.dat <- data.frame( pop.cat = c(COVID.data$Pop.Cat,"Urban","Urban","Urban"),
                      population = pops,
                      cts = t(cd.cum))
## cumulative time series for each rurality group
cov.ts <- rowsum(cd.dat[,3:ncol(cd.dat)], group = cd.dat$pop.cat)
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
## population and death totals
r.pop = as.numeric(sum(cd.dat$population[cd.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(cd.dat$population[cd.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(cd.dat$population[cd.dat$pop.cat=="Urban"]))
d.tot = as.numeric(sum(cd.dat[,ncol(cd.dat)]))
p.tot = as.numeric(sum(cd.dat$population))
## SMR standardize
E.r = (r.pop*d.tot)/(p.tot)
E.s = (s.pop*d.tot)/(p.tot)
E.u = (u.pop*d.tot)/(p.tot)
cov.ts$Rural.SMR <- cov.ts$Rural/E.r
cov.ts$Semi.SMR <- cov.ts$Semi/E.s
cov.ts$Urban.SMR <- cov.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
cov.ts$Rural.100k <- cov.ts$Rural*k100.r
cov.ts$Semi.100k <- cov.ts$Semi*k100.s
cov.ts$Urban.100k <- cov.ts$Urban*k100.u

## Raw Cumulative plot
cdRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("COVID Cumulative Confirmed Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="black", "Semi"="red", 
                                 "Urban"="blue"))
## /100,000 plot
cd100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural.100k"))+
  geom_line(aes(y=Semi.100k, col = "Semi.100k"))+
  geom_line(aes(y=Urban.100k, col = "Urban.100k"))+
  ylab("COVID Cumulative Confirmed Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.100k", "Semi.100k", "Urban.100k"),
                      values = c("Rural.100k"="black", "Semi.100k"="red", 
                                 "Urban.100k"="blue"))

## for the later plots for Lisa's presentation (added Mid March 22)
cd.plot <- cov.ts


## probable + confirmed cases ####
pcc.cum <- prob_conf.cases_TS[,-(ncol(prob_conf.cases_TS))]
for (i in 2:nrow(pcc.cum)){
  pcc.cum[i,] <- pcc.cum[i,] + pcc.cum[i-1,]
}
##add population data and rurality designations (tacks on the last 3 urbans for the KC etc adds)
pcc.dat <- data.frame( pop.cat = c(COVID.data$Pop.Cat,"Urban","Urban","Urban"),
                      population = pops,
                      cts = t(pcc.cum))
## cumulative time series for each rurality group
cov.ts <- rowsum(pcc.dat[,3:ncol(pcc.dat)], group = pcc.dat$pop.cat)
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
## population and case totals
r.pop = as.numeric(sum(pcc.dat$population[pcc.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(pcc.dat$population[pcc.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(pcc.dat$population[pcc.dat$pop.cat=="Urban"]))
cases.tot = as.numeric(sum(pcc.dat[,ncol(pcc.dat)]))
p.tot = as.numeric(sum(pcc.dat$population))
## SMR standardize
E.r = (r.pop*cases.tot)/(p.tot)
E.s = (s.pop*cases.tot)/(p.tot)
E.u = (u.pop*cases.tot)/(p.tot)
cov.ts$Rural.SMR <- cov.ts$Rural/E.r
cov.ts$Semi.SMR <- cov.ts$Semi/E.s
cov.ts$Urban.SMR <- cov.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
cov.ts$Rural.100k <- cov.ts$Rural*k100.r
cov.ts$Semi.100k <- cov.ts$Semi*k100.s
cov.ts$Urban.100k <- cov.ts$Urban*k100.u

## Raw Cumulative plot
pccRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Confirmed + Probable Cases") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="black", "Semi"="red", 
                                 "Urban"="blue"))
## /100,000 plot
pcc100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural.100k"))+
  geom_line(aes(y=Semi.100k, col = "Semi.100k"))+
  geom_line(aes(y=Urban.100k, col = "Urban.100k"))+
  ylab("Cumulative Confirmed + Probable Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.100k", "Semi.100k", "Urban.100k"),
                      values = c("Rural.100k"="black", "Semi.100k"="red", 
                                 "Urban.100k"="blue"))

##probable + confirmed deaths cumulative plot ####
pcd.cum <- prob_conf.deaths_TS[,-(ncol(prob_conf.deaths_TS))]
for (i in 2:nrow(pcd.cum)){
  pcd.cum[i,] <- pcd.cum[i,] + pcd.cum[i-1,]
}
##add population data and rurality designations (tacks on the last 3 urbans for the KC etc adds)
pcd.dat <- data.frame( pop.cat = c(COVID.data$Pop.Cat,"Urban","Urban","Urban"),
                      population = pops,
                      cts = t(pcd.cum))
## cumulative time series for each rurality group
cov.ts <- rowsum(pcd.dat[,3:ncol(pcd.dat)], group = pcd.dat$pop.cat)
rownames(cov.ts) <- list("Rural", "Semi", "Urban")
cov.ts <- as.data.frame(t(cov.ts)) 
cov.ts$date <- seq(from = as.Date("2020-01-01"),
                   to = as.Date("2020-01-01")+nrow(cov.ts)-1, by = 1)
## population and death totals
r.pop = as.numeric(sum(pcd.dat$population[pcd.dat$pop.cat=="Rural"]))
s.pop = as.numeric(sum(pcd.dat$population[pcd.dat$pop.cat=="Semi"]))
u.pop = as.numeric(sum(pcd.dat$population[pcd.dat$pop.cat=="Urban"]))
d.tot = as.numeric(sum(pcd.dat[,ncol(pcd.dat)]))
p.tot = as.numeric(sum(pcd.dat$population))
## SMR standardize
E.r = (r.pop*d.tot)/(p.tot)
E.s = (s.pop*d.tot)/(p.tot)
E.u = (u.pop*d.tot)/(p.tot)
cov.ts$Rural.SMR <- cov.ts$Rural/E.r
cov.ts$Semi.SMR <- cov.ts$Semi/E.s
cov.ts$Urban.SMR <- cov.ts$Urban/E.u
## per 100,000 standardize
k100.r = (100000/r.pop)
k100.s = (100000/s.pop)
k100.u = (100000/u.pop)
cov.ts$Rural.100k <- cov.ts$Rural*k100.r
cov.ts$Semi.100k <- cov.ts$Semi*k100.s
cov.ts$Urban.100k <- cov.ts$Urban*k100.u

## Raw Cumulative plot
pcdRaw <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural, col = "Rural"))+
  geom_line(aes(y=Semi, col = "Semi"))+
  geom_line(aes(y=Urban, col = "Urban"))+
  ylab("Cumulative Confirmed + Probable Deaths") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="black", "Semi"="red", 
                                 "Urban"="blue"))
## /100,000 plot
pcd100k <- ggplot(cov.ts,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural.100k"))+
  geom_line(aes(y=Semi.100k, col = "Semi.100k"))+
  geom_line(aes(y=Urban.100k, col = "Urban.100k"))+
  ylab("Cumulative Confirmed + Probable Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural.100k", "Semi.100k", "Urban.100k"),
                      values = c("Rural.100k"="black", "Semi.100k"="red", 
                                 "Urban.100k"="blue"))

grid.arrange(cc100k,cd100k,pcc100k,pcd100k)




## update the plot legend & title for Lisa's presentation (mid March 2022)
## Cumulative Deaths /100,000 plot

## /100,000 plot
cc.plot.plot <- ggplot(cc.plot,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural"))+
  geom_line(aes(y=Semi.100k, col = "Semi"))+
  geom_line(aes(y=Urban.100k, col = "Urban"))+
  ylab("Cumulative Cases / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="#000000", "Semi"="#E69F00", 
                                 "Urban"="#56B4E9")) +
  labs(title = "COVID Cumulative Cases 2020 to 2022",
       subtitle = "by rurality designation")

cd.plot.plot <- cd100k <- ggplot(cd.plot,aes(x=date))+
  geom_line(aes(y=Rural.100k, col = "Rural"))+
  geom_line(aes(y=Semi.100k, col = "Semi"))+
  geom_line(aes(y=Urban.100k, col = "Urban"))+
  ylab("Cumulative Deaths / 100,000") +
  scale_x_date(breaks = "3 months", date_labels = "%b %y") +
  scale_colour_manual("", 
                      breaks = c("Rural", "Semi", "Urban"),
                      values = c("Rural"="#000000", "Semi"="#E69F00", 
                                 "Urban"="#56B4E9")) +
  labs(title = "COVID Cumulative Confirmed Deaths 2020 to 2022",
       subtitle = "by rurality designation")

grid.arrange(cc.plot.plot, cd.plot.plot, nrow = 2)

Mo.shape.combined <- Mo.shape %>% mutate(Rurality = COVID.data$Pop.Cat)
p2 <- ggplot(data = Mo.shape.combined) +
  geom_sf(aes(fill = Rurality)) +
  scale_fill_manual(values = c("Rural"="#000000", "Semi"="#E69F00", 
                               "Urban"="#56B4E9")) +
  labs(title = "COVID County Rurality Designations") +
  theme(legend.title= element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(p2, cd.plot.plot, ncol = 2)
grid.arrange(p2, cc.plot.plot, ncol = 2)










