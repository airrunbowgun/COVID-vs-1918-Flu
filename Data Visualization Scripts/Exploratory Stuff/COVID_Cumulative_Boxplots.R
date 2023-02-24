setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
library(tidyverse)
C.dth <- read.csv("covid_deaths_31JulCum.csv")
## 9667 total COVID deaths ##
dtot_all <- sum(C.dth$Cum_31Jul)
dtot_apr <- sum(C.dth$Cum_1Apr)
## County data file (for population numbers)
load(file = "var2020cl.rdata")
E_all <- (var2020cl$TotalPop * dtot_all/sum(var2020cl$TotalPop))
E_Apr <- (var2020cl$TotalPop * dtot_apr/sum(var2020cl$TotalPop))
C.dth <- mutate(C.dth, ï..countyFIPS = substring(as.character(C.dth$ï..countyFIPS),3),
                SMR_all = Cum_31Jul/(var2020cl$TotalPop * dtot_all/sum(var2020cl$TotalPop)),
                SMR_Apr = Cum_1Apr/(var2020cl$TotalPop * dtot_apr/sum(var2020cl$TotalPop))
                )

ggplot(C.dth)+
  geom_boxplot(aes(x=pop.cat,y=SMR_Apr))
ggplot(C.dth)+
  geom_boxplot(aes(x=pop.cat,y=SMR_all))