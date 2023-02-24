## This will create two Rdata files both of which have the MO shape files included: 
##  1. COVID_data.Rdata 
##      Has all of the 2020 county predictor variables (including the Urban/rural designations) for 115 counties
##      COVID confirmed and confirmed + probable cases and deaths (daily time series designated with "_TS" suffix for 115 counties + !!!KC, Joplin, Independence!!! 
##      AND pandemic total counts in the "COVID.data" dataframe [which also has the predictor variables] for 115 counties)
##      Cumulative confirmed deaths "COVID.death.cum_TS" for 115 counties

##  2. FLU_data.rdata
##       Has all of the 1910 county predictor variables (including the Urban/rural designations) 
##       flu death counts (daily time series designated with "_TS" suffix AND pandemic 
##       total counts in the "FLU.data" dataframe [which also has the predictor variables])
##       Cumulative Flu deaths "FLU.death.cum_TS"

library(tidyverse) 
library(readxl)
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Excel_Files")
## County-level map data ####
Mo.shape <- tigris::counties(state = "Missouri", year = 2019) # get the shapefile
Mo.shape <- arrange(Mo.shape,Mo.shape$COUNTYFP) # order shape file based on FIP
Mo.shape.new <- as(Mo.shape, "Spatial") # to convert to SpatialPolygonsDataFrame
# Centroids for counties
centroids <-rgeos::gCentroid(Mo.shape.new, byid = TRUE, id = Mo.shape.new@data$COUNTYFP) 
centroids.mat <- as.matrix(centroids@coords) # grab the centroid coordinates
centroids.mat <- data.frame(COUNTYFP = rownames(centroids.mat), centroid.lon = centroids.mat[,1], centroid.lat = centroids.mat[,2])

### ### ### ### ### ### ### ###
####  2020 Predictor data   ####
### ### ### ### ### ### ### ###
var.2020.overall <- as.data.frame(read_xlsx("1910 & 2010 Vars-20210613-corrected.xlsx", sheet = "2020 overall",
                                            range = "A1:K116"))
## 2020 rurality group designations (based on updated designations discussed 12/21)
urb.rur <- read.csv("urb_rur_2020_UPDATED.csv")

## convert the excel, numeric, state+county FIP (29***)
## to 3 digit, character, county FIP (shape file FIPs are formatted this way) 
var.2020.overall$COUNTYFP <- substring(as.character(var.2020.overall$FIP),3) 
urb.rur <- mutate(urb.rur[,c(1,3)], COUNTYFP = substring(as.character(countyFIPS),3)) %>%
  arrange(COUNTYFP) %>%
  select(COUNTYFP,pop.cat)
COVID.data <- var.2020.overall %>% 
  arrange(var.2020.overall$COUNTYFP) %>% # Order sa shape file
  select(!(FIP)) %>% # get rid of the original FIP numbers
  mutate(County = Mo.shape$NAME) %>% #county character names exactly match shapefile names
  left_join(urb.rur, by = "COUNTYFP") %>%
  relocate(County,COUNTYFP,pop.cat) 
identical(Mo.shape$COUNTYFP, COVID.data$COUNTYFP) # quick check to make sure nothing funky is going on with the covariate data

## Column names
colnames(COVID.data) <- list("NAME", "COUNTYFP", "Pop.Cat", "TotalPop", 
                             "Density", "Literacy", "Avg.Farm.Val", "PCTMale", 
                             "PCPs100k", "YO.Ratio", "Prop.White", "Prop.Hisp")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
##  Death and case count data (time series AND pandemic totals) ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Daily confirmed cases ####
conf.cases_TS <- as.data.frame(read_xlsx("CasesDeaths-MO dashboard-211209-verified.xlsx", sheet = "confirmed cases",
                                         range = cell_cols("A:DO")))
##create a column for R formatted dates
conf.cases_TS <- mutate(conf.cases_TS[3:nrow(conf.cases_TS),2:ncol(conf.cases_TS)], 
                        Date = seq(from = as.Date("2020-01-01"), to = as.Date(as.Date("2020-01-01") + nrow(conf.cases_TS) - 3), by = 'day'))
##total confirmed cases for each county across time
conf.cases <- as.data.frame(t(colSums(conf.cases_TS[,-(ncol(conf.cases_TS))])))
##combine Joplin, KC, and Independence into their respective counties (based on population percentages !!!!!SEE NOTES IN EXCEL SPREADSHEET!!!!)
conf.cases <- mutate(conf.cases, Jackson = Jackson + Independence,               ## all of independence in Jackson county
                     Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                     Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                     Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                     Jasper = Jasper + Joplin * 0.9,   ## 90% of Joplin in Jasper county
                     Newton = Newton + Joplin * 0.1)   ## 10% of Joplin in Newton county
conf.cases.totals <- round(conf.cases[,1:115],0)

## Daily confirmed + probable cases (!!! Need to adjust the range should additional time points be added to this sheet {row 1 has a heading] !!!) ####
prob_conf.cases_TS <- as.data.frame(read_xlsx("CasesDeaths-MO dashboard-211209-verified.xlsx", sheet = "conf+prob case",
                                              range = "A2:DO712"))
##create a column for R formatted dates
prob_conf.cases_TS <- mutate(prob_conf.cases_TS[2:nrow(prob_conf.cases_TS),2:ncol(prob_conf.cases_TS)], 
                             Date = seq(from = as.Date("2020-01-01"), to = as.Date(as.Date("2020-01-01") + nrow(prob_conf.cases_TS) - 2), by = 'day'))
##total confirmed+probable cases for each county across time
prob_conf.cases <- as.data.frame(t(colSums(prob_conf.cases_TS[,-(ncol(prob_conf.cases_TS))])))
##combine Joplin, KC, and Independence into their respective counties (based on population percentages !!!!!SEE NOTES IN EXCEL SPREADSHEET!!!!)
prob_conf.cases <- mutate(prob_conf.cases, Jackson = Jackson + Independence,   ## all of independence in Jackson county
                          Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                          Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                          Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                          Jasper = Jasper + Joplin * 0.9,   ## % of Joplin in Jasper county
                          Newton = Newton + Joplin * 0.1)   ## % of Joplin in Newton county
prob_conf.cases.totals <- round(prob_conf.cases[,1:115],0)


## Daily confirmed deaths (!!! Need to adjust the range should additional time points be added to this sheet {row 713 has a single entry "total" :-/ )] !!!) ####
conf.deaths_TS <- as.data.frame(read_xlsx("CasesDeaths-MO dashboard-211209-verified.xlsx", sheet = "confirmed deaths",
                                          range = "A1:DO712"))
##create a column for R formatted dates
conf.deaths_TS <- mutate(conf.deaths_TS[3:nrow(conf.deaths_TS),2:ncol(conf.deaths_TS)], 
                         Date = seq(from = as.Date("2020-01-01"), to = as.Date(as.Date("2020-01-01") + nrow(conf.deaths_TS) - 3), by = 'day'))
COVID.death.cum_TS <- mutate(conf.deaths_TS, Jackson = Jackson + Independence,      ## all of independence in Jackson county
                             Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                             Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                             Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                             Jasper = Jasper + Joplin * 0.9,   ## 90% of Joplin in Jasper county
                             Newton = Newton + Joplin * 0.1)   ## 10% of Joplin in Newton county
COVID.death.cum_TS <- round(COVID.death.cum_TS[,1:115],0) ## rounds to whole counts, and removes Indep, KC, and Joplin (and the date column to allow for cumulative daily summation
##cumulative confirmed deaths ####
for (i in 2:nrow(COVID.death.cum_TS)){
  COVID.death.cum_TS[i,] <- COVID.death.cum_TS[i,]+COVID.death.cum_TS[i-1,]
}
## add the date column back on 
COVID.death.cum_TS <- mutate(COVID.death.cum_TS, Date = seq(from = as.Date("2020-01-01"), to = as.Date(as.Date("2020-01-01") + (nrow(conf.deaths_TS)-1)), by = 'day'))

##total confirmed deaths for each county across time
conf.deaths <- as.data.frame(t(colSums(conf.deaths_TS[,-(ncol(conf.deaths_TS))])))
##combine Joplin, KC, and Independence into their respective counties (based on population percentages !!!!!SEE NOTES IN EXCEL SPREADSHEET!!!!)
conf.deaths <- mutate(conf.deaths, Jackson = Jackson + Independence,      ## all of independence in Jackson county
                      Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                      Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                      Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                      Jasper = Jasper + Joplin * 0.9,   ## 90% of Joplin in Jasper county
                      Newton = Newton + Joplin * 0.1)   ## 10% of Joplin in Newton county
conf.deaths.totals <- round(conf.deaths[,1:115],0)

## Daily confirmed + probable deaths (!!! Need to adjust the range should additional time points be added to this sheet {row 1 has a heading] !!!) ####
prob_conf.deaths_TS <- as.data.frame(read_xlsx("CasesDeaths-MO dashboard-211209-verified.xlsx", sheet = "conf+prob dth",
                                               range = "A2:DO712"))
##create a column for R formatted dates
prob_conf.deaths_TS <- mutate(prob_conf.deaths_TS[2:nrow(prob_conf.deaths_TS),2:ncol(prob_conf.deaths_TS)], 
                              Date = seq(from = as.Date("2020-01-01"), to = as.Date(as.Date("2020-01-01") + nrow(prob_conf.deaths_TS) - 2), by = 'day'))
##total confirmed+probable deaths for each county across time
prob_conf.deaths <- as.data.frame(t(colSums(prob_conf.deaths_TS[,-(ncol(prob_conf.deaths_TS))])))
##combine Joplin, KC, and Independence into their respective counties (based on population percentages !!!!!SEE NOTES IN EXCEL SPREADSHEET!!!!)
prob_conf.deaths <- mutate(prob_conf.deaths, Jackson = Jackson + Independence, ## all of independence in Jackson county
                           Clay = Clay + `Kansas City` * (128631/495377),       ## % of KC in Clay county
                           Jackson = Jackson + `Kansas City` * (319781/495377), ## % of KC in Jackson county
                           Platte = Platte + `Kansas City` * (46965/495377),    ## % of KC in Platte county
                           Jasper = Jasper + Joplin * 0.9,   ## % of Joplin in Jasper county
                           Newton = Newton + Joplin * 0.1)   ## % of Joplin in Newton county
prob_conf.deaths.totals <- round(prob_conf.deaths[,1:115],0)

## Create a single data frame for the predictors and the various pandemic total counts
COVID.data <- mutate(COVID.data, conf.cases = as.vector(t(conf.cases.totals)),
                     prob_conf.cases = as.vector(t(prob_conf.cases.totals)),
                     conf.deaths = as.vector(t(conf.deaths.totals)),
                     prob_conf.deaths = as.vector(t(prob_conf.deaths.totals)))
## ## ##
# save(COVID.data, conf.cases_TS, prob_conf.cases_TS, conf.deaths_TS,
#      prob_conf.deaths_TS, Mo.shape, centroids, COVID.death.cum_TS,
#      file = "C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files/COVID_data.Rdata")
# ## ## ##




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
##  1918 Pandemic Death count data (time series AND pandemic totals) ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Import daily death counts from the excel file ####
flu.deaths_TS <- as.data.frame(read_xlsx("1918-1920 Flu and Pneumonia Deaths (orbannc@health.missouri.edu 3).xlsx", sheet = "Cumulative Daily",
                                         range = "B1:DL1098"))
## order counties to match ascending FIPS (as in the MO shape file)
flu.deaths_TS <- flu.deaths_TS[,order(flu.deaths_TS[1,])]

##create a column for R formatted dates
flu.deaths_TS <- mutate(flu.deaths_TS[2:nrow(flu.deaths_TS),], 
                        Date = seq(from = as.Date("1918-01-01"), to = as.Date("1920-12-31"), by = 'day'))
## cumulative COVID death time series for 115 counties##
FLU.death.cum_TS <- select(flu.deaths_TS, !(Date)) ##removes the date column to allow for cumulative daily summation
##cumulative confirmed deaths ####
for (i in 2:nrow(FLU.death.cum_TS)){
  FLU.death.cum_TS[i,] <- FLU.death.cum_TS[i,]+FLU.death.cum_TS[i-1,]
}
## add the date column back on 
FLU.death.cum_TS <- mutate(FLU.death.cum_TS, Date = seq(from = as.Date("1918-01-01"), to = as.Date("1920-12-31"), by = 'day'))


##total deaths for each county across time
flu.deaths <- as.data.frame(t(colSums(flu.deaths_TS[,-(ncol(flu.deaths_TS))])))

### ### ### ### ### ### ### ###
####    1910 predictor data ####
### ### ### ### ### ### ### ###

## 1910 covariate data
var.1910.overall <- as.data.frame(readxl::read_xlsx("1910 & 2010 Vars-20210613-corrected.xlsx", sheet = "1910 overall",range = 'A1:J116'))
## 1910 rurality group designations
urb.rur.1910 <- read.csv("urb_rur_1910.csv")
## convert the excel, numeric, state+county FIP (29***)
## to 3 digit, character, county FIP (shape file FIPs are formatted this way) 
var.1910.overall$COUNTYFP <- substring(as.character(var.1910.overall$FIP),3)
urb.rur.1910 <- mutate(urb.rur.1910[,c(1,3)], COUNTYFP = substring(as.character(ï..countyFIPS),3)) %>%
  arrange(COUNTYFP) %>%
  select(COUNTYFP,pop.cat)
county.preds.1910 <- var.1910.overall %>% 
  arrange(var.1910.overall$COUNTYFP) %>% # Order sa shape file
  select(!(FIP)) %>% # get rid of the original FIP numbers
  mutate(County = Mo.shape$NAME) %>% #county character names exactly match shapefile names
  left_join(urb.rur.1910, by = "COUNTYFP") %>%
  relocate(County,COUNTYFP,pop.cat)  
identical(Mo.shape$COUNTYFP, county.preds.1910$COUNTYFP) # quick check to make sure nothing funky is going on with the covariate data

## add sum total death counts for the entire pandemic
FLU.data <- mutate(county.preds.1910, Total.Deaths = t(flu.deaths))
## Column names
colnames(FLU.data) <- list("NAME", "COUNTYFP", "Pop.Cat", "TotalPop", 
                           "Density", "Literacy", "Avg.Farm.Val", "PCTMale",
                           "PCPs100k", "YO.Ratio", "Prop.White", "Total.Deaths")

## 
# save(FLU.data,Mo.shape,centroids,flu.deaths_TS,FLU.death.cum_TS, file = "C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/2022/Import R and Rdata Files/FLU_data.rdata")
##