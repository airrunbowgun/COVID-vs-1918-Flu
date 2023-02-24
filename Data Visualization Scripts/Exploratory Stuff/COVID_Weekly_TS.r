library(dplyr) 
setwd("C:/Users/Arrun/Desktop/One Drive/OneDrive - University of Missouri/Researchin/COVID NSF project (Summer 21)/Local Data Files")
load("var2020cl.rdata")
load("Mo.shape.rdata")

## COVID Cases Daily Cases
daycase.dat <- as.data.frame(t(readxl::read_excel("COVID_COUNTS_28Jun21.xlsx", sheet = "Cum Daily Cases",
                                             col_names = FALSE)))
day = rbind(as.Date("2020-01-22") + 0:(ncol(daycase.dat)-3))
daycase.dat[1,] <- cbind(daycase.dat[1,1:2],day)
colnames(daycase.dat) <- daycase.dat[1,]
daycase.dat <- daycase.dat[-1,]
daycase.dat[,-c(1:2)] <- lapply(daycase.dat[,-c(1:2)], as.numeric)
## convert the excel, numeric, state+county FIP (29***)
## to 3 digit, character, county FIP (shape file FIPs are formatted this way) 
daycase.dat$FIP <- substring(as.character(daycase.dat[,2]),3)
daycase.dat <- arrange(daycase.dat,daycase.dat$FIP) # Order sa shape & covariate file
which(daycase.dat$FIP != var2020cl$FIP) # quick check to make sure nothing funky is going on with the count data (should == integer(0))

## COVID Cases Daily Deaths
daydeath.dat <- as.data.frame(t(readxl::read_excel("COVID_COUNTS_28Jun21.xlsx", sheet = "Cum Daily Deaths",
                                                  col_names = FALSE)))
colnames(daydeath.dat) <- daydeath.dat[1,]
daydeath.dat <- daydeath.dat[-1,]
daydeath.dat[,-c(1:2)] <- lapply(daydeath.dat[,-c(1:2)], as.numeric)

## convert the excel, numeric, state+county FIP (29***)
## to 3 digit, character, county FIP (shape file FIPs are formatted this way) 
daydeath.dat$FIP <- substring(as.character(daydeath.dat[,2]),3)
daydeath.dat <- arrange(daydeath.dat,daydeath.dat$FIP) # Order sa shape & covariate file
which(daydeath.dat$FIP != var2020cl$FIP) # quick check to make sure nothing funky is going on with the count data (should == integer(0))


# weekly counts from daily cumulative counts ####
# Cases ###
#produces a na matrix with enough space for COMPLETE weeks
wkcase <- matrix(, nrow = 115, ncol = floor((ncol(daycase.dat)-2)/7))

## weekly counts from daily cumulative counts for COMPLETE weeks                 
for (i in 1:floor((ncol(daycase.dat)-2)/7)){
  wkcase[,i] <- daycase.dat[,3+7*i] - daycase.dat[,3+7*(i-1)]
}

#creates a week complete date string for COMPLETE weeks from the start 01/22/20 (1st complete week is 01/29/20)
wkstrt <- as.Date("2020-01-29") + seq(from = 0, to = floor((ncol(daycase.dat)-2)/7-1)*7 , by = 7)

## Haven't been able to marry the date string to the data frame yet :-(
wkcasec <- cbind(wkstrt,t(wkcase))

