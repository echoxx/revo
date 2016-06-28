setwd("~/Google_Drive2/R/Revolutions")

library(haven) #imports DTA with column title descriptions
library(foreign) #does not import column title descriptions
library(dplyr) #for selecting columns

revo <- read_dta("Measuring Revolution.COLGAN.2012Nov.dta")
revo_noTitles <- read.dta("Measuring Revolution.COLGAN.2012Nov.dta")

#Columns to select
## Review use of force vs irregular transition. Paper is not clear about whether use of force is subset of 
## irregular transition, or how they differ.
revo.forcetrimmed <- select(revo, usedforce, irregulartransition)

#All "use force" should also be "irreg", but not all "irreg" will be "usedforce"
irregcheck1 <- revo.forcetrimmed[,1] == 1 & revo.forcetrimmed[,2] == 0
irregcheck2 <- revo.forcetrimmed[,1] == 0 & revo.forcetrimmed[,2] == 1

sum(irregcheck1, na.rm = TRUE) #This should be 0 
sum(irregcheck2, na.rm = TRUE) #This is likely to be nonzero


##DEMOC
##AUTOC
##POLITY2
##DURABLE
##POLITY