#### SECTION HEADING S####
##Descriptions for sections
#Descriptions for individual lines



#### SECTION 0: SETWD, LIBRARIES ####
setwd("~/Google_Drive2/R/Revolutions")

library(data.table)
library(ggplot2)
library(haven) #imports DTA with column title descriptions
library(foreign) #does not import column title descriptions
library(dplyr) #for selecting columns
library(zoo) #lvcf



#### SECTION 1: IMPORT & TRIM DATA ####
revo <- read_dta("Measuring Revolution.COLGAN.2012Nov.dta")

# Columns to select
revo.trimmed <- select(revo, ccname, year, startdate, enddate, leader, age, age0,
                       revolutionaryleader,
                       polity, polity2, durable)
# Order by country name, then year
revo.trimmed <- revo.trimmed[order(revo.trimmed$ccname, revo.trimmed$year),]                       




#### SECTION 2: ADD YEAR & TRANSITION-RELATED COLUMNS & TAGS####
## Create data table & and new columns. 
# Transition column defined by Standardized Authority Scores (see polity code book)                                            
# Year_diff column used to create final year tag
# Which year of data, subset by country 

dt_revo.trimmed <- data.table(revo.trimmed)
dt_revo.trimmed[,transition := polity < -10, by = ccname]
dt_revo.trimmed[,year_diff:=c(0,diff(year)),by=list(ccname)] 
dt_revo.trimmed[,data_year:=seq_along(year),by=list(ccname)] 

# Create final year tag for last year of state years.
dt_revo.trimmed[,last_ccyear:=ifelse( shift(year_diff, n = 1L, type = "lead") == 0, 1, 0) ]
dt_revo.trimmed$last_ccyear[nrow(dt_revo.trimmed)] <- 1 #Otherwise, final value is NA since there is not leading row 



#### SECTION 3: ADD POLITY CARRYFORWARD & TRANSITION START/END TAG COLUMNS ####
##   Creates last_polity column using na.locf.
##   Side effect from split year+1 / merge technique: Strips out initial state year for all countries, 
##     since need before & after data to calculate polity change 

all_countries <- dt_revo.trimmed
all_countries[,polity_cens:=ifelse(polity< -10, NA,polity)]
all_countries[,polity_carryforward:=na.locf(polity_cens,na.rm=F),by=ccname]

#Only picks up transitions by leaders, not adjacent transitions by multiple leaders
###ATTEMPT TO PICK UP ADJACENT LEADERS/TRANSITIONS
all_countries[,transition_start_end:=c(0,diff(transition)), by = list(ccname)] #---> IS THIS STILL NECESSARY?
all_countries[,transition_start_count:=1L:.N, by = list(ccname, transition, transition_start_end)]
all_countries[, transition_length:=(1L:.N), by = list()]

###TEST
test <- all_countries2[c( "Afghanistan", "Cambodia")]

test[,transition_start_count:=1L:.N, by = list(ccname, transition, transition_start_end)] #This works
test[,transition_start_count:=ifelse(transition_start_end != 1, 0, transition_start_count)] #Tallys # of new transition starts

tempcounter <- numeric(length(test$ccname))
for(i in seq_along(test$ccname) ) {
  if(test$transition_start_count[i] > 0 && test$transition[i] == T && test$transition_start_end[i] ==1 
     && test$ccname[i] == test$ccname[i-1]) {
    tempcounter[i] <- test$transition_start_count[i]
  } else if (test$transition_start_count[i] == 0 && test$transition[i] == T && test$transition_start_end[i] == 0 
             && test$ccname[i] == test$ccname[i-1]) {
    tempcounter[i] <- tempcounter[i-1]
  } else {
    tempcounter[i] <- 0
  }
}

test[,pleasework:=tempcounter]

test[, transition_length:=(1L:.N), by = list(ccname, transition_start_count, transition)] #NEED THIS LINE AND ABOVE INE?
test[transition == F, transition_length:= 0] #2nd command



test_index <- which ( test[,transition_start_end == 1] )
test[test_index[1]:test_index[2]-1,transition_start_count:=transition_start_count[test_index[1]]]
test[test_index[1]:test_index[2], year]

test[,transition_start_count[test_index[1]:(test_index[2]-1)]] #subsets correctly
test$transition_start_count[test_index[1]:(test_index[2]-1)] <- test$transition_start_count[test_index[1]] # how to generalize?

#test[, transition_start_count:=ifelse(transition_start_end == 1, (1L:.N), 0), by = list(transition_start_end)] 
#test[, transition_length:=(1L:.N), by = list(leader, transition)] 
###END TEST

###OLD CODE
all_countries[, ':='(transition_length = 1L:.N), by = list(leader, transition)] 

all_countries[, transition_length:=ifelse(transition == F, 0, transition_length)]
all_countries[,transition_start_end:=c(0,diff(transition)), by = list(ccname)] #---> IS THIS STILL NECESSARY?
###END OLD CODE



#### SECTION 4: REVO_START_END TAG DEFINITIONS ####
##  Note: Revo_start_end def: IF ==1, revo start, if == -1 revo end. 
##  If == 2, end one revoleader & start another in same year.

# Trigger 1:  general cases
all_countries[,revo_start_end:=c(0,diff(revolutionaryleader)), by = list(ccname)]

# Trigger 2: two adjacent revolutionary leaders from same country. 
# See: Burkina Faso, Sankara/Campaore
all_countries[,revo_start_end:=ifelse(revolutionaryleader == shift(revolutionaryleader, n=1L, type = "lag") &
                                        revolutionaryleader == 1 & 
                                        leader != shift(leader, n=1L, type = "lag"),2,revo_start_end), by = ccname]

# Trigger 3: final country year
all_countries[,revo_start_end:=ifelse(revolutionaryleader == 1 & last_ccyear ==1,-1,revo_start_end)]
                                        


#### SECTION 5: CREATE & MERGE PREV-YEAR COUNTRY DATA WITH EXISTING DATA TABLE ####

# Creates previous countr dt (year+1) to merge, in order to have last year figures in same rows
prev_country_data <- all_countries[,list(ccname, year = year+1, data_year, last_leader = leader, last_revo = revolutionaryleader, 
                                         last_leader_age0 = age0,
                                         last_polity = polity_carryforward, 
                                         last_transition_length = transition_length)]
all_countries2 <- merge(all_countries, prev_country_data, by = c("ccname", "year"))
all_countries2 <- all_countries2[, list(ccname, year, data_year = data_year.y, leader, last_leader, last_leader_age0,
                                      revolutionaryleader, last_revo, 
                                      polity, last_polity, durable,
                                      transition, last_transition_length, 
                                      revo_start_end, transition_start_end)]

#  Hardcoding this since original dataset excludes year 1979 for Zimbabwe, 
#   which excludes Zimbabwe in merging process above.
all_countries2$revo_start_end[7360] <- 1 



#### SECTION 6: CREATE CONDENSED TRANSITION/REVO DT ####
##  Only include years where revo starts or transition ends

# Sets all NAs to 0 in transition tag
#index_na <- which ( all_countries2[,is.na(revo_start_end) | is.na(transition_start_end)] )
#all_countries2$transition_start_end[index_na] <- 0 

# Selects all 1) end of transitions (-1), 2) start of revos(2) and 3) start/end of revos (2)
index_tr <- which ( all_countries2[,transition_start_end == -1 | revo_start_end == 1 | revo_start_end == 2] )
tr_condensed <- data.table(all_countries2[index_tr])

# Excludes observations with standardized authority codes (-88, -77, -66)
tr_condensed <- tr_condensed[polity >= -10,]

# Changes revo_start_end tags of 2 to 1 for plotting purposes
index_2to1 <- which (tr_condensed[,revo_start_end == 2]) 
tr_condensed$revo_start_end[index_2to1] <- 1

# sets arbitrary number below 1 for ggplot to show relative sizes
index_0topt5 <- which (tr_condensed[,last_transition_length == 0])
tr_condensed$last_transition_length[index_0topt5] <- 0.5 

# creates polity change column
tr_condensed[,polity_change:=polity - last_polity]



#### SECTION 7: SPLIT, CLEAN, & JOIN REVO & NONREVO-TRANS DTS ####
## Split used to clean NAs & observations with transition polity codes

# Nonrevo transitions only
nonrevotrans <- tr_condensed[revolutionaryleader == 0 & transition_start_end == -1]
removeduplicates <- which(!duplicated(nonrevotrans$leader))
nonrevotrans <- (nonrevotrans[removeduplicates,])
nonrevotrans <- nonrevotrans[complete.cases(nonrevotrans),]

# Revos only 
allrevos <- tr_condensed[revo_start_end == 1 | (transition_start_end == -1 & revolutionaryleader == 1)]
removeduplicates <- which(!duplicated(allrevos$leader))
allrevos <- allrevos[removeduplicates,]
allrevos <- allrevos[complete.cases(allrevos),]

# Join & create clean DT
revotrans.clean <- full_join(nonrevotrans, allrevos)
revotrans.clean.trimmed <- select(revotrans.clean, ccname, year, leader, last_leader, revolutionaryleader,
                                                 last_revo, polity, last_polity, polity_change,
                                                 last_transition_length, revo_start_end, transition_start_end)


#### SECTION 8: CHARTS ####
ggplot(revotrans.clean, aes(x = last_polity, y = polity_change, color = revo_start_end, size = last_transition_length)) + geom_point()



#### SECTION 9: OUTPUTS ####
