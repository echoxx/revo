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
                       usedforce, irregulartransition,
                       chg_executivepower, chg_politicalideology, chg_nameofcountry, 
                       chg_propertyowernship, chg_womenandethnicstatus, chg_religioningovernment,
                       chg_revolutionarycommittee, totalcategorieschanged,
                       polity, polity2, democ, autoc)
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
##   Creates last_polity/democ/autoc column using na.locf.
##   Side effect from split year+1 / merge technique: Strips out initial state year for all countries, 
##     since need before & after data to calculate polity change 

all_countries <- dt_revo.trimmed

#Create polity carryforward
all_countries[,polity_cens:=ifelse(polity< -10, NA,polity)] #Polity ranges from -10 to +10
all_countries[,polity_carryforward:=na.locf(polity_cens,na.rm=F),by=ccname]

#Create democ carryforward
all_countries[,democ_cens:=ifelse(democ< 0, NA,democ)] #Democ ranges 0 to 10
all_countries[,democ_carryforward:=na.locf(democ_cens,na.rm=F),by=ccname]

#Create autoc carryforward
all_countries[,autoc_cens:=ifelse(autoc < 0, NA,autoc)] #Democ ranges 0 to 10
all_countries[,autoc_carryforward:=na.locf(autoc_cens,na.rm=F),by=ccname]



###Transitions - picks up adjacent leaders/transitions 
##Total length of transition, not length of time by transition by leader
all_countries[,transition_start_end:=c(0,diff(transition)), by = list(ccname)] #---> IS THIS STILL NECESSARY?
all_countries[,transition_start_count:=1L:.N, by = list(ccname, transition, transition_start_end)]
all_countries[,transition_start_count:=ifelse(transition_start_end != 1, 0, transition_start_count)]

tempcounter <- numeric(length(all_countries$ccname))
for(i in seq_along(all_countries$ccname) ) {
  if (is.na(all_countries$transition[i]) | is.na(all_countries$transition_start_count[i])) { 
    tempcounter[i] <- 0
    } else if (all_countries$transition_start_count[i] > 0 && all_countries$transition[i] == T && 
     all_countries$transition_start_end[i] ==1 && all_countries$ccname[i] == all_countries$ccname[i-1]) {
    tempcounter[i] <- all_countries$transition_start_count[i]
    } else if (all_countries$transition_start_count[i] == 0 && all_countries$transition[i] == T && 
             all_countries$transition_start_end[i] == 0 && all_countries$ccname[i] == all_countries$ccname[i-1]) {
    tempcounter[i] <- tempcounter[i-1]
    } else {
    tempcounter[i] <- 0
  }
}
all_countries[,transition_start_count:=tempcounter]

all_countries[, transition_length:=(1L:.N), by = list(ccname, transition_start_count, transition)] #NEED THIS LINE AND ABOVE INE?
all_countries[transition == F, transition_length:= 0] #2nd command

#all_countries[,transition_start_end:=c(0,diff(transition)), by = list(ccname)] #---> IS THIS STILL NECESSARY?



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
prev_country_data <- all_countries[,list(ccname, year = year+1, data_year, last_leader = leader, 
                                         last_revo = revolutionaryleader, last_leader_age0 = age0,
                                         last_polity = polity_carryforward, 
                                         last_democ = democ_carryforward, 
                                         last_autoc = autoc_carryforward,
                                         last_usedforce = usedforce, last_irregulartransition = irregulartransition,
                                         last_transition_length = transition_length)]
all_countries2 <- merge(all_countries, prev_country_data, by = c("ccname", "year"))
all_countries2 <- all_countries2[, list(ccname, year, data_year = data_year.y, leader, age0,
                                        usedforce, irregulartransition,
                                      last_leader, last_leader_age0,
                                      last_usedforce, last_irregulartransition,
                                      revolutionaryleader, last_revo,
                                      polity, last_polity, 
                                      democ, last_democ, 
                                      autoc,last_autoc,
                                      chg_executivepower, chg_politicalideology, chg_nameofcountry, 
                                      chg_propertyowernship, chg_womenandethnicstatus, chg_religioningovernment,
                                      chg_revolutionarycommittee, totalcategorieschanged,
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
tr_condensed[,democ_change:=democ - last_democ]
tr_condensed[,autoc_change:=autoc - last_autoc]





#### SECTION 7: SPLIT, CLEAN, & JOIN REVO & NONREVO-TRANS DTS ####
## Split used to clean NAs & observations with transition polity codes

# Nonrevo transitions only
nonrevotrans <- tr_condensed[revolutionaryleader == 0 & transition_start_end == -1]
removeduplicates <- which(!duplicated(nonrevotrans$leader))
nonrevotrans <- (nonrevotrans[removeduplicates,])
nonrevotrans <- nonrevotrans[!is.na(nonrevotrans$polity & last_polity),] #changed

# Revos only 
allrevos <- tr_condensed[revo_start_end == 1 | (transition_start_end == -1 & revolutionaryleader == 1)]
removeduplicates <- which(!duplicated(allrevos$leader))
allrevos <- allrevos[removeduplicates,]
allrevos <- allrevos[!is.na(allrevos$polity & last_polity),]

# Join & create clean DT
revotrans.clean <- full_join(nonrevotrans, allrevos)





#### SECTION 8: OUTPUTS ####

#Revolutionary leaders with positive  polity/democ/autoc effect
rev_pospol_tally <- sum(allrevos$polity_change > 0)
rev_pospol_mean <- allrevos[polity_change > 0, mean(polity_change)]
rev_pospol_median <- allrevos[polity_change > 0, median(polity_change)]
rev_pospol_mean_length <- allrevos[polity_change > 0,mean(last_transition_length)]
rev_pospol_median_length <- allrevos[polity_change > 0,median(last_transition_length)]

rev_posdem_tally <- sum(allrevos$democ_change > 0)
rev_posdem_mean <- allrevos[democ_change > 0, mean(democ_change)]
rev_posdem_median <- allrevos[democ_change > 0, median(democ_change)]
rev_posdem_mean_length <- allrevos[democ_change > 0,mean(last_transition_length)]
rev_posdem_median_length <- allrevos[democ_change > 0,median(last_transition_length)]

rev_posaut_tally <- sum(allrevos$autoc_change > 0)
rev_posaut_mean <- allrevos[autoc_change > 0, mean(autoc_change)]
rev_posaut_median <- allrevos[autoc_change > 0, median(autoc_change)]
rev_posaut_mean_length <- allrevos[autoc_change > 0,mean(last_transition_length)]
rev_posaut_median_length <- allrevos[autoc_change > 0,median(last_transition_length)]

#Revolutionary leaders with neutral polity/democ/autoc effect
rev_neutpol_tally <- sum(allrevos$polity_change == 0)
rev_neutpol_mean <- allrevos[polity_change == 0, mean(polity_change)]
rev_neutpol_median <- allrevos[polity_change == 0, median(polity_change)]
rev_neutpol_mean_length <- allrevos[polity_change == 0,mean(last_transition_length)]
rev_neutpol_median_length <- allrevos[polity_change == 0,median(last_transition_length)]

rev_neutdem_tally <- sum(allrevos$democ_change == 0)
rev_neutdem_mean <- allrevos[democ_change == 0, mean(democ_change)]
rev_neutdem_median <- allrevos[democ_change == 0, median(democ_change)]
rev_neutdem_mean_length <- allrevos[democ_change == 0,mean(last_transition_length)]
rev_neutdem_median_length <- allrevos[democ_change == 0,median(last_transition_length)]

rev_neutaut_tally <- sum(allrevos$autoc_change == 0)
rev_neutaut_mean <- allrevos[autoc_change == 0, mean(autoc_change)]
rev_neutaut_median <- allrevos[autoc_change == 0, median(autoc_change)]
rev_neutaut_mean_length <- allrevos[autoc_change == 0,mean(last_transition_length)]
rev_neutaut_median_length <- allrevos[autoc_change == 0,median(last_transition_length)]

#Revolutionary leaders with negative polity/democ/autoc effect
rev_negpol_tally <- sum(allrevos$polity_change < 0)
rev_negpol_mean <- allrevos[polity_change < 0, mean(polity_change)]
rev_negpol_median <- allrevos[polity_change < 0, median(polity_change)]
rev_negpol_mean_length <- allrevos[polity_change < 0,mean(last_transition_length)]
rev_negpol_median_length <- allrevos[polity_change < 0,median(last_transition_length)]

rev_negdem_tally <- sum(allrevos$democ_change < 0)
rev_negdem_mean <- allrevos[democ_change < 0, mean(democ_change)]
rev_negdem_median <- allrevos[democ_change < 0, median(democ_change)]
rev_negdem_mean_length <- allrevos[democ_change < 0,mean(last_transition_length)]
rev_negdem_median_length <- allrevos[democ_change < 0,median(last_transition_length)]

rev_negaut_tally <- sum(allrevos$autoc_change < 0)
rev_negaut_mean <- allrevos[autoc_change < 0, mean(autoc_change)]
rev_negaut_median <- allrevos[autoc_change < 0, median(autoc_change)]
rev_negaut_mean_length <- allrevos[autoc_change < 0,mean(last_transition_length)]
rev_negaut_median_length <- allrevos[autoc_change < 0,median(last_transition_length)]

#Nonrevolutionary leaders with positive polity/democ/autoc effect
nonrev_pospol_tally <- sum(nonrevotrans$polity_change > 0)
nonrev_pospol_mean <- nonrevotrans[polity_change > 0, mean(polity_change)]
nonrev_pospol_median <- nonrevotrans[polity_change > 0, median(polity_change)]
nonrev_pospol_mean_length <- nonrevotrans[polity_change > 0,mean(last_transition_length)]
nonrev_pospol_median_length <- nonrevotrans[polity_change > 0,median(last_transition_length)]

nonrev_posdem_tally <- sum(nonrevotrans$democ_change > 0)
nonrev_posdem_mean <- nonrevotrans[democ_change > 0, mean(democ_change)]
nonrev_posdem_median <- nonrevotrans[democ_change > 0, median(democ_change)]
nonrev_posdem_mean_length <- nonrevotrans[democ_change > 0,mean(last_transition_length)]
nonrev_posdem_median_length <- nonrevotrans[democ_change > 0,median(last_transition_length)]

nonrev_posaut_tally <- sum(nonrevotrans$autoc_change > 0)
nonrev_posaut_mean <- nonrevotrans[autoc_change > 0, mean(autoc_change)]
nonrev_posaut_median <- nonrevotrans[autoc_change > 0, median(autoc_change)]
nonrev_posaut_mean_length <- nonrevotrans[autoc_change > 0, mean(last_transition_length)]
nonrev_posaut_median_length <- nonrevotrans[autoc_change > 0,median(last_transition_length)]

#Nonrevolutionary leaders with neutral polity/democ/autoc effect
nonrev_neutpol_tally <- sum(nonrevotrans$polity_change == 0)
nonrev_neutpol_mean <- nonrevotrans[polity_change == 0, mean(polity_change)]
nonrev_neutpol_median <- nonrevotrans[polity_change == 0, median(polity_change)]
nonrev_neutpol_mean_length <- nonrevotrans[polity_change == 0,mean(last_transition_length)]
nonrev_neutpol_median_length <- nonrevotrans[polity_change == 0,median(last_transition_length)]

nonrev_neutdem_tally <- sum(nonrevotrans$democ_change == 0)
nonrev_neutdem_mean <- nonrevotrans[democ_change == 0, mean(democ_change)]
nonrev_neutdem_median <- nonrevotrans[democ_change == 0, median(democ_change)]
nonrev_neutdem_mean_length <- nonrevotrans[democ_change == 0,mean(last_transition_length)]
nonrev_neutdem_median_length <- nonrevotrans[democ_change == 0,median(last_transition_length)]

nonrev_neutaut_tally <- sum(nonrevotrans$autoc_change == 0)
nonrev_neutaut_mean <- nonrevotrans[autoc_change == 0, mean(autoc_change)]
nonrev_neutaut_median <- nonrevotrans[autoc_change == 0, median(autoc_change)]
nonrev_neutaut_mean_length <- nonrevotrans[autoc_change == 0, mean(last_transition_length)]
nonrev_neutaut_median_length <- nonrevotrans[autoc_change == 0,median(last_transition_length)]


#Nonrevolutionary leaders with negative polity/democ/autoc effect
nonrev_negpol_tally <- sum(nonrevotrans$polity_change < 0)
nonrev_negpol_mean <- nonrevotrans[polity_change < 0, mean(polity_change)]
nonrev_negpol_median <- nonrevotrans[polity_change < 0, median(polity_change)]
nonrev_negpol_mean_length <- nonrevotrans[polity_change < 0,mean(last_transition_length)]
nonrev_negpol_median_length <- nonrevotrans[polity_change < 0,median(last_transition_length)]

nonrev_negdem_tally <- sum(nonrevotrans$democ_change < 0)
nonrev_negdem_mean <- nonrevotrans[democ_change < 0, mean(democ_change)]
nonrev_negdem_median <- nonrevotrans[democ_change < 0, median(democ_change)]
nonrev_negdem_mean_length <- nonrevotrans[democ_change < 0,mean(last_transition_length)]
nonrev_negdem_median_length <- nonrevotrans[democ_change < 0,median(last_transition_length)]

nonrev_negaut_tally <- sum(nonrevotrans$autoc_change < 0)
nonrev_negaut_mean <- nonrevotrans[autoc_change < 0, mean(autoc_change)]
nonrev_negaut_median <- nonrevotrans[autoc_change < 0, median(autoc_change)]
nonrev_negaut_mean_length <- nonrevotrans[autoc_change < 0, mean(last_transition_length)]
nonrev_negaut_median_length <- nonrevotrans[autoc_change < 0,median(last_transition_length)]

#Construct polity_summary matrix
polity_summary <- matrix(c(rev_pospol_tally, rev_neutpol_tally, rev_negpol_tally,
                           rev_pospol_mean, rev_neutpol_mean, rev_negpol_mean,
                           rev_pospol_median, rev_neutpol_median, rev_negpol_median,
                           rev_pospol_mean_length, rev_neutpol_mean_length, rev_negpol_mean_length,
                           rev_pospol_median_length, rev_neutpol_median_length, rev_negpol_median_length,
                           nonrev_pospol_tally, nonrev_neutpol_tally, nonrev_negpol_tally,
                           nonrev_pospol_mean, nonrev_neutpol_mean, nonrev_negpol_mean,
                           nonrev_pospol_median, nonrev_neutpol_median, nonrev_negpol_median,
                           nonrev_pospol_mean_length, nonrev_neutpol_mean_length, nonrev_negpol_mean_length,
                           nonrev_pospol_median_length, nonrev_neutpol_median_length, nonrev_negpol_median_length), nrow=2, ncol=15, byrow = TRUE)

rownames(polity_summary) <- c("Revolutionary", "Non-Revolutionary")
colnames(polity_summary) <- c("Pos_polity_tally", "Neut_polity_tally", "Neg_polity_tally", 
                              "Pos_polity_mean",  "Neut_polity_mean", "Neg_polity_mean", 
                              "Pos_polity_median", "Neut_polity_median", "Neg_polity_median", 
                              "Pos_polity_mean_length", "Neut_polity_mean_length", "Neg_polity_mean_lengh",
                              "Pos_polity_median_length", "Neut_polity_median_length", "Neg_polity_median_length")   

#Construct democ_summary matrix
democ_summary <- matrix(c(rev_posdem_tally, rev_neutdem_tally, rev_negdem_tally,
                           rev_posdem_mean, rev_neutdem_mean, rev_negdem_mean,
                           rev_posdem_median, rev_neutdem_median, rev_negdem_median,
                           rev_posdem_mean_length, rev_neutdem_mean_length, rev_negdem_mean_length,
                           rev_posdem_median_length, rev_neutdem_median_length, rev_negdem_median_length,
                           nonrev_posdem_tally, nonrev_neutdem_tally, nonrev_negdem_tally,
                           nonrev_posdem_mean, nonrev_neutdem_mean, nonrev_negdem_mean,
                           nonrev_posdem_median, nonrev_neutdem_median, nonrev_negdem_median,
                           nonrev_posdem_mean_length, nonrev_neutdem_mean_length, nonrev_negdem_mean_length,
                           nonrev_posdem_median_length, nonrev_neutdem_median_length, nonrev_negdem_median_length), nrow=2, ncol=15, byrow = TRUE)

rownames(democ_summary) <- c("Revolutionary", "Non-Revolutionary")
colnames(democ_summary) <- c("Pos_democ_tally", "Neut_democ_tally", "Neg_democ_tally", 
                              "Pos_democ_mean",  "Neut_democ_mean", "Neg_democ_mean", 
                              "Pos_democ_median", "Neut_democ_median", "Neg_democ_median", 
                              "Pos_democ_mean_length", "Neut_democ_mean_length", "Neg_democ_mean_lengh",
                              "Pos_democ_median_length", "Neut_democ_median_length", "Neg_democ_median_length")   

#Construct autoc_summary matrix
autoc_summary <- matrix(c(rev_posaut_tally, rev_neutaut_tally, rev_negaut_tally,
                          rev_posaut_mean, rev_neutaut_mean, rev_negaut_mean,
                          rev_posaut_median, rev_neutaut_median, rev_negaut_median,
                          rev_posaut_mean_length, rev_neutaut_mean_length, rev_negaut_mean_length,
                          rev_posaut_median_length, rev_neutaut_median_length, rev_negaut_median_length,
                          nonrev_posaut_tally, nonrev_neutaut_tally, nonrev_negaut_tally,
                          nonrev_posaut_mean, nonrev_neutaut_mean, nonrev_negaut_mean,
                          nonrev_posaut_median, nonrev_neutaut_median, nonrev_negaut_median,
                          nonrev_posaut_mean_length, nonrev_neutaut_mean_length, nonrev_negaut_mean_length,
                          nonrev_posaut_median_length, nonrev_neutaut_median_length, nonrev_negaut_median_length), nrow=2, ncol=15, byrow = TRUE)

rownames(autoc_summary) <- c("Revolutionary", "Non-Revolutionary")
colnames(autoc_summary) <- c("Pos_autoc_tally", "Neut_autoc_tally", "Neg_autoc_tally", 
                             "Pos_autoc_mean",  "Neut_autoc_mean", "Neg_autoc_mean", 
                             "Pos_autoc_median", "Neut_autoc_median", "Neg_autoc_median", 
                             "Pos_autoc_mean_length", "Neut_autoc_mean_length", "Neg_autoc_mean_lengh",
                             "Pos_autoc_median_length", "Neut_autoc_median_length", "Neg_autoc_median_length") 



#### SECTION 9: CHARTS ####


##Both revos & non revos
#Scatterplots
ggplot(revotrans.clean, aes(x = last_polity, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()

ggplot(revotrans.clean, aes(x = last_democ, y = last_autoc)) + geom_point()

ggplot(revotrans.clean, aes(x = last_democ, y = democ_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_democ, y = autoc_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_democ, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()

ggplot(revotrans.clean, aes(x = last_autoc, y = autoc_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_autoc, y = democ_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_autoc, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()


ggplot(revotrans.clean, (aes(x = last_transition_length, y = polity_change, color = revolutionaryleader))) + geom_point()
ggplot(revotrans.clean,aes(x=usedforce,y=polity_change))+geom_point()
ggplot(allrevos,aes(x=usedforce,y=polity_change))+geom_point()
ggplot(nonrevotrans,aes(x=usedforce,y=polity_change))+geom_point()

ggplot(revotrans.clean,(aes(x = democ_change, y = autoc_change))) + geom_point()
ggplot(revotrans.clean,(aes(x = democ_change, y = autoc_change))) + geom_smooth()


##Revos only
ggplot(allrevos,aes(x=usedforce,y=polity_change))+geom_point()
#ggplot(allrevos, aes(x = chg_executivepower, y = polity_change)) + geom_count()
#ggplot(allrevos, aes(x = chg_politicalideology, y = polity_change)) + geom_count()

#Revos - Scatterplots
ggplot(allrevos, aes(x = polity_change, y = totalcategorieschanged)) + geom_point()
ggplot(allrevos, aes(x = polity, y = polity_change)) + geom_point()
ggplot(allrevos, aes(x = last_transition_length, y = polity_change)) + geom_point()

#Revos - Density plots
ggplot(allrevos, aes(x = last_polity)) + geom_density()
ggplot(allrevos, aes(x = polity)) + geom_density()

ggplot(allrevos, aes(x = last_democ)) + geom_density()
ggplot(allrevos, aes(x = democ)) + geom_density()

ggplot(allrevos, aes(x = last_autoc)) + geom_density()
ggplot(allrevos, aes(x = autoc)) + geom_density()

ggplot(allrevos, aes(x = polity_change)) + geom_density()
ggplot(allrevos, aes(x = democ_change)) + geom_density()
ggplot(allrevos, aes(x = autoc_change)) + geom_density()

#Transition length
ggplot(allrevos, aes(x = last_transition_length)) + geom_density()


#Revos - No clear relationship
ggplot(allrevos, aes(x = last_polity, y = polity_change, color = totalcategorieschanged)) + geom_point()


#Nonrevos only
ggplot(nonrevotrans,aes(x=usedforce,y=polity_change))+geom_point()

ggplot(nonrevotrans, aes(x = polity_change)) + geom_density()
ggplot(nonrevotrans, aes(x = last_transition_length)) + geom_density()
ggplot(nonrevotrans, aes(x = last_transition_length, y = polity_change)) + geom_point()

ggplot(nonrevotrans, aes(x = last_polity)) + geom_density()
ggplot(nonrevotrans, aes(x = polity)) + geom_density()
ggplot(nonrevotrans, aes(x = polity_change)) + geom_density()
ggplot(nonrevotrans, aes(x = last_transition_length)) + geom_density()


#ggplot(allrevos, aes(x = last_polity, y = polity_change)) + geom_smooth()
#ggplot(allrevos, aes(x = polity_change, y = last_transition_length)) + geom_point()

