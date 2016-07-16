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
allrevos <- tr_condensed[revo_start_end == 1 | (transition_start_end == -1 & revolutionaryleader == 1)] ###Might only need 1 tag here###
removeduplicates <- which(!duplicated(allrevos$leader))
allrevos <- allrevos[removeduplicates,]
allrevos <- allrevos[!is.na(allrevos$polity & last_polity),]

# Join & create clean DT
revotrans.clean <- data.table ( full_join(nonrevotrans, allrevos) )





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


#Colgan's Variable 1
##REVOS
revos_usedforce_total_count <- sum(allrevos$usedforce, na.rm = T)
revos_noforce_total_count <- sum(allrevos$usedforce == 0, na.rm = T)
revos_usedforce_total_mean <- allrevos[usedforce == 1, mean(polity_change, na.rm = T)]
revos_noforce_total_mean <- allrevos[usedforce == 0, mean(polity_change, na.rm = T)]

revos_usedforce_pospol_count <- allrevos[polity_change > 0, sum(usedforce, na.rm = T)]
revos_noforce_pospol_count <- allrevos[polity_change >0, sum(!usedforce, na.rm = T) ]
revos_usedforce_pospol_mean <- allrevos[polity_change > 0 & usedforce == 1, mean(polity_change, na.rm = T)]
revos_noforce_pospol_mean <- allrevos[polity_change > 0 & usedforce == 0, mean(polity_change, na.rm = T)]

revos_usedforce_neutpol_count <- allrevos[polity_change == 0, sum(usedforce, na.rm = T)]
revos_noforce_neutpol_count <- allrevos[polity_change == 0, sum(!usedforce, na.rm = T)]
revos_usedforce_neutpol_mean <- allrevos[polity_change == 0 & usedforce == 1, mean(polity_change, na.rm = T)]
revos_noforce_neutpol_mean <- allrevos[polity_change == 0 & usedforce == 0, mean(polity_change, na.rm = T)]

revos_usedforce_negpol_count <- allrevos[polity_change < 0, sum(usedforce, na.rm = T)]
revos_noforce_negpol_count <- allrevos[polity_change < 0, sum(!usedforce, na.rm = T), ]
revos_usedforce_negpol_mean <- allrevos[polity_change < 0 & usedforce == 1, mean(polity_change, na.rm = T)]
revos_noforce_negpol_mean <- allrevos[polity_change < 0 & usedforce == 0, mean(polity_change, na.rm = T)]

##NONREVOS
trans_usedforce_total_count <- sum(nonrevotrans$usedforce, na.rm = T)
trans_noforce_total_count <- sum(nonrevotrans$usedforce == 0, na.rm = T)
trans_usedforce_total_mean <- nonrevotrans[usedforce == 1, mean(polity_change, na.rm = T)]
trans_noforce_total_mean <- nonrevotrans[usedforce == 0, mean(polity_change, na.rm = T)]

trans_usedforce_pospol_count <- nonrevotrans[polity_change > 0, sum(usedforce, na.rm = T)]
trans_noforce_pospol_count <- nonrevotrans[polity_change > 0, sum(!usedforce, na.rm = T)]
trans_usedforce_pospol_mean <- nonrevotrans[polity_change > 0 & usedforce == 1, mean(polity_change, na.rm = T)]
trans_noforce_pospol_mean <- nonrevotrans[polity_change > 0 & usedforce == 0, mean(polity_change, na.rm = T)]

trans_usedforce_neutpol_count <- nonrevotrans[polity_change == 0, sum(usedforce, na.rm = T)]
trans_noforce_neutpol_count <- nonrevotrans[polity_change == 0, sum(!usedforce, na.rm = T)]
trans_usedforce_neutpol_mean <- nonrevotrans[polity_change == 0 & usedforce == 1, mean(polity_change, na.rm = T)]
trans_noforce_neutpol_mean <- nonrevotrans[polity_change == 0 & usedforce == 0, mean(polity_change, na.rm = T)]

trans_usedforce_negpol_count <- nonrevotrans[polity_change < 0, sum(usedforce, na.rm = T)]
trans_noforce_negpol_count <- nonrevotrans[polity_change < 0, sum(!usedforce, na.rm = T)]
trans_usedforce_negpol_mean <- nonrevotrans[polity_change < 0 & usedforce == 1, mean(polity_change, na.rm = T)]
trans_noforce_negpol_mean <- nonrevotrans[polity_change < 0 & usedforce == 0, mean(polity_change, na.rm = T)]


usedforce_summary <- matrix(c(revos_usedforce_total_count, revos_noforce_total_count, revos_usedforce_total_mean, revos_noforce_total_mean,
                              revos_usedforce_pospol_count, revos_noforce_pospol_count, revos_usedforce_pospol_mean, revos_noforce_pospol_mean,
                              revos_usedforce_neutpol_count, revos_noforce_neutpol_count, revos_usedforce_neutpol_mean, revos_noforce_neutpol_mean,
                              revos_usedforce_negpol_count, revos_noforce_negpol_count, revos_usedforce_negpol_mean, revos_noforce_negpol_mean,
                              trans_usedforce_total_count, trans_noforce_total_count, trans_usedforce_total_mean, trans_noforce_total_mean,
                              trans_usedforce_pospol_count, trans_noforce_pospol_count, trans_usedforce_pospol_mean, trans_noforce_pospol_mean,
                              trans_usedforce_neutpol_count, trans_usedforce_neutpol_count, trans_usedforce_neutpol_mean, trans_noforce_neutpol_mean,
                              trans_usedforce_negpol_count, trans_usedforce_negpol_count, trans_usedforce_negpol_mean, trans_noforce_negpol_mean), nrow = 2, ncol = 16, byrow = T)
rownames(usedforce_summary) <- c("Revolutionary", "Non-Revolutionary")
colnames(usedforce_summary) <- c("Usedforce_total_count", "Noforce_total_count", "Usedforce_total_mean", "Noforce_total_mean",
                                 "Usedforce_pospol_count", "Noforce_pospol_count", "Usedforce_pospol_mean", "noforce_pospol_mean",
                                 "Usedforce_neutpol_count", "Noforce_neutpol_count", "Usedforce_neutpol_mean", "Noforce_neutpol_mean",
                                 "Usedforce_negpol_count", "Noforce_negpol_count", "Usedforce_negpol_mean", "Noforce_negpol_mean")
                                 


#Colgan's Variable 2 
##REVOS
#Total categories
total_categories_3 <- tally(allrevos[totalcategorieschanged == 3])
total_categories_3.5 <- tally(allrevos[totalcategorieschanged == 3.5])
total_categories_4 <- tally(allrevos[totalcategorieschanged == 4])
total_categories_4.5 <- tally(allrevos[totalcategorieschanged == 4.5])
total_categories_5 <- tally(allrevos[totalcategorieschanged == 5])
total_categories_5.5 <- tally(allrevos[totalcategorieschanged == 5.5])
total_categories_6 <- tally(allrevos[totalcategorieschanged == 6])
total_categories_6.5 <- tally(allrevos[totalcategorieschanged == 6.5])
total_categories_7 <- tally(allrevos[totalcategorieschanged == 7])

#1. chg_executivepower
all_executivepower_1 <- tally(revotrans.clean[chg_executivepower == 1])
all_executivepower_0 <- tally(revotrans.clean[chg_executivepower == 0])
all_executivepower_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_executivepower)
all_executivepower_ttest <- t.test(polity_change ~ chg_executivepower, revotrans.clean)
all_executivepower_wtest <- wilcox.test(polity_change ~ chg_executivepower, revotrans.clean)

revo_executivepower_1 <- tally(allrevos[chg_executivepower == 1])
revo_executivepower_0 <- tally(allrevos[chg_executivepower == 0])
revo_executivepower_ctest <- cor.test(allrevos$polity_change, allrevos$chg_executivepower)
revo_executivepower_ttest <- t.test(polity_change ~ chg_executivepower, allrevos)
revo_executivepower_wtest <- wilcox.test(polity_change ~ chg_executivepower, allrevos)

nonrevo_executivepower_1 <- tally(nonrevotrans[chg_executivepower == 1])
nonrevo_executivepower_0 <- tally(nonrevotrans[chg_executivepower == 0])
nonrevo_executivepower_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_executivepower)
nonrevo_executivepower_ttest <- t.test(nonrevotrans$polity_change, nonrevotrans$chg_executivepower)
nonrevo_executivepower_wtest <- wilcox.test(nonrevotrans$polity_change, nonrevotrans$chg_executivepower)
#ggplot(na.omit(nonrevotrans), aes(x = factor(chg_executivepower), y = polity_change)) + geom_boxplot()

#2. chg_politicalideology
all_politicalideology_1 <- tally(revotrans.clean[chg_politicalideology == 1])
all_politicalideology_0 <- tally(revotrans.clean[chg_politicalideology == 0])
all_politicalideology_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_politicalideology)
all_politicalideology_ttest <- t.test(polity_change ~ chg_politicalideology, revotrans.clean[chg_politicalideology == 1 | chg_politicalideology == 0])
all_politicalideology_wtest <- wilcox.test(polity_change ~ chg_politicalideology, revotrans.clean[chg_politicalideology == 1 | chg_politicalideology == 0])

revo_politicalideology_1 <- tally(allrevos[chg_politicalideology == 1])
revo_politicalideology_0 <- tally(allrevos[chg_politicalideology == 0])
revo_politicalideology_ctest <- cor.test(allrevos$polity_change, allrevos$chg_politicalideology)
revo_politicalideology_ttest <- t.test(polity_change ~ chg_politicalideology, allrevos[chg_politicalideology == 1 | chg_politicalideology == 0])
revo_politicalideology_wtest <- wilcox.test(polity_change ~ chg_politicalideology, allrevos[chg_politicalideology == 1 | chg_politicalideology == 0])

nonrevo_politicalideology_1 <- tally(nonrevotrans[chg_politicalideology == 1])
nonrevo_politicalideology_0 <- tally(nonrevotrans[chg_politicalideology == 0])
nonrevo_politicalideology_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_politicalideology)
nonrevo_politicalideology_ttest <- t.test(polity_change ~ chg_politicalideology, nonrevotrans[chg_politicalideology == 1 | chg_politicalideology == 0])
nonrevo_politicalideology_wtest <- wilcox.test(polity_change ~ chg_politicalideology, nonrevotrans[chg_politicalideology == 1 | chg_politicalideology == 0])

#3. chg_nameofcountry
all_nameofcountry_1 <- tally(revotrans.clean[chg_nameofcountry == 1])
all_nameofcountry_0 <- tally(revotrans.clean[chg_nameofcountry == 0])
all_nameofcountry_ctest <- cor.test(revotrans.cleans$polity_change, revotrans.clean$chg_nameofcountry)
all_nameofcountry_ttest <- t.test(polity_change ~ chg_nameofcountry, revotrans.clean[chg_nameofcountry == 1 | chg_nameofcountry == 0])
all_nameofcountry_wtest <- wilcox.test(polity_change ~ chg_nameofcountry, revotrans.clean[chg_nameofcountry == 1 | chg_nameofcountry == 0])

revo_nameofcountry_1 <- tally(allrevos[chg_nameofcountry == 1])
revo_nameofcountry_0 <- tally(allrevos[chg_nameofcountry == 0])
revo_nameofcountry_ctest <- cor.test(allrevos$polity_change, allrevos$chg_nameofcountry)
revo_nameofcountry_ttest <- t.test(polity_change ~ chg_nameofcountry, allrevos[chg_nameofcountry == 1 | chg_nameofcountry == 0])
revo_nameofcountry_wtest <- wilcox.test(polity_change ~ chg_nameofcountry, allrevos[chg_nameofcountry == 1 | chg_nameofcountry == 0])

nonrevo_nameofcountry_1 <- tally(nonrevotrans[chg_nameofcountry == 1])
nonrevo_nameofcountry_0 <- tally(nonrevotrans[chg_nameofcountry == 0])
nonrevo_nameofcountry_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_nameofcountry)
nonrevo_nameofcountry_ttest <- t.test(polity_change ~ chg_nameofcountry, nonrevotrans[chg_nameofcountry == 1 | chg_nameofcountry == 0])
nonrevo_nameofcountry_wtest <- wilcox.test(polity_change ~ chg_nameofcountry, nonrevotrans[chg_nameofcountry == 1 | chg_nameofcountry == 0])


#4. chg_propertyowernship
all_property_1 <- tally(revotrans.clean[chg_propertyowernship == 1])
all_property_0 <- tally(revotrans.clean[chg_propertyowernship == 0])
all_property_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_propertyowernship)
all_property_ttest <- t.test(polity_change ~ chg_propertyowernship, revotrans.clean[chg_propertyowernship == 1 | chg_propertyowernship == 0])
all_property_wtest <- wilcox.test(polity_change ~ chg_propertyowernship, revotrans.clean[chg_propertyowernship == 1 | chg_propertyowernship == 0])

revo_property_1 <- tally(allrevos[chg_propertyowernship == 1])
revo_property_0 <- tally(allrevos[chg_propertyowernship == 0])
revo_property_ctest <- cor.test(allrevos$polity_change, allrevos$chg_propertyowernship)
revo_property_ttest <- t.test(polity_change ~ chg_propertyowernship, allrevos[chg_propertyowernship == 1 | chg_propertyowernship == 0])
revo_property_wtest <- wilcox.test(polity_change ~ chg_propertyowernship, allrevos[chg_propertyowernship == 1 | chg_propertyowernship == 0])

nonrevo_property_1 <- tally(nonrevotrans[chg_propertyowernship == 1])
nonrevo_property_0 <- tally(nonrevotrans[chg_propertyowernship == 0])
nonrevo_property_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_propertyowernship)
nonrevo_property_ttest <- t.test(polity_change ~ chg_propertyowernship, nonrevotrans[chg_propertyowernship == 1 | chg_propertyowernship == 0])
nonrevo_property_wtest <- wilcox.test(polity_change ~ chg_propertyowernship, nonrevotrans[chg_propertyowernship == 1 | chg_propertyowernship == 0])

#5. chg_womenandethnicstatus
all_womenandethnic_1 <- tally(revotrans.clean[chg_womenandethnicstatus == 1])
all_womenandethnic_0 <- tally(revotrans.clean[chg_womenandethnicstatus == 0])
all_womenandethnic_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_womenandethnicstatus)
all_womenandethnic_ttest <- t.test(polity_change ~ chg_womenandethnicstatus, revotrans.clean[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])
all_womenandethnic_wtest <- wilcox.test(polity_change ~ chg_womenandethnicstatus, revotrans.clean[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])

revo_womenandethnic_1 <- tally(allrevos[chg_womenandethnicstatus == 1])
revo_womenandethnic_0 <- tally(allrevos[chg_womenandethnicstatus == 0])
revo_womenandethnic_ctest <- cor.test(allrevos$polity_change, allrevos$chg_womenandethnicstatus)
revo_womenandethnic_ttest <- t.test(polity_change ~ chg_womenandethnicstatus, allrevos[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])
revo_womenandethnic_wtest <- wilcox.test(polity_change ~ chg_womenandethnicstatus, allrevos[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])

nonrevo_womenandethnic_1 <- tally(nonrevotrans[chg_womenandethnicstatus == 1])
nonrevo_womenandethnic_0 <- tally(nonrevotrans[chg_womenandethnicstatus == 0])
nonrevo_womenandethnic_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_womenandethnicstatus)
nonrevo_womenandethnic_ttest <- t.test(polity_change ~ chg_womenandethnicstatus, nonrevotrans[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])
nonrevo_womenandethnic_wtest <- wilcox.test(polity_change ~ chg_womenandethnicstatus, nonrevotrans[chg_womenandethnicstatus == 1 | chg_womenandethnicstatus == 0])

#6 chg_religioningovernment
all_religioningovernment_1 <- tally(revotrans.clean[chg_religioningovernment == 1])
all_religioningovernment_0 <- tally(revotrans.clean[chg_religioningovernment == 0])
all_religioningovernment_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_religioningovernment)
all_religioningovernment_ttest <- t.test(polity_change ~ chg_religioningovernment, revotrans.clean[chg_religioningovernment == 1 | chg_religioningovernment == 0])
all_religioningovernment_wtest <- wilcox.test(polity_change ~ chg_religioningovernment, revotrans.clean[chg_religioningovernment == 1 | chg_religioningovernment == 0])

revo_religioningovernment_1 <- tally(allrevos[chg_religioningovernment == 1])
revo_religioningovernment_0 <- tally(allrevos[chg_religioningovernment == 0])
revo_religioningovernment_ctest <- cor.test(allrevos$polity_change, allrevos$chg_religioningovernment)
revo_religioningovernment_ttest <- t.test(polity_change ~ chg_religioningovernment, allrevos[chg_religioningovernment == 1 | chg_religioningovernment == 0])
revo_religioningovernment_wtest <- wilcox.test(polity_change ~ chg_religioningovernment, allrevos[chg_religioningovernment == 1 | chg_religioningovernment == 0])

nonrevo_religioningovernment_1 <- tally(nonrevotrans[chg_religioningovernment == 1])
nonrevo_religioningovernment_0 <- tally(nonrevotrans[chg_religioningovernment == 0])
nonrevo_religioningovernment_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_religioningovernment)
nonrevo_religioningovernment_ttest <- t.test(polity_change ~ chg_religioningovernment, nonrevotrans[chg_religioningovernment == 1 | chg_religioningovernment == 0])
nonrevo_religioningovernment_wtest <- wilcox.test(polity_change ~ chg_religioningovernment, nonrevotrans[chg_religioningovernment == 1 | chg_religioningovernment == 0])


#7 chg_revolutionarycommittee
all_revolutionarycommittee_1 <- tally(revotrans.clean[chg_revolutionarycommittee == 1])
all_revolutionarycommittee_0 <- tally(revotrans.clean[chg_revolutionarycommittee == 0])
all_revolutionarycommittee_ctest <- cor.test(revotrans.clean$polity_change, revotrans.clean$chg_revolutionarycommittee)
all_revolutionarycommittee_ttest <- t.test(polity_change ~ chg_revolutionarycommittee, revotrans.clean[chg_revolutionarycommittee == 1 | chg_revolutionarycommittee  == 0])
all_revolutionarycommittee_wtest <- wilcox.test(polity_change ~ chg_revolutionarycommittee , revotrans.clean[chg_revolutionarycommittee  == 1 | chg_revolutionarycommittee  == 0])


revo_revolutionarycommittee_1 <- tally(allrevos[chg_revolutionarycommittee == 1])
revo_revolutionarycommittee_0 <- tally(allrevos[chg_revolutionarycommittee == 0])
revo_revolutionarycommittee_ctest <- cor.test(allrevos$polity_change, allrevos$chg_revolutionarycommittee)
revo_revolutionarycommittee_ttest <- t.test(polity_change ~ chg_revolutionarycommittee, allrevos[chg_revolutionarycommittee == 1 | chg_revolutionarycommittee  == 0])
revo_revolutionarycommittee_wtest <- wilcox.test(polity_change ~ chg_revolutionarycommittee , allrevos[chg_revolutionarycommittee  == 1 | chg_revolutionarycommittee  == 0])

nonrevo_revolutionarycommittee_1 <- tally(nonrevotrans[chg_revolutionarycommittee == 1])
nonrevo_revolutionarycommittee_0 <- tally(nonrevotrans[chg_revolutionarycommittee == 0])
nonrevo_revolutionarycommittee_ctest <- cor.test(nonrevotrans$polity_change, nonrevotrans$chg_revolutionarycommittee)
nonrevo_revolutionarycommittee_ttest <- t.test(polity_change ~ chg_revolutionarycommittee, nonrevotrans[chg_revolutionarycommittee == 1 | chg_revolutionarycommittee  == 0])
nonrevo_revolutionarycommittee_wtest <- wilcox.test(polity_change ~ chg_revolutionarycommittee , nonrevotrans[chg_revolutionarycommittee  == 1 | chg_revolutionarycommittee  == 0])


criteria2sub_summary <- matrix(c(all_executivepower_0, all_executivepower_1, all_executivepower_ttest$p.value, all_executivepower_wtest$p.value,
                                 all_politicalideology_0, all_politicalideology_1, all_politicalideology_ttest$p.value, all_politicalideology_wtest$p.value,
                                 all_nameofcountry_0, all_nameofcountry_1, all_nameofcountry_ttest$p.value, all_nameofcountry_wtest$p.value,
                                 all_property_0, all_property_1, all_property_ttest$p.value, all_property_wtest$p.value,
                                 all_womenandethnic_0, all_womenandethnic_1, all_womenandethnic_ttest$p.value, all_womenandethnic_wtest$p.value,
                                 all_religioningovernment_0, all_religioningovernment_1, all_religioningovernment_ttest$p.value, all_religioningovernment_wtest$p.value,
                                 all_revolutionarycommittee_0, all_revolutionarycommittee_1, all_revolutionarycommittee_ttest$p.value, all_revolutionarycommittee_wtest$p.value,
                                 revo_executivepower_0, revo_executivepower_1, revo_executivepower_ttest$p.value, revo_executivepower_wtest$p.value,
                                 revo_politicalideology_0, revo_politicalideology_1, revo_politicalideology_ttest$p.value, revo_politicalideology_wtest$p.value,
                                 revo_nameofcountry_0, revo_nameofcountry_1, revo_nameofcountry_ttest$p.value, revo_nameofcountry_wtest$p.value,
                                 revo_property_0, revo_property_1, revo_property_ttest$p.value, revo_property_wtest$p.value,
                                 revo_womenandethnic_0, revo_womenandethnic_1, revo_womenandethnic_ttest$p.value, revo_womenandethnic_wtest$p.value,
                                 revo_religioningovernment_0, revo_religioningovernment_1, revo_religioningovernment_ttest$p.value, revo_religioningovernment_wtest$p.value,
                                 revo_revolutionarycommittee_0, revo_revolutionarycommittee_1, revo_revolutionarycommittee_ttest$p.value, revo_revolutionarycommittee_wtest$p.value,
                                 nonrevo_executivepower_0, nonrevo_executivepower_1, nonrevo_executivepower_ttest$p.value, nonrevo_executivepower_wtest$p.value,
                                 nonrevo_politicalideology_0, nonrevo_politicalideology_1, nonrevo_politicalideology_ttest$p.value, nonrevo_politicalideology_wtest$p.value,
                                 nonrevo_nameofcountry_0, nonrevo_nameofcountry_1, nonrevo_nameofcountry_ttest$p.value, nonrevo_nameofcountry_wtest$p.value,
                                 nonrevo_property_0, nonrevo_property_1, nonrevo_property_ttest$p.value, nonrevo_property_wtest$p.value,
                                 nonrevo_womenandethnic_0, nonrevo_womenandethnic_1, nonrevo_womenandethnic_ttest$p.value, nonrevo_womenandethnic_wtest$p.value,
                                 nonrevo_religioningovernment_0, nonrevo_religioningovernment_1, nonrevo_religioningovernment_ttest$p.value, nonrevo_religioningovernment_wtest$p.value,
                                 nonrevo_revolutionarycommittee_0, nonrevo_revolutionarycommittee_1, nonrevo_revolutionarycommittee_ttest$p.value, nonrevo_revolutionarycommittee_wtest$p.value), 
                               nrow = 3, ncol = 28, byrow = T)
rownames(criteria2sub_summary) <- c("All", "Revolutionary", "Non-Revolutionary")
colnames(criteria2sub_summary) <- c("Exec_count_0", "Exec_count_1", "Exec_ttest_pval", "Exec_wtest_pval",
                                    "Politicalideology_count_0", "Politicalideology_count_1", "Politicalideology_ttest_pval", "Politicalideology_wtest_pval",
                                    "Nameofcountry_count_0", "Nameofcountry_count_1", "Nameofcountry_ttest_pval", "Nameofcountry_wtest_pval",
                                    "Property_ownership_count_0", "Property_ownership_count_1", "Property_ownership_ttest_pval", "Property_ownership_wtest_pval",
                                    "Womenandethnic_count_0", "Womenandethnic_count_1", "Womenandethnic_ttest_pval", "Womenandethnic_wtest_pval",
                                    "Religioningovt_count_0", "Religioningovt_count_1", "Religioningovt_ttest_pval", "Religioningovt_wtest_pval",
                                    "RevCommittee_count_0", "RevCommittee_count_1", "RevCommittee_ttest_pval", "RevCommittee_wtest_pval")


#### SECTION 9: STATISTICAL TESTS ####
#Polity, democ, autoc, revo vs nonrevo
all_ttest_polity_change <- t.test(polity_change ~ revolutionaryleader, revotrans.clean)
all_wtest_polity_change <- wilcox.test(polity_change ~ revolutionaryleader, revotrans.clean)

all_ttest_democ_change <- t.test(democ_change ~ revolutionaryleader, revotrans.clean)
all_wtest_democ_change <- wilcox.test(democ_change ~ revolutionaryleader, revotrans.clean)

all_ttest_autoc_change <- t.test(democ_change ~ revolutionaryleader, revotrans.clean)
all_wtest_autoc_change <- wilcox.test(democ_change ~ revolutionaryleader, revotrans.clean)

#Usedforce, all, within revo, within nonrevo
all_ttest_polity_usedforce <- t.test(polity_change ~ usedforce, revotrans.clean) 
all_wtest_polity_usedforce <- wilcox.test(polity_change ~ usedforce, revotrans.clean) 

revo_ttest_polity_usedforce <- t.test(polity_change ~ usedforce, allrevos) 
revo_wtest_polity_usedforce <- wilcox.test(polity_change ~ usedforce, allrevos)

nonrevo_ttest_polity_usedforce <- t.test(polity_change ~ usedforce, nonrevotrans)
nonrevo_wtest_polity_usedforce <- wilcox.test(polity_change ~ usedforce, nonrevotrans) ###"Cannot compute exact p-value with ties"###


#### SECTION 10: CHARTS ####

####KEEP####

#Boxplot comparisons btwn revo & nonrevo
ggplot(revotrans.clean, aes(x = factor(revolutionaryleader), y = polity_change)) + geom_boxplot()
ggplot(revotrans.clean, aes(x = factor(revolutionaryleader), y = democ_change)) + geom_boxplot()
ggplot(revotrans.clean, aes(x = factor(revolutionaryleader), y = autoc_change)) + geom_boxplot()


#Density Plot comparisons btwn revo & nonrevo
##Polity, Democ, Autoc change
ggplot(allrevos, aes(x = polity_change)) + geom_density()
ggplot(allrevos, aes(x = democ_change)) + geom_density()
ggplot(allrevos, aes(x = autoc_change)) + geom_density()

ggplot(nonrevotrans, aes(x = polity_change)) + geom_density()
ggplot(nonrevotrans, aes(x = democ_change)) + geom_density()
ggplot(nonrevotrans, aes(x = autoc_change)) + geom_density()


##Polity, democ, and autoc before & after scores
ggplot(allrevos, aes(x = last_polity)) + geom_density()
ggplot(allrevos, aes(x = polity)) + geom_density()
ggplot(nonrevotrans, aes(x = last_polity)) + geom_density()
ggplot(nonrevotrans, aes(x = polity)) + geom_density()

ggplot(allrevos, aes(x = last_democ)) + geom_density()
ggplot(allrevos, aes(x = democ)) + geom_density()
ggplot(nonrevotrans, aes(x = last_democ)) + geom_density()
ggplot(nonrevotrans, aes(x = democ)) + geom_density()

ggplot(allrevos, aes(x = last_autoc)) + geom_density()
ggplot(allrevos, aes(x = autoc)) + geom_density()
ggplot(nonrevotrans, aes(x = last_autoc)) + geom_density()
ggplot(nonrevotrans, aes(x = autoc)) + geom_density()


#Criteria 1 - Usedforce
ggplot(revotrans.clean,aes(x=factor(usedforce),y=polity_change))+geom_boxplot()
ggplot(allrevos,aes(x=factor(usedforce),y=polity_change))+geom_boxplot()
ggplot(nonrevotrans,aes(x=factor(usedforce),y=polity_change))+geom_boxplot()

ggplot(revotrans.clean,aes(x=factor(usedforce),y=polity_change))+geom_count()
ggplot(allrevos,aes(x=factor(usedforce),y=polity_change))+geom_count()
ggplot(nonrevotrans,aes(x=factor(usedforce),y=polity_change))+geom_count()


##Criteria 2 - Totalcategorieschanged
ggplot(allrevos, aes(x = factor(totalcategorieschanged), y = polity_change)) + geom_boxplot()
ggplot(allrevos, aes(x = factor(totalcategorieschanged), y = democ_change)) + geom_boxplot()
ggplot(allrevos, aes(x = factor(totalcategorieschanged), y = autoc_change)) + geom_boxplot()



####END KEEP####



#Transition length
ggplot(allrevos, aes(x = last_transition_length)) + geom_density()

#Nonrevos only
ggplot(nonrevotrans,aes(x=usedforce,y=polity_change))+geom_point()


ggplot(nonrevotrans, aes(x = last_transition_length)) + geom_density()
ggplot(nonrevotrans, aes(x = last_transition_length, y = polity_change)) + geom_point()

ggplot(nonrevotrans, aes(x = last_polity)) + geom_density()
ggplot(nonrevotrans, aes(x = polity)) + geom_density()
ggplot(nonrevotrans, aes(x = polity_change)) + geom_density()
ggplot(nonrevotrans, aes(x = last_transition_length)) + geom_density()

ggplot(revotrans.clean, aes(x = last_polity, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = factor(revolutionaryleader), y = polity_change)) + geom_boxplot()



ggplot(revotrans.clean,(aes(x = democ_change, y = autoc_change))) + geom_point() + geom_smooth(method = "lm")

#Density plots of all Criteria2 subs
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_executivepower)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_politicalideology)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_nameofcountry)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_propertyowernship)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_womenandethnicstatus)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_religioningovernment)
ggplot(na.omit(allrevos), aes(x = polity_change)) + geom_density() + facet_wrap(~chg_revolutionarycommittee)

#Scatter & boxplots of all Criteria2 subs
ggplot(na.omit(allrevos), aes(x = factor(chg_executivepower), y = polity_change)) + geom_count() 
ggplot(allrevos, aes(x = factor(chg_executivepower), y = polity_change)) + geom_boxplot()


ggplot(na.omit(allrevos), aes(x = factor(chg_politicalideology), y = polity_change)) + geom_count() 
ggplot(na.omit(allrevos), aes(x = factor(chg_politicalideology), y = polity_change)) + geom_boxplot()


ggplot(na.omit(allrevos), aes(x = factor(chg_nameofcountry), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(chg_nameofcountry), y = polity_change)) + geom_boxplot()


ggplot(na.omit(allrevos), aes(x = factor(chg_propertyowernship), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(chg_propertyowernship), y = polity_change)) + geom_boxplot() 


ggplot(na.omit(allrevos), aes(x = factor(chg_womenandethnicstatus), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(chg_womenandethnicstatus), y = polity_change)) + geom_boxplot() 

ggplot(na.omit(allrevos), aes(x = factor(chg_religioningovernment), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(chg_religioningovernment), y = polity_change)) + geom_boxplot()


ggplot(na.omit(allrevos), aes(x = factor(chg_revolutionarycommittee), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(chg_revolutionarycommittee), y = polity_change)) + geom_boxplot()


ggplot(na.omit(allrevos), aes(x = factor(totalcategorieschanged), y = polity_change)) + geom_point() 
ggplot(na.omit(allrevos), aes(x = factor(totalcategorieschanged), y = polity_change)) + geom_boxplot()
cor.test(allrevos$polity_change, allrevos$totalcategorieschanged)


##Both revos & non revos
#Scatterplots
ggplot(revotrans.clean, aes(x = last_democ, y = last_autoc)) + geom_point()

ggplot(revotrans.clean, aes(x = last_democ, y = democ_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_democ, y = autoc_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_democ, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()

ggplot(revotrans.clean, aes(x = last_autoc, y = autoc_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_autoc, y = democ_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()
ggplot(revotrans.clean, aes(x = last_autoc, y = polity_change, color = revolutionaryleader, size = last_transition_length)) + geom_point()


ggplot(revotrans.clean, (aes(x = last_transition_length, y = polity_change, color = revolutionaryleader))) + geom_point()

ggplot(na.omit(revotrans.clean),aes(x=factor(usedforce),y=polity_change))+geom_point()
ggplot(na.omit(allrevos),aes(x=factor(usedforce),y=polity_change))+geom_point()
ggplot(na.omit(nonrevotrans),aes(x=factor(usedforce),y=polity_change))+geom_point()



ggplot(revotrans.clean,(aes(x = democ_change, y = autoc_change))) + geom_smooth()

#Subsets of revo criteria 1 & 2 according to Colgan
allrevos_integers <- allrevos
allrevos_integers$chg_politicalideology <- as.integer(allrevos$chg_politicalideology)
allrevos_integers$chg_propertyowernship <- as.integer(allrevos$chg_propertyowernship)
allrevos_integers$chg_womenandethnicstatus <- as.integer(allrevos$chg_womenandethnicstatus)
allrevos_integers$chg_religioningovernment <- as.integer(allrevos$chg_religioningovernment)
allrevos_integers$chg_revolutionarycommittee <- as.integer(allrevos$chg_revolutionarycommittee)

allrevos_melt <- melt.data.table(allrevos_integers, id.vars = c("ccname", "polity", "last_polity", "polity_change", 
                                            "democ", "last_democ", "democ_change", 
                                            "autoc", "last_autoc", "autoc_change"), 
                      measure.vars = c("chg_executivepower", "chg_politicalideology", "chg_nameofcountry", 
                                            "chg_propertyowernship", "chg_womenandethnicstatus", "chg_religioningovernment", 
                                            "chg_revolutionarycommittee"))

names(allrevos_melt) <- c("ccname", "polity", "last_polity", "polity_change", "democ", "last_democ", "democ_change", 
                          "autoc", "last_autoc", "autoc_change", "Criteria_2sub", "Criteria_2sub_onoff")

ggplot(na.omit(allrevos_melt), aes(x = factor(Criteria_2sub_onoff), y = polity_change)) + geom_boxplot() + facet_wrap(~Criteria_2sub)
ggplot(na.omit(allrevos_melt), aes(x = Criteria_2sub, y = polity_change, fill = interaction(Criteria_2sub, Criteria_2sub_onoff))) + 
  geom_boxplot()





##Revos only

#Revos - Scatterplots
ggplot(allrevos, aes(x = totalcategorieschanged, y = polity_change)) + geom_count()




#ggplot(allrevos, aes(x = last_polity, y = polity_change)) + geom_smooth()
#ggplot(allrevos, aes(x = polity_change, y = last_transition_length)) + geom_point()

