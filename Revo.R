setwd("~/Google_Drive2/R/Revolutions")

library(data.table)
library(ggplot2)
library(haven) #imports DTA with column title descriptions
library(foreign) #does not import column title descriptions
library(dplyr) #for selecting columns

revo <- read_dta("Measuring Revolution.COLGAN.2012Nov.dta")

#Columns to select
## Review use of force vs irregular transition. Paper is not clear about whether use of force is subset of 
## irregular transition, or how they differ.

revo.forcetrimmed <- select(revo, usedforce, irregulartransition)

#This was code to verify relationship between usedforce and IrregularTransition
##All "use force" should also be "irreg", but not all "irreg" will be "usedforce"
irregcheck1 <- revo.forcetrimmed[,1] == 1 & revo.forcetrimmed[,2] == 0
irregcheck2 <- revo.forcetrimmed[,1] == 0 & revo.forcetrimmed[,2] == 1

sum(irregcheck1, na.rm = TRUE) #This should be 0 
sum(irregcheck2, na.rm = TRUE) #This is likely to be nonzero


revo.trimmed <- select(revo, ccname, year, startdate, enddate, leader, age, age0,
                       #Revolutionary leader same as revolution government (per Colgan). 
                       #RevolutionaryLeader coded if IrregularTransition=1, RadicalPolicy=1, FoundingLeader=0, ForeignInstalled=0
                       revolutionaryleader, #ambiguouscoding,
                       #Colgan's 1st criterion - irregular transition
                       ##irregulartransition, usedforce, foundingleader, foreigninstall, radicalideology,  #1st criteria, irregular transition
                       #Colgan's 2nd criterion - radical policy
                       ##chg_executivepower, chg_politicalideology, chg_nameofcountry, chg_propertyowernship, chg_womenandethnicstatus, chg_religioningovernment, chg_revolutionarycommittee, totalcategorieschanged, 
                       democ, autoc, polity2, polity, durable, democratizing)


dt_revo.trimmed <- data.table(revo.trimmed)
dt_revo.trimmed[,year_diff:=c(0,diff(year)),by=list(ccname)]
dt_revo.trimmed[,data_year:=seq_along(year),by=list(ccname)] 
#dt_revo.trimmed[,rev_diff:=c(0,diff(revolutionaryleader)),by=list(ccname)] ###This does not provide correct # of revolutions(77)###
dt_revo.trimmed[,transition := polity < -10, by = ccname]


#If new revolution occurs in given year. Includes adjacent leaders that are both coded as revolutionary. All other years are coded 0.
new_rev_vec <- numeric(length = (length(dt_revo.trimmed$transition)))
for (i in seq_along(dt_revo.trimmed$transition)) {
  if ( (dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) && 
       (dt_revo.trimmed$revolutionaryleader[i]== 1) &&
       (dt_revo.trimmed$leader[i] != dt_revo.trimmed$leader[max(i-1,1)])) {
    new_rev_vec[i] <- 1
  } else {
    new_rev_vec[i] <- max(dt_revo.trimmed$revolutionaryleader[i] - dt_revo.trimmed$revolutionaryleader[max(i-1,1)],0)
  }
}

dt_revo.trimmed[,new_rev:= new_rev_vec]

count_transition <- numeric( length = length(dt_revo.trimmed$transition))
for (i in seq_along(dt_revo.trimmed$transition)) { 
  if (is.na(dt_revo.trimmed$transition[i])) {
    count_transition[i] <- 0
  } else if (dt_revo.trimmed$transition[i] == TRUE && dt_revo.trimmed$ccname[i] == dt_revo.trimmed$ccname[i-1]) {
    count_transition[i] <- count_transition[(i-1)] + 1
  } else if (dt_revo.trimmed$transition[i] == TRUE && dt_revo.trimmed$ccname[i] != dt_revo.trimmed$ccname[i-1]) {
    count_transition[i] <- 1
  } else {
    count_transition[i] <- 0
  }
}

#Transitions are NOT the same as revolutionary leaders
dt_revo.trimmed[,transition_years:=(count_transition)]

###NEED TO FIND A WAY TO EXCLUDE 1ST LEADERS IN DATA SET (SUCH AS HOXHA IN ALBANIA)###
###THIS IS AN IMPORTANT TAG FOR ANALYSIS###
#End of transition period? End of revolutionary transition?
transition_end <- numeric()
revo_end <- numeric()
for (i in seq_along(dt_revo.trimmed$transition)) {
  if (is.na(dt_revo.trimmed$transition[i]) | is.na(dt_revo.trimmed$transition[max(i-1,1)])) {
    transition_end[i] <- NA
    revo_end[i] <- NA
  } else if ( (dt_revo.trimmed$transition[i] == FALSE && dt_revo.trimmed$transition[max(i-1,1)] == TRUE)) { 
    transition_end[i] <- TRUE
      

    if ((dt_revo.trimmed$revolutionaryleader[i] == 0) && (dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) ||
          dt_revo.trimmed$revolutionaryleader[i] == 1 && dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1 && 
          (dt_revo.trimmed$leader[i] != dt_revo.trimmed$leader[i+1])) {
        revo_end[i] <- TRUE
      } else {
        revo_end[i] <- FALSE
      }
    
  } else if (dt_revo.trimmed$revolutionaryleader[i] == 0 && dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) {
      revo_end[i] <- TRUE
      transition_end[i] <- FALSE
    } else {
      transition_end[i] <- FALSE
      revo_end[i] <- FALSE
    }
}

#If post-transition leader is not same as transition/revo leader, who was transition/revo leader?
###DOES NOT CAPTURE CORRECT REVO LEADER IF REVO HAPPENS IN UNDER 1 YEAR. SEE ALBANIA, ALIA/BERISHA###
transition_leader <- character(length= length(dt_revo.trimmed$leader))
revo_leader <- character(length= length(dt_revo.trimmed$leader))
for (i in seq_along(revo_end)) {
  if (is.na(revo_end[i]) | is.na(transition_end[max(i-1,1)])) {
    transition_leader[i] <- NA
    revo_leader[i] <- NA
  } else if (revo_end[i] == 1) {
    revo_leader[i] <- dt_revo.trimmed$leader[max(i-1,1)]
    transition_leader[i] <- dt_revo.trimmed$leader[max(i-1,1)]
  } else if (transition_end[i] == 1) {
    revo_leader[i] <- NA
    transition_leader[i] <- dt_revo.trimmed$leader[max(i-1,1)]
  } else {
    revo_leader[i] <- NA
    transition_leader[i] <- NA
  }
}

dt_revo.trimmed[,leader_transition:= transition_leader]
dt_revo.trimmed[,leader_revo:= revo_leader]
dt_revo.trimmed[,end_transition:= transition_end]
dt_revo.trimmed[,end_revo:= revo_end]




#Calculate transition_polity_change and revo_polity_change columns
polity_transition_change <- numeric(length = length(dt_revo.trimmed$polity2))
polity_revo_change <- numeric(length = length(dt_revo.trimmed$polity2))
polity_pre <- numeric(length = length(dt_revo.trimmed$polity2))
polity_post <- numeric(length = length(dt_revo.trimmed$polity2))
##Builds transition_polity_change, & codes revo_polity_change if transition period also a revolution
for (i in seq_along(dt_revo.trimmed$transition)) {
  skip_back <- numeric()
  if (is.na(dt_revo.trimmed$transition[i])){
    next()      ##IS THIS CORRECT?
  } else if (is.na(dt_revo.trimmed$transition[max(i-1,1)])) {
    next()
    } else if (( (dt_revo.trimmed$transition[i] == FALSE) && (dt_revo.trimmed$transition[max(i-1,1)] == TRUE) )) {
     if (dt_revo.trimmed$ccname[i] == dt_revo.trimmed$ccname[max(i-skip_back,1)]){    #checks if same country
       skip_back <- (1 + dt_revo.trimmed$transition_years[i-1])
       polity_pre[i] <- dt_revo.trimmed$polity2[max(i-skip_back,1)]
       polity_transition_change[i] <- dt_revo.trimmed$polity2[i] - dt_revo.trimmed$polity2[max(i-skip_back,1)]
     } else if (dt_revo.trimmed$ccname[i] != dt_revo.trimmed$ccname[max(i-1,1)]) {
       skip_back <- 0
       polity_pre[i] <- dt_revo.trimmed$polity2[i]
       polity_transition_change[i] <- 0 
     }
     #Checks if transition is also a revolution
     if (dt_revo.trimmed$revolutionaryleader[max(i-(skip_back - 1),1)] == 1) { #skip_back - 1 since skip_back would be year before revolution begins
       polity_revo_change[i] <- polity_transition_change[i]
     } else {
       polity_revo_change[i] <- 0
     }
     
     polity_post[i] <- dt_revo.trimmed$polity2[i]
  
     } else  {
       polity_transition_change[i] <- 0
     }
  #if(dt_revo.trimmed$leader[i] == "Hoxha") { print(c(i, skip_back, dt_revo.trimmed$polity2[i], dt_revo.trimmed$polity2[i-skip_back]))}
}

##Checks revo_polity_change for revolutions that were not coded as transitions
for (i in seq_along(dt_revo.trimmed$new_rev)) {
  if (polity_revo_change[i] != 0) {
    next()
  } else if (dt_revo.trimmed$new_rev[i] == 1 && dt_revo.trimmed$transition[i] == FALSE) {
    polity_revo_change[i] <- dt_revo.trimmed$polity2[i] - dt_revo.trimmed$polity2[i-1]
  }
}

  
either_polity_counter <- numeric(length = length(dt_revo.trimmed$polity2))
for (i in seq_along(either_polity_counter)) {
  if(is.na(polity_transition_change[i]) | is.na(polity_revo_change[i]) ) {
    either_polity_counter[i] <- NA
    } else if(polity_transition_change[i] == polity_revo_change[i]) {
    either_polity_counter[i] <- polity_transition_change[i]
  } else  {
    either_polity_counter[i] <- polity_transition_change[i] + polity_revo_change[i]
  }
}

#Return total transition years in year *after* transition ends (to line up with polity_change figures)
transition_years_counter <- numeric()
for (i in seq_along(dt_revo.trimmed$polity2)) {
  if(is.na(dt_revo.trimmed$end_transition[i]) | is.na(dt_revo.trimmed$end_transition[i]) ) {
    either_polity_counter[i] <- NA
    } else if(dt_revo.trimmed$end_transition[i] != 0 | dt_revo.trimmed$end_revo[i] != 0) {
    transition_years_counter[i] <- dt_revo.trimmed$transition_years[i-1]
  } else {
    transition_years_counter[i] <- 0
  }
}

#Add new columns to DT
dt_revo.trimmed[,polity_pre_change:=polity_pre]
dt_revo.trimmed[,polity_post_change:=polity_post]
dt_revo.trimmed[,transition_polity_change:=polity_transition_change]
dt_revo.trimmed[,revo_polity_change:=polity_revo_change]
dt_revo.trimmed[,either_polity_change:=either_polity_counter]
dt_revo.trimmed[,length_transition:=transition_years_counter]

##This creates a data table of all transitions + revolutions, and ties to Colgan's 77 revo figure##
##Excludes revolutions where leader never had stable polity score (e.g. transition value for all years)
##Example: Afghanistan - B. Rabbani
transition_index <- which(dt_revo.trimmed$end_transition == 1 | dt_revo.trimmed$end_revo == 1)
dt_revo.transition <- dt_revo.trimmed[transition_index,]
dt_revo.transition <- dt_revo.transition[order(dt_revo.transition$ccname),]

#####CHARTS#####
prechange_eitherpolity_revleader <- ggplot(dt_revo.transition, aes(x = polity_pre_change, y = either_polity_change, color = end_revo)) + geom_point()
prechange_eitherpolity_revleader <- ggplot(dt_revo.transition, aes(x = polity_pre_change, y = either_polity_change, color = end_revo, size = length_transition)) + geom_point()
postchange_eitherpolity_revleader <- ggplot(dt_revo.transition, aes(x = polity_post_change, y = either_polity_change, color = end_revo, size = length_transition)) + geom_point()
multiple_country.ts <- ggplot(dt_revo.trimmed,aes(x=data_year,y=polity2,color=revolutionaryleader,group=1))+geom_line()+facet_wrap(~ccname)

ggplot(dt_revo.transition, aes(x = age0, y = either_polity_change, color = end_revo)) + geom_point()

#####OUTPUTS#####
###AGE CORRELATIONS ARE INCORRECT, AS THEY ARE PICKING UP CURRENT YEAR LEADER, RATHER THAN TRANSITION/REVO YEAR LEADER###



#Revolutionary leaders with positive positive polity effect
pos_polity_revleaders_tally <- dt_revo.transition$either_polity_change > 0 & dt_revo.transition$end_revo == 1
pos_polity_revleaders_index <- which(dt_revo.transition$either_polity_change > 0 & dt_revo.transition$end_revo == 1)
pos_polity_revleaders <- dt_revo.transition[pos_polity_revleaders_index,either_polity_change]
pos_polity_length_revleaders <- dt_revo.transition[pos_polity_revleaders_index, length_transition]

count_pos_polity_revleaders <- sum(pos_polity_revleaders_tally, na.rm = TRUE)
mean_pos_polity_revleaders <- mean(pos_polity_revleaders)
median_pos_polity_revleaders <- median(pos_polity_revleaders)
agemean_pos_polity_revleaders <- mean(dt_revo.transition$age0[pos_polity_revleaders_index])
agemedian_pos_polity_revleaders <- median(dt_revo.transition$age0[pos_polity_revleaders_index])
mean_length_pos_polity_revleaders <- mean(pos_polity_length_revleaders)
median_length_pos_polity_revleaders <- median(pos_polity_length_revleaders)

#Revolutionary leaders with neutral polity effect
neut_polity_revleaders_tally <- dt_revo.transition$either_polity_change == 0 & dt_revo.transition$end_revo == 1
neut_polity_revleaders_index <- which(dt_revo.transition$either_polity_change == 0 & dt_revo.transition$end_revo == 1)
neut_polity_revleaders <- dt_revo.transition[neut_polity_revleaders_index,either_polity_change]
neut_polity_length_revleaders <- dt_revo.transition[neut_polity_revleaders_index, length_transition]

count_neut_polity_revleaders <- sum(neut_polity_revleaders_tally, na.rm = TRUE)
mean_neut_polity_revleaders <- mean(neut_polity_revleaders)
median_neut_polity_revleaders <- median(neut_polity_revleaders)
agemean_neut_polity_revleaders <- mean(dt_revo.transition$age0[neut_polity_revleaders_index])
agemedian_neut_polity_revleaders <- median(dt_revo.transition$age0[neut_polity_revleaders_index])
mean_length_neut_polity_revleaders <- mean(neut_polity_length_revleaders)
median_length_neut_polity_revleaders <- median(neut_polity_length_revleaders)

#Revolutionary leaders with negative polity effect
neg_polity_revleaders_tally <- dt_revo.transition$either_polity_change < 0 & dt_revo.transition$end_revo == 1
neg_polity_revleaders_index <- which(dt_revo.transition$either_polity_change < 0 & dt_revo.transition$end_revo == 1)
neg_polity_revleaders <- dt_revo.transition[neg_polity_revleaders_index,either_polity_change]
neg_polity_length_revleaders <- dt_revo.transition[neg_polity_revleaders_index, length_transition]

count_neg_polity_revleaders <- sum(neg_polity_revleaders_tally, na.rm = TRUE)
mean_neg_polity_revleaders <- mean(neg_polity_revleaders)
median_neg_polity_revleaders <- median(neg_polity_revleaders)
agemean_neg_polity_revleaders <- mean(dt_revo.transition$age0[neg_polity_revleaders_index])
agemedian_neg_polity_revleaders <- median(dt_revo.transition$age0[neg_polity_revleaders_index])
mean_length_neg_polity_revleaders <- mean(neg_polity_length_revleaders)
median_length_neg_polity_revleaders <- median(neg_polity_length_revleaders)

#Non-Revolutionary leaders with positive positive polity effect
pos_polity_norevleaders_tally <- dt_revo.transition$either_polity_change > 0 & dt_revo.transition$end_revo == 0
pos_polity_norevleaders_index <- which(dt_revo.transition$either_polity_change > 0 & dt_revo.transition$end_revo == 0)
pos_polity_norevleaders <- dt_revo.transition[pos_polity_norevleaders_index,either_polity_change]
pos_polity_length_norevleaders <- dt_revo.transition[pos_polity_norevleaders_index, length_transition]

count_pos_polity_norevleaders <- sum(pos_polity_norevleaders_tally, na.rm = TRUE)
mean_pos_polity_norevleaders <- mean(pos_polity_norevleaders)
median_pos_polity_norevleaders <- median(pos_polity_norevleaders)
agemean_pos_polity_norevleaders <- mean(dt_revo.transition$age0[pos_polity_norevleaders_index])
agemedian_pos_polity_norevleaders <- median(dt_revo.transition$age0[pos_polity_norevleaders_index])
mean_length_pos_polity_norevleaders <- mean(pos_polity_length_norevleaders)
median_length_pos_polity_norevleaders <- median(pos_polity_length_norevleaders)

#Non-Revolutionary leaders with neutral positive polity effect
neut_polity_norevleaders_tally <- dt_revo.transition$either_polity_change == 0 & dt_revo.transition$end_revo == 0
neut_polity_norevleaders_index <- which(dt_revo.transition$either_polity_change == 0 & dt_revo.transition$end_revo == 0)
neut_polity_norevleaders <- dt_revo.transition[neut_polity_norevleaders_index,either_polity_change]
neut_polity_length_norevleaders <- dt_revo.transition[neut_polity_norevleaders_index, length_transition]

count_neut_polity_norevleaders <- sum(neut_polity_norevleaders_tally, na.rm = TRUE)
mean_neut_polity_norevleaders <- mean(neut_polity_norevleaders)
median_neut_polity_norevleaders <- median(neut_polity_norevleaders)
agemean_neut_polity_norevleaders <- mean(dt_revo.transition$age0[neut_polity_norevleaders_index])
agemedian_neut_polity_norevleaders <- median(dt_revo.transition$age0[neut_polity_norevleaders_index])
mean_length_neut_polity_norevleaders <- mean(neut_polity_length_norevleaders)
median_length_neut_polity_norevleaders <- median(neut_polity_length_norevleaders)

#Non-Revolutionary leaders with negative polity effect
neg_polity_norevleaders_tally <- dt_revo.transition$either_polity_change < 0 & dt_revo.transition$end_revo == 0
neg_polity_norevleaders_index <- which(dt_revo.transition$either_polity_change < 0 & dt_revo.transition$end_revo == 0)
neg_polity_norevleaders <- dt_revo.transition[neg_polity_norevleaders_index,either_polity_change]
neg_polity_length_norevleaders <- dt_revo.transition[neg_polity_norevleaders_index, length_transition]

count_neg_polity_norevleaders <- sum(neg_polity_norevleaders_tally, na.rm = TRUE)
mean_neg_polity_norevleaders <- mean(neg_polity_norevleaders)
median_neg_polity_norevleaders <- median(neg_polity_norevleaders)
agemean_neg_polity_norevleaders <- mean(dt_revo.transition$age0[neg_polity_norevleaders_index])
agemedian_neg_polity_norevleaders <- median(dt_revo.transition$age0[neg_polity_norevleaders_index])
mean_length_neg_polity_norevleaders <- mean(neg_polity_length_norevleaders)
median_length_neg_polity_norevleaders <- median(neg_polity_length_norevleaders)



#Construct matrices by rev/norev, pos/neg polity
##Transition count
polity_summary <- matrix(c(count_pos_polity_revleaders, count_neut_polity_revleaders, count_neg_polity_revleaders, 
                           mean_pos_polity_revleaders, mean_neg_polity_revleaders,
                           median_pos_polity_revleaders, median_neg_polity_revleaders,
                           mean_length_pos_polity_revleaders, mean_length_neut_polity_revleaders, mean_length_neg_polity_revleaders,
                           median_length_pos_polity_revleaders, median_length_neut_polity_revleaders, median_length_neg_polity_revleaders,
                           agemean_pos_polity_revleaders, agemean_neut_polity_revleaders, agemean_neg_polity_revleaders,
                           agemedian_pos_polity_revleaders, agemedian_neut_polity_revleaders, agemedian_neg_polity_revleaders,
                           count_pos_polity_norevleaders, count_neut_polity_norevleaders, count_neg_polity_norevleaders,
                           mean_pos_polity_norevleaders, mean_neg_polity_norevleaders,
                           median_pos_polity_norevleaders, median_neg_polity_norevleaders,
                           mean_length_pos_polity_norevleaders, mean_length_neut_polity_norevleaders, mean_length_neg_polity_norevleaders,
                           median_length_pos_polity_norevleaders, median_length_neut_polity_norevleaders, median_length_neg_polity_norevleaders,
                           agemean_pos_polity_norevleaders, agemean_neut_polity_norevleaders, agemean_neg_polity_norevleaders,
                           agemedian_pos_polity_norevleaders, agemedian_neut_polity_norevleaders, agemedian_neg_polity_norevleaders), nrow=2, ncol=19, byrow = TRUE)
rownames(polity_summary) <- c("Revolutionary", "Non-Revolutionary")
colnames(polity_summary) <- c("Count_pos_polity", "Count_neut_polity", "Count_neg_polity", 
                              "Mean_pos_polity",  "Mean_neg_polity", 
                              "Median_pos_polity", "Median_neg_polity", 
                              "Mean_length_length", "Mean_neut_length", "Mean_neg_length",
                              "Median_pos_length", "Median_neut_length", "Median_neg_length", 
                              "Mean_age_pos", "Mean_age_neut", "Mean_age_neg",
                              "Median_age_pos", "Median_age_neut", "Median_age_neg")






# Revolution & Transition years count
revo_total <- sum(dt_revo.trimmed$rev_dif == 1)
transition_total <- sum(dt_revo.trimmed$transition_years == 1, na.rm = TRUE)

#Democratizing 
##Democratizing - Frequency
democ_index <- which(revo.trimmed$democratizing == 1)
democ_leaders <- data.frame(revo.trimmed$ccname[democ_index] , revo.trimmed$leader[democ_index])
names(democ_leaders) <- c("country", "leader")
democ_leaders_unique <- unique(democ_leaders)

###Democratizing - Frequency - Outputs
democ_out_count <- length(democ_leaders_unique[,1])
democ_out_countryyears <- length(democ_leaders[,1])

#### NOTES TO DATASET ###
#polity = democ - autoc

#polity2 = adjustment of polity score to remove -88, -77, -66
#-66 Cases of foreign “interruption” are treated as “system missing.”
#-77 Cases of “interregnum,” or anarchy, are converted to a “neutral” Polity score of “0.”
#-88 Cases of “transition” are prorated across the span of the transition. For example, country X 
#has a POLITY score of -7 in 1957, followed by three years of -88 and, finally, a score of +5 in 1961. 
#The change (+12) would be prorated over the intervening three years at a rate of per year, so that the 
#converted scores would be as follows: 1957 -7; 1958 -4; 1959 -1; 1960 +2; and 1961 +5.

#democratizing - variable created by Colgan. "a leader is coded as if the Polity store of his state increases 
#by at least 5 points in the first five years of tenure". Colgan includes the column closer to Criterion 1 variables,
#but democratizing does not influence RevolutionaryLeader

###END NOTES###





