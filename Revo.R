##REVIEW##

setwd("~/Google_Drive2/R/Revolutions")

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

dt_revo.trimmed <- data.table(revo.trimmed)
dt_revo.trimmed[,year_diff:=c(0,diff(year)),by=list(ccname)]
dt_revo.trimmed[,data_year:=seq_along(year),by=list(ccname)] 
#dt_revo.trimmed[,rev_diff:=c(0,diff(revolutionaryleader)),by=list(ccname)] ###This does not provide correct # of revolutions(77)###
dt_revo.trimmed[,transition := polity < -10, by = ccname]


##Way to not use a for loop?

#New revolution/revolutionary leader?
##This does not provide correct # of revolutions either
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


polity_transition_change <- numeric(length = length(dt_revo.trimmed$polity2))
polity_revo_change <- numeric(length = length(dt_revo.trimmed$polity2))
for (i in seq_along(dt_revo.trimmed$transition)) {
  skip_back <- numeric()
  if (is.na(dt_revo.trimmed$transition[i])){
    next()
  } else if (is.na(dt_revo.trimmed$transition[max(i-1,1)])) {
    next()
  } else if (( (dt_revo.trimmed$transition[i] == FALSE) && (dt_revo.trimmed$transition[max(i-1,1)] == TRUE) )) {
     skip_back <- (1 + dt_revo.trimmed$transition_years[i-1])
     polity_transition_change[i] <- dt_revo.trimmed$polity2[i] - dt_revo.trimmed$polity2[i-skip_back]
     
     #Checks if transition is also a revolution
     if (dt_revo.trimmed$revolutionaryleader[i-(skip_back - 1)] == 1) { #skip_back - 1 since skip_back would be year before revolution begins
       polity_revo_change[i] <- polity_transition_change[i]
     } else {
       polity_revo_change[i] <- 0
     }
  
     } else  {
       polity_transition_change[i] <- 0
     }
}
  
dt_revo.trimmed[,transition_polity_change:=polity_transition_change]
dt_revo.trimmed[,revo_polity_change:=polity_revo_change]


###Jeremy's stuff
## ggplot(dt_revo.trimmed,aes(x=data_year,y=polity,color=revolutionaryleader,group=1))+geom_line()+facet_wrap(~ccname) 
##ggplot(dt_revo.trimmed,aes(x=data_year,y=polity2,color=revolutionaryleader,group=1))+geom_line()+facet_wrap(~ccname)
##dt_revo.trimmed[,mean(polity),by=list(ccname)]

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

###Create new data table only for 1) revolution and 2) transition state-years
#retain time series
transition_revo_index <-  dt_revo.trimmed[,.(transition == TRUE | revolutionaryleader == 1 | transition_polity_change !=0 | revo_polity_change != 0)] 
transition_revo_index <- which(transition_revo_index == TRUE)
dt_revo.transition.ts <- dt_revo.trimmed[transition_revo_index,]

#DT that'that just shows polity changes for revos & transitions
transition_revo_index <- dt_revo.transition.ts[,.(transition_polity_change !=0 | revo_polity_change !=0 )]
transition_revo_index <- which(transition_revo_index == TRUE)
dt_revo.transition <- dt_revo.transition.ts[transition_revo_index,]



