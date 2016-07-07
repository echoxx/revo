setwd("~/Google_Drive2/R/Revolutions")

library(data.table)
library(ggplot2)
library(haven) #imports DTA with column title descriptions
library(foreign) #does not import column title descriptions
library(dplyr) #for selecting columns
library(zoo) #lvcf

#Import
revo <- read_dta("Measuring Revolution.COLGAN.2012Nov.dta")

#Columns to select

revo.trimmed <- select(revo, ccname, year, startdate, enddate, leader, age, age0,
                       revolutionaryleader,
                       polity, polity2)
revo.trimmed <- revo.trimmed[order(revo.trimmed$ccname),]                       

##Other columns to consider
#Revolutionary leader same as revolution government (per Colgan). 
#RevolutionaryLeader coded if IrregularTransition=1, RadicalPolicy=1, FoundingLeader=0, ForeignInstalled=0
#Colgan's 1st criterion - ambiguouscoding, irregulartransition, usedforce, foundingleader, foreigninstall, radicalideology
#Colgan's 2nd criterion - chg_executivepower, chg_politicalideology, chg_nameofcountry, chg_propertyownership,chg_womenandethnicstatus, chg_religioningovernment, chg_revolutionarycommittee, totalcategorieschanged, 
#democ, autoc, durable, democratizing
                                              


dt_revo.trimmed <- data.table(revo.trimmed)
dt_revo.trimmed[,transition := polity < -10, by = ccname]
dt_revo.trimmed[,year_diff:=c(0,diff(year)),by=list(ccname)] #necessary?
dt_revo.trimmed[,data_year:=seq_along(year),by=list(ccname)] #necessary?

#Creates final year tag for last year of state years.
dt_revo.trimmed[,last_ccyear:=ifelse( shift(year_diff, n = 1L, type = "lead") == 0, 1, 0) ]
dt_revo.trimmed$last_ccyear[nrow(dt_revo.trimmed)] <- 1 #Otherwise, final value is NA since there is not leading row 


###Creates last_polity column using na.locf
###Side effect from split year+1 / merge technique: Strips out initial state year for all countries, 
####since need before & after data to calculate polity change 
all_countries <- dt_revo.trimmed
all_countries[,polity_cens:=ifelse(polity< -10, NA,polity)]
all_countries[,polity_carryforward:=na.locf(polity_cens,na.rm=F),by=ccname]

all_countries[, ':='(transition_length = 1L:.N), by = list(leader, transition)]
all_countries[, transition_length:=ifelse(transition == F, 0, transition_length)]
all_countries[,transition_start_end:=c(0,diff(transition)), by = list(ccname)] #---> IS THIS STILL NECESSARY?

#REVO_START_END KEY
##IF ==1, revo start, if == -1 revo end. If == 2, end one revoleader & start another in same year

#Triggers for general cases
all_countries[,revo_start_end:=c(0,diff(revolutionaryleader)), by = list(ccname)]

#Triggers for following event: two adjacent revolutionary leaders from same country. See: Burkina Faso, Sankara/Campaore
all_countries[,revo_start_end:=ifelse(revolutionaryleader == shift(revolutionaryleader, n=1L, type = "lag") &
                                        revolutionaryleader == 1 & 
                                        leader != shift(leader, n=1L, type = "lag"),2,revo_start_end), by = ccname]

#Triggers for final country year
all_countries[,revo_start_end:=ifelse(revolutionaryleader == 1 & last_ccyear ==1,-1,revo_start_end)]
                                        

#Creates previous countr dt (year+1) to merge, in order to have last year figures in same rows
prev_country_data <- all_countries[,list(ccname, year = year+1, last_leader = leader, last_revo = revolutionaryleader, 
                                         last_leader_age0 = age0,
                                         last_polity = polity_carryforward, 
                                         last_transition_length = transition_length)]
all_countries2 = merge(all_countries, prev_country_data, by = c("ccname", "year"))
all_countries2 <- all_countries2[, list(ccname, year, leader, last_leader, last_leader_age0,
                                      revolutionaryleader, last_revo, 
                                      polity, last_polity, 
                                      transition, transition_length, last_transition_length, 
                                      revo_start_end, transition_start_end)]

#Condense all_countries into DT with ONLY years following transitions or revolutions 
index_na <- which ( all_countries[,is.na(revo_start_end) | is.na(transition_start_end)] )
all_countries2$transition_start_end[index_na] <- 0 #Sets all NAs to 0 in transition tag

index_tr <- which ( all_countries[,transition_start_end == -1 | revo_start_end == -1 | revo_start_end == 2] )
tr_condensed <- data.table(all_countries[index_tr])

tr_condensed <- tr_condensed[polity >= -10,]

#View ( tr_condensed[revo_start_end == -1 | revo_start_end == 2,] )



#test=all_countries[ccname=="Afghanistan"]
#test[, ':='(transition_length = 1:.N), by = list(leader,transition)]
#test[,transition_length:=ifelse(transition == FALSE, 0, transition_length)]
#[ , `:=`( COUNT = .N , IDX = 1:.N ) , by = VAL ]
#test[, transition_length:=ifelse(transition == TRUE, transition + shift(transition_length, 1L, type = "lag"),0) ]
#http://stackoverflow.com/questions/19869145/counting-in-r-data-table


#test[,polity_carryforward:=na.locf(polity_cens),by=ccname]
#######


#extract years that are both transition and revo leader
dt_transrevo=dt_revo.trimmed[transition==1&revolutionaryleader==1]
test=dt_revo.trimmed[ccname=="Afghanistan"]

#Might work for determining length of regime
leaders=dt_revo.trimmed[!duplicated(dt_revo.trimmed[,list(leader,ccname)])]
leaders[,startyear:=as.numeric(strftime(strptime(startdate,"%d/%m/%Y"),"%Y"))]
leaders[,endyear:=as.numeric(strftime(strptime(enddate,"%d/%m/%Y"),"%Y"))]
leaders[,regime_length:=endyear-startyear+1]  ###Why is this startyear+1 instead of just startyear?

pre_revo=leaders[,list(ccname,year=startyear-1)]  #Was this to be merged with leaders?

leaders[,regime_type:="non-revolutionary"]
leaders[revolutionaryleader==1&transition==1,regime_type:="transition revolutionary"]
leaders[revolutionaryleader==1&transition==0,regime_type:="non-transition revolutionary"]

View(leaders[,list(ccname, leader, year, startdate, enddate, startyear, endyear, revolutionaryleader, 
                   polity, regime_length, regime_type)])
head(dt_transrevo)

dt_revo.trimmed[,trans_diff:=c(0,diff(transition)),by=ccname]

transstarts=dt_revo.trimmed[trans_diff==1]
transends=dt_revo.trimmed[trans_diff==-1]
dim(transstarts)
dim(transends)
table(transstarts$ccname)
table(transends$ccname)
test=dt_revo.trimmed[ccname=="Afghanistan"]
View(test)

country_data=dt_revo.trimmed[ccname=="Afghanistan"]
country_data
prev_country_data=country_data[,list(year=year+1,last_leader=leader,last_revo=revolutionaryleader)]
country_data=merge(country_data,prev_country_data,by="year")
country_data[,different_leader:=leader!=last_leader]


for (i in seq_along(dt_revo.trimmed$transition)) {
  if (is.na(dt_revo.trimmed$transition[i]) | is.na(dt_revo.trimmed$transition[max(i-1,1)])) {
    transition_end[i] <- NA
    revo_end[i] <- NA
  } else if ( (dt_revo.trimmed$transition[i] == FALSE && dt_revo.trimmed$transition[max(i-1,1)] == TRUE)) { 
    transition_end[i] <- TRUE
    
    if ((dt_revo.trimmed$revolutionaryleader[i] == 0) && (dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) ||
        dt_revo.trimmed$revolutionaryleader[i] == 1 && dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1 && 
        dt_revo.trimmed$leader[i] != dt_revo.trimmed$leader[max(i-1,1)] &&
        dt_revo.trimmed$ccname[i] == dt_revo.trimmed$ccname[max(i-1,1)]) {
      revo_end[i] <- TRUE
    } else if ((dt_revo.trimmed$revolutionaryleader[i] == 1 && dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) &&
               (dt_revo.trimmed$leader[i] == dt_revo.trimmed$leader[max(i-1,1)])) {
      revo_end[i] <- TRUE
    } else {
      revo_end[i] <- FALSE
    }
    
  } else if ((dt_revo.trimmed$revolutionaryleader[i] == 0 && dt_revo.trimmed$revolutionaryleader[max(i-1,1)] == 1) &&
             dt_revo.trimmed$ccname[i] == dt_revo.trimmed$ccname[max(i-1,1)]) {
    revo_end[i] <- TRUE
    transition_end[i] <- FALSE
  } else {
    transition_end[i] <- FALSE
    revo_end[i] <- FALSE
  }
}


transruns=rle(test[,transition])
years=cumsum(transruns$lengths)
test[years]
