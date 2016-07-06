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
                       #Revolutionary leader same as revolution government (per Colgan). 
                       #RevolutionaryLeader coded if IrregularTransition=1, RadicalPolicy=1, FoundingLeader=0, ForeignInstalled=0
                       revolutionaryleader, #ambiguouscoding,
                       #Colgan's 1st criterion - irregular transition
                       ##irregulartransition, usedforce, foundingleader, foreigninstall, radicalideology,  #1st criteria, irregular transition
                       #Colgan's 2nd criterion - radical policy
                       ##chg_executivepower, chg_politicalideology, chg_nameofcountry, chg_propertyowernship, chg_womenandethnicstatus, chg_religioningovernment, chg_revolutionarycommittee, totalcategorieschanged, 
                       democ, autoc, polity2, polity, durable, democratizing)
revo.trimmed <- revo.trimmed[order(revo.trimmed$ccname),]

dt_revo.trimmed <- data.table(revo.trimmed)
dt_revo.trimmed[,year_diff:=c(0,diff(year)),by=list(ccname)]
dt_revo.trimmed[,data_year:=seq_along(year),by=list(ccname)] 
dt_revo.trimmed[,transition := polity < -10, by = ccname]


###Creates last_polity column using na.locf
###Side effect from split year+1 / merge technique: Strips out initial state year for all countries, 
####since need before & after data to calculate polity change 
all_countries <- dt_revo.trimmed
all_countries[,polity_cens:=ifelse(polity< -10, NA,polity)]
all_countries[,polity_carryforward:=na.locf(polity_cens,na.rm=F),by=ccname]



all_countries = merge(all_countries, prev_country_data, by = c("ccname", "year"))
all_countries[, ':='(transition_length = 1:.N), by = list(leader, transition)]
all_countries[, transition_length:=ifelse(transition == F, 0, transition_length)]
#View(all_countries[,list(ccname,year,leader,last_leader,revolutionaryleader,last_revo,polity,polity_cens,polity_carryforward,last_polity)])

#all_countries[,transition_start:=c(0,diff(transition))] ---> IS THIS STILL NECESSARY?
#View(all_countries[,list(ccname, year, leader, revolutionaryleader, polity, transition, transition_start)])

#test=all_countries[ccname=="Afghanistan"]
#test[, ':='(transition_length = 1:.N), by = list(leader,transition)]
#test[,transition_length:=ifelse(transition == FALSE, 0, transition_length)]
#[ , `:=`( COUNT = .N , IDX = 1:.N ) , by = VAL ]
#test[, transition_length:=ifelse(transition == TRUE, transition + shift(transition_length, 1L, type = "lag"),0) ]
#http://stackoverflow.com/questions/19869145/counting-in-r-data-table

#prev_country_data <- all_countries[,list(ccname, year = year+1, last_leader = leader, last_revo = revolutionaryleader, last_polity = polity_carryforward)]
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
