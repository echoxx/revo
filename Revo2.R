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


########
all_countries <- dt_revo.trimmed
all_countries[,polity_cens:=ifelse(polity< -10, NA,polity)]
all_countries[,polity_carryforward:=na.locf(polity_cens,na.rm=F),by=ccname]


prev_country_data <- all_countries[,list(ccname, year = year+1, last_leader = leader, last_revo = revolutionaryleader, last_polity = polity_carryforward)]

all_countries = merge(all_countries, prev_country_data, by = c("ccname", "year"))
View(all_countries[,list(ccname,year,leader,last_leader,revolutionaryleader,last_revo,polity,polity_cens,polity_carryforward,last_polity)])

test=all_countries[ccname=="Afghanistan"]
test[,polity_carryforward:=na.locf(polity_cens),by=ccname]
#######


#extract years that are both transition and revo leader
dt_transrevo=dt_revo.trimmed[transition==1&revolutionaryleader==1]
test=dt_revo.trimmed[ccname=="Afghanistan"]
leaders=dt_revo.trimmed[!duplicated(dt_revo.trimmed[,list(leader,ccname)])]
leaders[,startyear:=as.numeric(strftime(strptime(startdate,"%d/%m/%Y"),"%Y"))]
leaders[,endyear:=as.numeric(strftime(strptime(enddate,"%d/%m/%Y"),"%Y"))]
leaders[,regime_length:=endyear-startyear+1]
pre_revo=leaders[,list(ccname,year=start_year-1)]

leaders[,regime_type:="non-revolutionary"]
leaders[revolutionaryleader==1&transition==1,regime_type:="transition revolutionary"]
leaders[revolutionaryleader==1&transition==0,regime_type:="non-transition revolutionary"]

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
