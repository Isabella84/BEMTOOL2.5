
runTrafficLights_Frontiers<- function(w) {

bmt_wnd_eval <- showMessage("Traffic lights in progress...")
gtkWidgetSetSensitive(BMTmain_window, F)

print("Traffic lights in progress...")

workdir <- getwd()
setwd(casestudy_path)
dir.create("Traffic lights")
setwd(workdir)

SQ_QUO_SCENARIO_NAME <- "Scenario-SQ"

name_all_scenarios <- paste(casestudy_path, "/Evaluation/", casestudy_name, " - Biological indicators - ALL SCENARIOS.csv", sep="")
ALL_indicators <- read.csv(name_all_scenarios, sep=";")

name_all_scenarios <- paste(casestudy_path, "/Evaluation/", casestudy_name, " - Economic output - ALL SCENARIOS.csv", sep="")
eco <- read.csv(name_all_scenarios, sep=";")
colnames(eco) <- colnames(ALL_indicators)
ALL_indicators <- data.frame(rbind(ALL_indicators, eco) )

name_all_scenarios <- paste(casestudy_path, "/Evaluation/", casestudy_name, " - Pressure impact indicators - ALL SCENARIOS.csv", sep="")
press <- read.csv(name_all_scenarios, sep=";")
colnames(press) <- colnames(ALL_indicators)

ALL_indicators <- data.frame(rbind(ALL_indicators, press) )

ALL_indicators_2021 <- ALL_indicators[ALL_indicators$Year == years.forecast[foreperiod] ,]

ALL_indicators_2014 <- ALL_indicators[ALL_indicators$Year == years[simperiod],]

variable_for_trafficLigths <- unique(ALL_indicators_2021$Variable)

selection_byfleet <- c("total.landings", "Economic.indicators[CR.BER]",  "total.revenues", "employment", "Economic.indicators[ROI]")

selection_bystock <- c("SSB_exploited_pop", "Landing", "F")

scenarios_code <- unique(ALL_indicators_2021$ID_scenario)
scenarios_code <- scenarios_code[order(scenarios_code)]

TL_overall <- data.frame(matrix(nrow=length(scenarios_code)+1, ncol=0))

ALL_indicators_2021_overall_eco <- ALL_indicators_2021[ALL_indicators_2021$Variable %in% selection_byfleet & ALL_indicators_2021$Fleet_segment == "ALL" , ]
ALL_indicators_2014_overall_eco <- ALL_indicators_2014[ALL_indicators_2014$Variable %in% selection_byfleet & ALL_indicators_2014$Fleet_segment == "ALL" , ]

col_to_add <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[1], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]

col_to_add <- rbind(col_to_add, ALL_indicators_2014_overall_eco[ALL_indicators_2014_overall_eco$Variable == selection_byfleet[1] & ALL_indicators_2014_overall_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])

col_to_add[nrow(col_to_add), 1] <- paste( col_to_add[nrow(col_to_add), 1] , "-", years[simperiod], "(baseline year)")
  TL_overall <- cbind(TL_overall, col_to_add)
 
 col_to_add <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[2], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]

col_to_add <- rbind(col_to_add, ALL_indicators_2014_overall_eco[ALL_indicators_2014_overall_eco$Variable == selection_byfleet[2] & ALL_indicators_2014_overall_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
 TL_overall <- cbind(TL_overall, col_to_add[,2])

#
# col_to_add_R <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[3] , c(3,8)]
# col_to_add_R <- col_to_add_R[order(col_to_add_R$ID_scenario),]
#  col_to_add_BER <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[5], c(3,8)]
#  col_to_add_BER <- col_to_add_BER[order(col_to_add_BER$ID_scenario),]
#  
#col_to_add_BER$R_BER <- col_to_add_R$Value/ col_to_add_BER$Value  
#
# TL_overall <- cbind(TL_overall, col_to_add_BER$R_BER)

 col_to_add <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[5], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
 col_to_add <- rbind(col_to_add, ALL_indicators_2014_overall_eco[ALL_indicators_2014_overall_eco$Variable == selection_byfleet[5] & ALL_indicators_2014_overall_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])

 TL_overall <- cbind(TL_overall, col_to_add[,2])

 
  col_to_add <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[3], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
 col_to_add <- rbind(col_to_add, ALL_indicators_2014_overall_eco[ALL_indicators_2014_overall_eco$Variable == selection_byfleet[3] & ALL_indicators_2014_overall_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])

 TL_overall <- cbind(TL_overall, col_to_add[,2])
 
 


  
   col_to_add <- ALL_indicators_2021_overall_eco[ALL_indicators_2021_overall_eco$Variable == selection_byfleet[4], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
 col_to_add <- rbind(col_to_add, ALL_indicators_2014_overall_eco[ALL_indicators_2014_overall_eco$Variable == selection_byfleet[4] & ALL_indicators_2014_overall_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])

 TL_overall <- cbind(TL_overall, col_to_add[,2])
  
  for (spes in 1:length(BMT_SPECIES)) {
      col_to_add <- ALL_indicators_2021[ALL_indicators_2021$Variable == selection_bystock[1] & ALL_indicators_2021$Stock == BMT_SPECIES[spes], c(3,8)]
      col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
       col_to_add <- rbind(col_to_add, ALL_indicators_2014[ALL_indicators_2014$Variable ==   selection_bystock[1] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] &  ALL_indicators_2021$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
       
      TL_overall <- cbind(TL_overall, col_to_add[,2])
  }
  
  
    for (spes in 1:length(BMT_SPECIES)) {
    col_to_add <- ALL_indicators_2021[ALL_indicators_2021$Variable == selection_bystock[2] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == "ALL", c(3,8)]   
      col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
                   col_to_add <- rbind(col_to_add, ALL_indicators_2014[ALL_indicators_2014$Variable ==   selection_bystock[2] & ALL_indicators_2014$Stock == BMT_SPECIES[spes] & ALL_indicators_2014$Fleet_segment == "ALL" &  ALL_indicators_2014$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])             
      TL_overall <- cbind(TL_overall, col_to_add[,2])
  }
  
  
      for (spes in 1:length(BMT_SPECIES)) {
      col_to_add <- ALL_indicators_2021[ALL_indicators_2021$Variable ==   selection_bystock[3] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == "ALL", c(3,8)]
      col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
      col_to_add <- rbind(col_to_add, ALL_indicators_2014[ALL_indicators_2014$Variable ==   selection_bystock[3] & ALL_indicators_2014$Stock == BMT_SPECIES[spes] & ALL_indicators_2014$Fleet_segment == "ALL" & ALL_indicators_2014$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
                   
      TL_overall <- cbind(TL_overall, col_to_add[,2])
  }
  
  name_all_scenarios <- paste(casestudy_path, "/Traffic lights/", casestudy_name, " - TL data OVERALL.csv", sep="")
  
colnames(TL_overall) <- c("ID_scenario", "total.landings",	"CR.BER",	"ROI", "Revenues",	"Empl",	paste("SSB", BMT_SPECIES), paste("Landing", BMT_SPECIES), paste("F", BMT_SPECIES)) 
write.table(TL_overall , file=name_all_scenarios, sep=";", row.names=F)



#TL_overall_PERCENTAGE <- data.frame(matrix(nrow=(length(scenarios_code)-1), ncol=length(head__))) 
#colnames(TL_overall_PERCENTAGE)  <-  head__

TL_overall_PERCENTAGE <- TL_overall

TL_overall_SQ <- TL_overall[TL_overall$ID_scenario == SQ_QUO_SCENARIO_NAME,]

for (nr in 1:(nrow(TL_overall_PERCENTAGE)-1) ) {
if ( TL_overall_PERCENTAGE$ID_scenario[nr] != SQ_QUO_SCENARIO_NAME ) {
     for (nc in 2:(ncol(TL_overall_PERCENTAGE)-length(BMT_SPECIES)) ) {
           TL_overall_PERCENTAGE[nr,nc] <-   round(( as.numeric(as.character(TL_overall_PERCENTAGE[nr,nc])) - as.numeric(as.character(TL_overall_SQ[1,nc])))/  as.numeric(as.character(TL_overall_SQ[1,nc])) * 100 , 2)
     }
     }
}

#TL_overall_PERCENTAGE$Total <- rowSums(TL_overall_PERCENTAGE[,2:ncol(TL_overall_PERCENTAGE)])

  name_all_scenarios <- paste(casestudy_path, "/Traffic lights/", casestudy_name, " - Traffic lights OVERALL.csv", sep="")
write.table(TL_overall_PERCENTAGE , file=name_all_scenarios, sep=";", row.names=F)




head__percentage <- c("ID_scenario", "total.landings",	"CR.BER",	"ROI", "Revenues",	"Empl", paste("Landing", BMT_SPECIES), paste("F", BMT_SPECIES) ) 
head__ <- c("ID_scenario", "total.landings",	"CR.BER",	"ROI", "Revenues",	"Empl", paste("Landing", BMT_SPECIES), paste("F", BMT_SPECIES) ) 
  
TL_FLEET <- data.frame(matrix(nrow=0, ncol=length(head__))) 

TL_FLEET_PERCENTAGE <- data.frame(matrix(nrow=0, ncol=length(head__percentage))) 
 
for (fle in 1:length(BMT_FLEETSEGMENTS)) {

head_fleet <- c(BMT_FLEETSEGMENTS[fle], head__[2:length(head__)] )
TL_FLEET <- rbind(TL_FLEET, head_fleet)
colnames(TL_FLEET) <- c("ID_scenario", "total.landings",	"CR.BER",	"ROI", "Revenues",	"Empl", paste("Landing", BMT_SPECIES), paste("F", BMT_SPECIES) ) 

ALL_indicators_2021_fleet_eco <- ALL_indicators_2021[ALL_indicators_2021$Variable %in% selection_byfleet & ALL_indicators_2021$Fleet_segment == BMT_FLEETSEGMENTS[fle] , ]
ALL_indicators_2014_fleet_eco <- ALL_indicators_2014[ALL_indicators_2014$Variable %in% selection_byfleet & ALL_indicators_2014$Fleet_segment == BMT_FLEETSEGMENTS[fle] , ]

TL_FLEET_temp <- data.frame(matrix(nrow=(length(scenarios_code)+1), ncol=0)) 

col_to_add <- ALL_indicators_2021_fleet_eco[ALL_indicators_2021_fleet_eco$Variable == selection_byfleet[1], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]

col_to_add <- rbind(col_to_add, ALL_indicators_2014_fleet_eco[ALL_indicators_2014_fleet_eco$Variable == selection_byfleet[1] & ALL_indicators_2014_fleet_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
col_to_add[nrow(col_to_add), 1] <- paste( col_to_add[nrow(col_to_add), 1] , "-", years[simperiod], "(baseline year)")

 TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add)

col_to_add <- ALL_indicators_2021_fleet_eco[ALL_indicators_2021_fleet_eco$Variable == selection_byfleet[2], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
col_to_add <- rbind(col_to_add, ALL_indicators_2014_fleet_eco[ALL_indicators_2014_fleet_eco$Variable == selection_byfleet[2] & ALL_indicators_2014_fleet_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
 TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])

col_to_add <- ALL_indicators_2021_fleet_eco[ALL_indicators_2021_fleet_eco$Variable == selection_byfleet[5], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
col_to_add <- rbind(col_to_add, ALL_indicators_2014_fleet_eco[ALL_indicators_2014_fleet_eco$Variable == selection_byfleet[5] & ALL_indicators_2014_fleet_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
 TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])
 
col_to_add <- ALL_indicators_2021_fleet_eco[ALL_indicators_2021_fleet_eco$Variable == selection_byfleet[3], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
col_to_add <- rbind(col_to_add, ALL_indicators_2014_fleet_eco[ALL_indicators_2014_fleet_eco$Variable == selection_byfleet[3] & ALL_indicators_2014_fleet_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
 TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])
  
col_to_add <- ALL_indicators_2021_fleet_eco[ALL_indicators_2021_fleet_eco$Variable == selection_byfleet[4], c(3,8)]
col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
col_to_add <- rbind(col_to_add, ALL_indicators_2014_fleet_eco[ALL_indicators_2014_fleet_eco$Variable == selection_byfleet[4] & ALL_indicators_2014_fleet_eco$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
 TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])
  
    for (spes in 1:length(BMT_SPECIES)) {
    
    if (nrow(ALL_indicators_2021[ALL_indicators_2021$Variable ==   selection_bystock[2] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == BMT_FLEETSEGMENTS[fle], c(3,8)]) != 0) {
      col_to_add <- ALL_indicators_2021[ALL_indicators_2021$Variable ==   selection_bystock[2] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == BMT_FLEETSEGMENTS[fle], c(3,8)]
      col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
      
      col_to_add <- rbind(col_to_add, ALL_indicators_2014[ALL_indicators_2014$Variable ==   selection_bystock[2] & ALL_indicators_2014$Stock == BMT_SPECIES[spes] & ALL_indicators_2014$Fleet_segment == BMT_FLEETSEGMENTS[fle] & ALL_indicators_2021$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
      
      TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])
  } else {
      TL_FLEET_temp <- cbind(TL_FLEET_temp, rep(0, (length(scenarios_code)+1)))   
  }
  }
  
      for (spes in 1:length(BMT_SPECIES)) {
          if (nrow(ALL_indicators_2021[ALL_indicators_2021$Variable ==   selection_bystock[3] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == BMT_FLEETSEGMENTS[fle], c(3,8)]) != 0) {
      col_to_add <- ALL_indicators_2021[ALL_indicators_2021$Variable ==   selection_bystock[3] & ALL_indicators_2021$Stock == BMT_SPECIES[spes] & ALL_indicators_2021$Fleet_segment == BMT_FLEETSEGMENTS[fle], c(3,8)]
      col_to_add <- col_to_add[order(col_to_add$ID_scenario),]
      
      col_to_add <- rbind(col_to_add, ALL_indicators_2014[ALL_indicators_2014$Variable ==   selection_bystock[3] & ALL_indicators_2014$Stock == BMT_SPECIES[spes] & ALL_indicators_2014$Fleet_segment == BMT_FLEETSEGMENTS[fle] & ALL_indicators_2021$ID_scenario == SQ_QUO_SCENARIO_NAME, c(3,8)])
      
      TL_FLEET_temp <- cbind(TL_FLEET_temp, col_to_add[,2])
  } else {
      TL_FLEET_temp <- cbind(TL_FLEET_temp, rep(0, (length(scenarios_code)+1)))   
  }
  }
  
  
 colnames(TL_FLEET_temp) <- c("ID_scenario", "total.landings",	"CR.BER", "ROI",	"Revenues",	"Empl", paste("Landing", BMT_SPECIES), paste("F", BMT_SPECIES) )  
 TL_FLEET <- rbind(TL_FLEET, TL_FLEET_temp  )
 
#  head__ <- c("ID_scenario", "Salary",	"CR.BER",	"Revenues",	"Empl", paste("Catch", BMT_SPECIES) )  
  
#TL_FLEET_temp_PERCENTAGE <- data.frame(matrix(nrow=(length(scenarios_code)-1), ncol=length(head__))) 
#colnames(TL_FLEET_temp_PERCENTAGE)  <-  head__
#
TL_FLEET_temp_PERCENTAGE <- TL_FLEET_temp

TL_FLEET_temp_SQ <- TL_FLEET_temp[TL_FLEET_temp$ID_scenario == SQ_QUO_SCENARIO_NAME,]

for (nr in 1:(nrow(TL_FLEET_temp_PERCENTAGE)-1) ) {
if ( TL_FLEET_temp_PERCENTAGE$ID_scenario[nr] != SQ_QUO_SCENARIO_NAME ) {
     for (nc in 2:(ncol(TL_FLEET_temp_PERCENTAGE)-length(BMT_SPECIES))) {
           TL_FLEET_temp_PERCENTAGE[nr,nc] <-   round(( as.numeric(as.character(TL_FLEET_temp_PERCENTAGE[nr,nc] )) - as.numeric(as.character(TL_FLEET_temp_SQ[1,nc])) )/  as.numeric(as.character(TL_FLEET_temp_SQ[1,nc])) * 100 , 2)
     }
     }
}

# TL_FLEET_temp_PERCENTAGE$Total <- rowSums(TL_FLEET_temp_PERCENTAGE[,2:ncol(TL_FLEET_temp_PERCENTAGE)])

head_fleet <- c(BMT_FLEETSEGMENTS[fle], head__percentage[2:length(head__percentage)] )
TL_FLEET_PERCENTAGE <- rbind(TL_FLEET_PERCENTAGE, head_fleet)
colnames(TL_FLEET_PERCENTAGE) <- c("ID_scenario", "total.landings",	"CR.BER", "ROI",	"Revenues",	"Empl", paste("Landing", BMT_SPECIES),paste("F", BMT_SPECIES) ) 
TL_FLEET_PERCENTAGE <- rbind(TL_FLEET_PERCENTAGE, TL_FLEET_temp_PERCENTAGE)
  
}    

  name_all_scenarios <- paste(casestudy_path, "/Traffic lights/", casestudy_name, " - TL data BY FLEET.csv", sep="")
write.table(TL_FLEET , file=name_all_scenarios, sep=";", row.names=F, col.names=F)

  name_all_scenarios <- paste(casestudy_path, "/Traffic lights/", casestudy_name, " - Traffic lights BY FLEET.csv", sep="")
 write.table(TL_FLEET_PERCENTAGE , file=name_all_scenarios, sep=";", row.names=F, col.names=F)

 bmt_wnd_eval$destroy()
wnd <- showMessageOK("        Traffic lights completed!        ")
gtkWidgetSetSensitive(BMTmain_window, T)


}
