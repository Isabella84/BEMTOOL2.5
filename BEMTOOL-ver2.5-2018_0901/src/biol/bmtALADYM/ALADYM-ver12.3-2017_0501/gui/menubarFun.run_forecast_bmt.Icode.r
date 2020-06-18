# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



########################################################################################################################################
########################################################################################################################################
########################################################################################################################################  
  BMT_STATE <<- "WAIT"  
  current_year <<- 1

source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
                #  INP$Recruits
if (current_year == 1) {
 #  new_aldForecast <<- setForecastfromGUI(new_aldForecast)
#   go_on <<- check_aldForecast(new_aldForecast)
   ALADYM_GUI_forecast_int[[ALADYM_spe]] <- new_aldForecast 
   ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]] 
} else {
   new_aldForecast <<-  ALADYM_GUI_forecast_int[[ALADYM_spe]] #ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]] 
}

  associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
 

FleetList_forecast <- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#print(paste("alla chiamata di run forecast anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)  
  
 n_ord <- 1                                 
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
    if (n_int %in% associated_fleetsegment_indices) {
      # FleetList_forecast <<-
      update_BMT_fleetsegments_fore.int(n_ord, n_int) #, FleetList_forecast
      n_ord <- n_ord+1
      # FleetList_simulation
    }
}
.GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]  <- FleetList_forecast 

#print(paste("dopo update_BMT_.... anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)  

    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )

    if (BMT_SCENARIO == BMT_HR_TAC_VARIATION) {
      Fleetyear <<- resetDAYSfromTAC(Fleetyear)    

    }
    
              yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
              biologicalUpdateResults_fore <- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)
              Populations <<- biologicalUpdateResults_fore$popus
              Interactionsyear <<- biologicalUpdateResults_fore$inters
              Fleetyear <<- biologicalUpdateResults_fore$fleets 


   	#	if (RUN_CI_FORE) {
      biologicalUpdateResults_fore <- updateBiologicalfromALADYM.fore.CI(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)
     Populations <<- biologicalUpdateResults_fore$popus
     Interactionsyear <<- biologicalUpdateResults_fore$inters
     Fleetyear <<- biologicalUpdateResults_fore$fleets  
		#	}       
      
      # Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 
# ######################################################################################################################
# ######################################################################################################################
# ######################################################################################################################

if ( BMT_SCENARIO != BMT_HR_TAC_VARIATION) {

ALADYM_spe <<- ALADYM_spe + 1

if (ALADYM_spe <= length(BMT_SPECIES)) {
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])

  if (ALADYM_flag) { 
        # to launch ALADYM with the GUI                        
              forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
              source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))
   
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]] 
                   
#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "ALADYM/GUIfle_fore.Rdata", sep="")
#load(path_to_save) 

#FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#print(paste("prima di chiamare ALADYM.r carico anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)
              
source(paste(ALADYM_home, "/ALADYM.r", sep=""))  

#print(paste("dopo aggiornamento prima di run forecast anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)

 .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast 

#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/GUIfle_fore.Rdata", sep="")
#save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  
              
#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/GUIfle_fore.Rdata", sep="")
#save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  
              
    }
      BMT_STATE <<- "WAIT" 
            
   }  else {
   # from the second year of the forecast
   
           Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)
       Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     

                                    
#FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]
         
for (current_year in 2:foreperiod) {

    current_year <<- current_year
    TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- simperiod + current_year
    
 Fleetyear <<- BMTFlbeh(option, Fleetyear, fdmat, eimat, admat, tpmat, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR, simperiod, length(BMT_FLEETSEGMENTS))
      Fleetyear <<- setEffortVarsForecast(Fleetyear)
  
  
for (spespe in 1:length(BMT_SPECIES) )  {
 suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.Icode_from2.r", sep="") ) ) 
   }        
	 
	  Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)
     Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     

    # per stampareeeeeeeeeeeeeeeeeeeee da eliminareeeeeeeeeeeeeeeeeeeeeeeeee
                  #fleet_to_print <- c(1:length(BMT_FLEETSEGMENTS))
#                  print("*********************************************************************", quote=F)
#                  print(years.forecast[current_year], quote=F)
#                  print("*********************************************************************", quote=F)
#                  for (fleet_all in fleet_to_print ) {
#                        print(Fleetyear[[current_year+simperiod]]@fleetsegments[[fleet_all]]@Economic.indicators)
#                  }
     
     TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR  + 1
     
     if (TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <= foreperiod) {
     Fleetyear <<- BMTFlbeh(option, Fleetyear, fdmat, eimat, admat, tpmat, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR, simperiod, length(BMT_FLEETSEGMENTS))     
     Fleetyear <<- setEffortVarsForecast(Fleetyear) 
     }  
 }
 
 BMT_STATE <<- "FORECAST"
 source(suppressWarnings(paste(getwd(), "/src/runBEMTOOLforecast.r", sep="") ) )   
   }
   
   } else {                      # scenario TAC

# ######################################################################################################################
# ######################################################################################################################
# ######################################################################################################################
  species_not_TAC_running <<- species_not_TAC_running + 1
   
ALADYM_spe <<- BMT_SPECIES_other_index[species_not_TAC_running]

if (species_not_TAC_running <= length(BMT_SPECIES_other_index)) {
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])

  if (ALADYM_flag) { 
        # to launch ALADYM with the GUI                        
              forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
              source(paste(ALADYM_home, "/ALADYM.r", sep=""))                
    }
      BMT_STATE <<- "WAIT" 
            
   }  else {
   # from the second year of the forecast
       Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     

   
for (current_year in 2:foreperiod) {

#     TAC_results <<- data.frame(matrix(nrow=0,ncol=8))
#    colnames(TAC_results) <-  c( "Stock", "Option_TAC", "Year", "Fleet_segment", "Quota", "Month", "Yield", "Unit")


      current_year <<- current_year
    TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- simperiod + current_year
     
  species_TAC <<-  as.numeric( substring(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] ), 2, nchar(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] )) ) )  
  BMT_SPECIES_other <<- BMT_SPECIES[c(1:length(BMT_SPECIES)) != species_TAC]
   option_TAC <<- as.numeric( as.character(cfg[rownames(cfg) == "casestudy.HR6",1] ) )
   
   
      ALADYM_spe <<- species_TAC 
 # print(paste("Running ALADYM integrated forecast in ", years.forecast[current_year], " [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=F) 
      
       if (current_year > 1 & option_TAC == 3) {
              abundance_path_temp <- paste(casestudy_path, "\\",harvest_rule_id, "\\", casestudy_name, " - Abundance indices - ", harvest_rule_id,".csv", sep="")
              abundance_mat <- read.csv(abundance_path_temp, sep=";")
   
              mean_SSBindex <- mean(abundance_mat$Index_SSB[abundance_mat$Year %in% years], na.rm=TRUE)

              vect_SSB <- c(NA)
              for (yea in 1:length(years)) {
              vect_SSB <- c(vect_SSB, mean(as.numeric(as.character(Interactionsyear[[yea]][[ALADYM_spe]]@exploitedStock@SSB)))) 
              }
              
              mean_SSB <- mean(vect_SSB, na.rm=TRUE)
              
              thisyear_SSB <- mean(as.numeric(as.character(Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]][[ALADYM_spe]]@exploitedStock@SSB)))
             
              this_SSBindex <- thisyear_SSB * mean_SSBindex /  mean_SSB
             
              to_add <- data.frame(cbind(years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod], this_SSBindex ))
              colnames(to_add) <- colnames(abundance_mat)

              abundance_mat <- rbind(abundance_mat , to_add)
              
              write.table(abundance_mat, file=abundance_path_temp, sep=";", row.names=FALSE) 
       }
      
      BMT_TAC <<- calculateTAC()
      
    print(paste("TAC in year", years.forecast[current_year],":", round(BMT_TAC,2) , "to be splitted among fleet segments"), quote=F )
    
    
for (this_spe in 1:length(ALADYM_GUI_populations)) {

    if  (ALADYM_GUI_fleets[[this_spe]]$species == ALADYM_spe) {
          FleetList_simulation <<- ALADYM_GUI_fleets[[this_spe]]$aldFleets
          FleetList_forecast <<- ALADYM_GUI_fleets_fore[[this_spe]]$aldFleets
    }
}

   new_aldForecast <<- ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]]["aldPopulation"]$aldPopulation
   new_aldSimulation <<- ALADYM_GUI_simulations[[ALADYM_spe]]["aldSimulation"]$aldSimulation
    
source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

 n_ord <- 1                                 
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
    if (n_int %in% associated_fleetsegment_indices) {
      # FleetList_forecast <<-
      update_BMT_fleetsegments_fore.int(n_ord, n_int) #, FleetList_forecast
      n_ord <- n_ord+1
      # FleetList_simulation
    }
}

fleet_list <- list(species=ALADYM_spe, aldFleets=FleetList_forecast) 

#if (!exists("ALADYM_GUI_fleets_fore") ) {
   ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- fleet_list

    
    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )        
    
     Fleetyear <<- resetDAYSfromTAC(Fleetyear)
     
     yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
     Populations <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$popus
     Interactionsyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$inters
     Fleetyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$fleets 

     Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 

for (spespe in 1:length(BMT_SPECIES) )  {

if (BMT_SPECIES[spespe] %in% BMT_SPECIES_other) {
ALADYM_spe <<- spespe 

for (this_spe in 1:length(ALADYM_GUI_populations)) {

    if  (ALADYM_GUI_fleets[[this_spe]]$species == ALADYM_spe) {
          FleetList_simulation <<- ALADYM_GUI_fleets[[this_spe]]$aldFleets
          FleetList_forecast <<- ALADYM_GUI_fleets_fore[[this_spe]]$aldFleets
    }
}

 new_aldForecast <<- ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]]["aldPopulation"]$aldPopulation
   new_aldSimulation <<- ALADYM_GUI_simulations[[ALADYM_spe]]["aldSimulation"]$aldSimulation
    
source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

 n_ord <- 1                                 
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
    if (n_int %in% associated_fleetsegment_indices) {
      # FleetList_forecast <<-
      update_BMT_fleetsegments_fore.int(n_ord, n_int) #, FleetList_forecast
      n_ord <- n_ord+1
      # FleetList_simulation
    }
}

fleet_list <- list(species=ALADYM_spe, aldFleets=FleetList_forecast) 

#if (!exists("ALADYM_GUI_fleets_fore") ) {
   ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- fleet_list

    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )
      yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
     Populations <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$popus
     Interactionsyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$inters
     Fleetyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$fleets 
     Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 

     }
 }

  
     Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     

    # per stampareeeeeeeeeeeeeeeeeeeee da eliminareeeeeeeeeeeeeeeeeeeeeeeeee
                  fleet_to_print <- c(1:length(BMT_FLEETSEGMENTS))
                  print("*********************************************************************", quote=F)
                  print(years.forecast[current_year], quote=F)
                  print("*********************************************************************", quote=F)
                  for (fleet_all in fleet_to_print ) {
                        print(Fleetyear[[current_year+simperiod]]@fleetsegments[[fleet_all]]@Economic.indicators)
                  }
     
     TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR  + 1
     
#     if (TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <= foreperiod) {
#     Fleetyear <<- BMTFlbeh(option, Fleetyear, fdmat, eimat, admat, tpmat, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR, simperiod, length(BMT_FLEETSEGMENTS))     
#     Fleetyear <<- setEffortVarsForecast(Fleetyear) 
#     } 

 TAC_res_path <<- paste(casestudy_path, "\\",harvest_rule_id, "\\", casestudy_name, " - TAC results - ", harvest_rule_id,".csv", sep="")
  write.table(TAC_results, file=TAC_res_path, sep=";", row.names=F)
   
 }
 
 BMT_STATE <<- "FORECAST"
 source(suppressWarnings(paste(getwd(), "/src/runBEMTOOLforecast.r", sep="") ) )   
   }

   }
