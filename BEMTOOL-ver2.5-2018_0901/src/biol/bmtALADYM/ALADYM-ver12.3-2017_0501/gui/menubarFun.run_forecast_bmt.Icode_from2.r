# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



ALADYM_spe <<- spespe 

  source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))
  
forecast <-  simperiod * 12 + (current_year-1)*12 +1
GLO$L_number <- simperiod * 12 + current_year*12 
INP$Year_simulation  <- simperiod + foreperiod
bmt_average_forecast <<-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),2]))  
print("Simulating years:", quote=F)
print(paste("from", forecast , "to",  GLO$L_number), quote=F)

#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/GUIfle_fore.Rdata", sep="")
#load(path_to_save, envir=.GlobalEnv)  

ALADYM_GUI_fleets_fore <<- ALADYM_GUI_fleets_fore_succ

#print(paste("prima di chiamare update_BMT_.... carico anno ",current_year," specie", ALADYM_spe))
#print(ALADYM_GUI_fleets_fore[[ALADYM_spe]][[1]]@fishingeffort.vector)  
#
  FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]
  
          FleetList_simulation <<- ALADYM_GUI_fleets[[ALADYM_spe]]
         # FleetList_forecast <<- ALADYM_GUI_fleets_fore[[ALADYM_spe]]
          
   new_aldForecast <<- ALADYM_GUI_forecast_int[[ALADYM_spe]]
   new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]]
   new_aldSimulation <<- ALADYM_GUI_simulations[[ALADYM_spe]]

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



  .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]  <-   FleetList_forecast 

#print(paste("salvataggio di dopo update_BMT_.... e prima di runALADYMforecast.int.r carico anno ",current_year," specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)
#
    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )
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
	#		}  