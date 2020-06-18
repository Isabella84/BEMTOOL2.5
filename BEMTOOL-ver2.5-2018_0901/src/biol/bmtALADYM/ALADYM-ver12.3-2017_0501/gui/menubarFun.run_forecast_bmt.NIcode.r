# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



new_aldForecast <<- setForecastfromGUI(new_aldForecast)
current_year <<- foreperiod

           source(paste(ALADYM_home, "/src/runALADYMforecast.r", sep="") )
              print(paste("Updating Biological data from ALADYM forecast for species [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=FALSE )
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
							             
              # fleet_list <- list(species=ALADYM_spe, aldFleets=FleetList_forecast) 

ALADYM_spe <<- ALADYM_spe + 1
if (ALADYM_spe <= length(BMT_SPECIES)) {
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])

  if (ALADYM_flag) { 
              forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  + 1
                   
                   if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) {
                        source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))
                        
                        associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
                        associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
                        associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
                        
                        # SETTING OF ALADYM VARIABLES FOR THE FORECAST PHASE
                        # read the forecast reduction input by the user
                        bmt_forecast_reduction <-  as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",2]))
                        # set the years of all the simulation and forecast for non integrated approach    
                        INP$Year_simulation <- simperiod + foreperiod
                        # set the time span in which get the reduction input by the user
                        bmt_time_span <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",1]))
                        # set the reference month 
                        Ref_month <- (simperiod + bmt_time_span) * 12
                        
        phase <<- "SIMULATION"
        source( paste(ALADYM_home, "/src/paths.r", sep="") )    
        mortalities_table <- read.csv(MORTALITY_table, sep=";") 
        phase <<- "FORECAST"
        source( paste(ALADYM_home, "/src/paths.r", sep="") )    
        
        fcurrent_spe <- mortalities_table$Annual_F_estimated[simperiod]
        Ref_point <-  fcurrent_spe * (100 - bmt_forecast_reduction)/100 
                        
                       # mortalities_table <- read.csv(MORTALITY_table, sep=";") 
#                        fcurrent_spe <- mortalities_table$Annual_F_estimated[simperiod]
#                        Ref_point <-  fcurrent_spe * (100 - bmt_forecast_reduction)/100 
                        
                        rm(Forecast_reduction)
                        Forecast_reduction <- INP$Forecast_reduction
                        
                        fl_ord <- 1
                        for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
                        if (n_int %in% associated_fleetsegment_indices) {
                        Forecast_reduction[1,fl_ord] <- as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])  
                        fl_ord <- fl_ord + 1
                        }            
                        }      
                        INP$Forecast_reduction <- Forecast_reduction  
       } 
          
source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))
source(paste(ALADYM_home, "/ALADYM.r", sep=""))      
    }
      BMT_STATE <<- "WAIT"  
      
   } else {
   
   if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) {
     source(paste(getwd(), "/src/biol/bmtALADYM/save_mortalities_change.r", sep=""))                          
     Fleetyear <<- setEffortVarsForecast(Fleetyear)
     }
       
      BMT_STATE <<- "FORECAST"
      source(suppressWarnings(paste(getwd(), "/src/runBEMTOOLforecast.r", sep="") ) ) 
   }
   