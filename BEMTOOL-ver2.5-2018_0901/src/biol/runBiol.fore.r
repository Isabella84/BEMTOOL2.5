# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

if (!MEY_CALCULATION) {
if (exists("ALADYM_GUI_forecast_int")) { rm(ALADYM_GUI_forecast_int) } 
  ALADYM_GUI_forecast_int <<- vector(mode="list", length=length(BMT_SPECIES))
} else {
    if (MEY_LEVEL == 1) {
    if (exists("ALADYM_GUI_forecast_int")) { rm(ALADYM_GUI_forecast_int) } 
  ALADYM_GUI_forecast_int <<- vector(mode="list", length=length(BMT_SPECIES))
    }
}

BMT_STATE <<- "WAIT"
if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY) {  #1 
      
    ALL_ENTRY_Z <- TRUE
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          ALL_ENTRY_Z <- ifelse(as.character(ALADYM_GUI_simulations[[m_spe]]@enteringMortality) == "Z", TRUE, FALSE )
          } 
      }
      
      if (ALL_ALADYM) {
           if (ALL_ENTRY_Z) {
           
          ALADYM_spe <<- 1
          source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
          INP$Year_simulation  <- simperiod + foreperiod
GLO$L_number <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
forecast <-  simperiod * 12 + 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
          Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))                # initializing the ALADYM GUI
      } else {
           showError("Change of selectivity scenario is not allowed \nif any stock has been simulated with ALADYM-F!") 
        print("SCENARIO 1 is not allowed if any stock has been simulated with ALADYM-F!") 
        
		error <- data.frame(matrix("SCENARIO 1 is not allowed if any stock has been simulated with ALADYM-F!", ncol=1, nrow=1))
		write.table(error, file = paste(casestudy_path, "/info.err", sep=""), sep=";", col.names=F, row.names=F)
		
		}
} else {
        showError("Change of selectivity scenario is not allowed \nif not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 1 is not allowed if not all the stocks have been simulated by ALADYM!")
       		
#		error <- data.frame(matrix("SCENARIO 1 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    
    
}  else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT) {  #2
    
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {
          ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1 
          
                    source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
                    INP$Year_simulation  <- simperiod + foreperiod
GLO$L_number <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
forecast <-  simperiod * 12 + 1
                    Tr <<-  INP$tr 
     if (!MEY_CALCULATION)  {
    
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))  
          BMT_STATE <<- "WAIT"
          } else {
           casestudy_name <<- "HR12-MEY"
          if (MEY_LEVEL == 1) {
             source(paste(ALADYM_home, "/ALADYM.r", sep=""))  
          BMT_STATE <<- "WAIT"
          } else {
              for (spespe in 1:length(BMT_SPECIES) )  {
              ALADYM_spe <<- spespe 
  source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.MEYcode.r", sep="") ) )
   }   
          }
          }
           
    } else {
        showError("Change of effort data scenario is not allowed \nif not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 2 is not allowed if not all the stocks have been simulated by ALADYM!")
         
#		error <- data.frame(matrix("SCENARIO 2 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    
    
} else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) {  #3
  
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {
      
        ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
      
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
         phase <<- "FORECAST"

#        save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\Tables\\[", casestudy_name, "] Mortalities_table SIM.csv", sep="")
        mortalities_table <- read.csv(MORTALITY_table, sep=";") 
        fcurrent_spe <- mortalities_table$Annual_F_estimated[simperiod]
        Ref_point <-  fcurrent_spe * (100 - bmt_forecast_reduction)/100 

        rm(Forecast_reduction)
         Forecast_reduction <- INP$Forecast_reduction
         
#         print(paste("Forecast reduction of", BMT_SPECIES[ALADYM_spe]))
#         print(Forecast_reduction)
         
        fl_ord <- 1
        for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
        if (n_int %in% associated_fleetsegment_indices) {
           Forecast_reduction[1,fl_ord] <- as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])  
           fl_ord <- fl_ord + 1
           }            
        }      
       
#       print(paste("Forecast reduction of", BMT_SPECIES[ALADYM_spe], "from bmtconfig file..."))
           INP$Forecast_reduction <- Forecast_reduction  
     #      print(INP$Forecast_reduction) 
       }    
                     Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))      

    } else {
        showError("Change of fishing mortality (by fleet segment) scenario is not allowed \nif not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 3 is not allowed if not all the stocks have been simulated by ALADYM!")
  
#	   	error <- data.frame(matrix("SCENARIO 3 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {  #4  
# ----------------------------------------------------------------------------
# launch Medium Term Forecast
# ----------------------------------------------------------------------------
     XSAinfo <<- fromBMTtoXSAobject(XSAinfo)
 
 for (m_spe in 1:m_stock) {
     MTF_spe <<- m_spe
     ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", MTF_spe, ".AladymSimulation", sep=""),1])
     SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", MTF_spe, ".StockAssessmentTool", sep=""),1])

 if ((SAtool == "NONE" | SAtool == "SURBA") & !ALADYM_flag)  {
       showError(paste("Change of overall fishing mortality scenario is not allowed for",BMT_SPECIES[MTF_spe],"\nif neither an assessed stocks nor a simulated one is available!")) 
       print(paste("Change of overall fishing mortality scenario is not allowed for",BMT_SPECIES[MTF_spe],"\nif neither an assessed stocks nor a simulated one is available!"), quote=F)
  	   	
error <-data.frame(matrix(paste("Change of overall fishing mortality scenario is not allowed for",BMT_SPECIES[MTF_spe],"if neither an assessed stocks nor a simulated one is available!"), ncol=1, nrow=1))
		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

 } else {
 
   # bloccare l'interfaccia
   
     source(paste(MTF_home , "/runMTF.r", sep=""))
     STF_spe <<- m_spe 
     source(paste(STF_home , "/runSTF.r", sep="")) 
        # ----------------------------------------------------------------------------
        # UPLOAD BMT OBJECTS from MTF after the forecast
        # ----------------------------------------------------------------------------
        print(paste("Updating Biological data from MTF for species [", BMT_SPECIES[MTF_spe], "]", sep=""), quote=FALSE) 
        Interactionsyear <<- updateBiologicalfromMTF(Interactionsyear)
 
 }     
}    
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_STATUS_QUO) {   #5

 ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {

         ALADYM_spe <<- 1
         
        forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
          source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
          
#INP$Year_simulation  <- simperiod + foreperiod
#GLO$L_number <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
#forecast <-  simperiod * 12 + 1
                     Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))  
          
      }    else {
        showError("Status quo scenario is not allowed if not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 5 is not allowed if not all the stocks have been simulated by ALADYM!")
      
#	  error <-data.frame(matrix("SCENARIO 5 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_TAC_VARIATION) {   #6

     ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {
               current_year <<- 1
        #  ALADYM_spe <<- 1
           source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
				  forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
          
        # if (ALL_ALADYM) {
     TAC_results <<- data.frame( matrix(nrow=0,ncol=8) )
    colnames(TAC_results) <-  c( "Stock", "Option_TAC", "Year", "Fleet_segment", "Quota", "Month", "Yield", "Unit")    
          
    species_TAC <<-  as.numeric( substring(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] ), 2, nchar(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] )) ) )  
  BMT_SPECIES_other <<- BMT_SPECIES[c(1:length(BMT_SPECIES)) != species_TAC]
  BMT_SPECIES_other_index <<- which(BMT_SPECIES %in% BMT_SPECIES_other)
  
   option_TAC <<- as.numeric( as.character(cfg[rownames(cfg) == "casestudy.HR6",1] ) )      
      
      ALADYM_spe <<- species_TAC   
    
     current_year <<- 1
    TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- simperiod + current_year
      
          if (current_year > 1 & option_TAC == 3) {
              abundance_path_temp <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - Abundance indices ", harvest_rule_id,".csv", sep="")
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
      
      species_not_TAC_running <<- 0
                      Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))                  # initializing the ALADYM GUI
          BMT_STATE <<- "WAIT"            

          if (FALSE) {
      for (current_year in 1:foreperiod) {

  #  source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )        
    
     #Fleetyear <<- resetDAYSfromTAC(Fleetyear)
     
     yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
     Populations <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$popus
     Interactionsyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$inters
     Fleetyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$fleets 

     Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 

for (spespe in 1:length(BMT_SPECIES) )  {

if (BMT_SPECIES[spespe] %in% BMT_SPECIES_other) {
ALADYM_spe <<- spespe 
 # print(paste("Running ALADYM integrated forecast in ", years.forecast[current_year], " [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=F) 
 
    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )
      yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
     Populations <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$popus
     Interactionsyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$inters
     Fleetyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$fleets 

     Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 

     }
 }
     Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     

 } 

 
  TAC_res_path <<- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - TAC results FORE ", harvest_rule_id,".csv", sep="")
  write.table(TAC_results, file=TAC_res_path, sep=";", row.names=F)
   }     
} else {
        showError("TAC scenario is not allowed if not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 6 is not allowed if not all the stocks have been simulated by ALADYM!")
      
#	  	  error <-data.frame(matrix("SCENARIO 6 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT) {   #7

  ALL_ENTRY_Z <- TRUE
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          ALL_ENTRY_Z <- ifelse(as.character(ALADYM_GUI_simulations[[m_spe]]@enteringMortality) == "Z", TRUE, FALSE )
          } 
      }

           if (ALL_ALADYM) {
           if (ALL_ENTRY_Z) {
          ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
		            source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
		                      Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))                # initializing the ALADYM GUI
      } else {
           showError("Change of selectivity scenario with change of effort data scenario \nis not allowed if any stock has been simulated with ALADYM-F!") 
        print("SCENARIO 7 is not allowed if any stock has been simulated with ALADYM-F!") 
       	  	  
		error <-data.frame(matrix("SCENARIO 7 is not allowed if any stock has been simulated with ALADYM-F!", ncol=1, nrow=1))
		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

      }
} else {
        showError("Change of selectivity scenario with change of effort data scenario \nis not allowed if not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 7 is not allowed if not all the stocks have been simulated by ALADYM!")
        
#		error <-data.frame(matrix("SCENARIO 7 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)
#
}   
 
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL) {  #8
    
 ALL_ENTRY_Z <- TRUE
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          ALL_ENTRY_Z <- ifelse(as.character(ALADYM_GUI_simulations[[m_spe]]@enteringMortality) == "Z", TRUE, FALSE )
          } 
      }

      
           if (ALL_ALADYM) {
           if (ALL_ENTRY_Z) {
          ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
		            source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
		                      Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))                  # initializing the ALADYM GUI
          BMT_STATE <<- "WAIT"                              
      } else {
           showError("Change of selectivity (with investment/disinvestment) scenario is not allowed \nif any stock has been simulated with ALADYM-F!") 
        print("SCENARIO 8 is not allowed if any stock has been simulated with ALADYM-F!")
        
				error <-data.frame(matrix("SCENARIO 8 is not allowed if any stock has been simulated with ALADYM-F!", ncol=1, nrow=1))
		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

      }
} else {
        showError("Change of selectivity (with investment/disinvestment) scenario is not allowed \nif not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 8 is not allowed if not all the stocks have been simulated by ALADYM!")
        
#		error <-data.frame(matrix("SCENARIO 8 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL) {    #9

# bmt_average_forecast <<-  as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR8",1]))
    
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {
#
    ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
		            source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep="")) 
                          Tr <<-  INP$tr 
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))                  # initializing the ALADYM GUI
          BMT_STATE <<- "WAIT"                              


#      for (current_year in 1:foreperiod) {
#    current_year <<- current_year
#    TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<- simperiod + current_year
#     
#     Fleetyear <<- BMTFlbeh(option, Fleetyear, fdmat, eimat, admat, tpmat, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR, simperiod, length(BMT_FLEETSEGMENTS))     
#     Fleetyear <<- setEffortVarsForecast(Fleetyear) 
#  
#for (spespe in 1:length(BMT_SPECIES) )  {
#ALADYM_spe <<- spespe 
#
# # print(paste("Running ALADYM integrated forecast in ", years.forecast[current_year], " [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=F) 
# 
#    source(paste(ALADYM_home, "/src/runALADYMforecast.int.r", sep="") )
#      yy <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
#     Populations <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$popus
#     Interactionsyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$inters
#     Fleetyear <<- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)$fleets 
#
#     Fleetyear <<- updateFleetfromInteraction(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR) 
#
#   }
#     Fleetyear <<- runEcon.fore(Fleetyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)     
#  } 
} else {
        showError("Change of effort data (with investment/disinvestment) scenario is not allowed\nif not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 9 is not allowed if not all the stocks have been simulated by ALADYM!")
        
#		error <-data.frame(matrix("SCENARIO 9 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL) {    #10

 ALL_ENTRY_Z <- TRUE
    ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          ALL_ENTRY_Z <- ifelse(as.character(ALADYM_GUI_simulations[[m_spe]]@enteringMortality) == "Z", TRUE, FALSE )
          } 
      }
      
      if (ALL_ALADYM) {
           if (ALL_ENTRY_Z) {
          ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
		            source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
		                      Tr <<-  INP$tr
          source(paste(ALADYM_home, "/ALADYM.r", sep=""))              # initializing the ALADYM GUI
          BMT_STATE <<- "WAIT"              
      } else {
           showError("Change of selectivity with change of effort data (with investment/disinvestment)\nscenario is not allowed if any stock has been simulated with ALADYM-F!") 
        print("SCENARIO 10 is not allowed if any stock has been simulated with ALADYM-F!") 
       
	    error <-data.frame(matrix("SCENARIO 10 is not allowed if any stock has been simulated with ALADYM-F!", ncol=1, nrow=1))
		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

      }
} else {
        showError("Change of selectivity with change of effort data (with investment/disinvestment)\nscenario is not allowed if not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 10 is not allowed if not all the stocks have been simulated by ALADYM!")
        
#		error <-data.frame(matrix("SCENARIO 10 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_STATUS_QUO_BEHAVIOURAL) {   #11

  ALL_ALADYM <- TRUE
      for (m_spe in 1:m_stock) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
      if (ALL_ALADYM) {
      
         ALADYM_spe <<- 1
          forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
		            source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  
        #  source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  

         current_year <<- 1
          
forecast <-  simperiod * 12 + (current_year-1)*12 +1
GLO$L_number <- simperiod * 12 + current_year*12 
INP$Year_simulation  <- simperiod + foreperiod
bmt_average_forecast <<-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),2]))  
print("Simulating years:", quote=F)
print(paste("from", forecast , "to",  GLO$L_number), quote=F)
          
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle_fore.Rdata", sep="")
load(path_to_save)  
FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#print(paste("prima di chiamare ALADYM.r carico anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)
              Tr <<-  INP$tr     
source(paste(ALADYM_home, "/ALADYM.r", sep=""))                  # initializing the ALADYM GUI
          
#print(paste("salvo alla fine di anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]])    

#print(paste("dopo aggiornamento prima di run forecast anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)

ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast 

#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/GUIfle_fore.Rdata", sep="")
#save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  
         
          BMT_STATE <<- "WAIT"                              

} else {
        showError("Status quo (with investment/disinvestment)\nscenario is not allowed if not all the stocks have been simulated by ALADYM!") 
        print("SCENARIO 11 is not allowed if not all the stocks have been simulated by ALADYM!")
      
#	  		error <-data.frame(matrix("SCENARIO 11 is not allowed if not all the stocks have been simulated by ALADYM!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

}   

}
