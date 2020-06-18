# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


saveBMTCFG_scenario<-function(w) {
     
#     setScenarioOptions()

max_ncol <- c()
sum_rows <- 0
 
     for (matt in BMTCFG_SUBTABLES_ECONOMIC_FORECAST ) {
     if (matt != "mat_cfg_assessment_tools") {
         obj <- data.frame(get(matt))
         max_ncol <- c(max_ncol, ncol(obj))
         sum_rows <- sum_rows + nrow(obj)    
#     print(obj)
#          print("-------------------------------------------------------------------------------------------------")
     }
     }
                                                       # , nrow=sum_rows
FINAL_BMTCFG <- data.frame(matrix("", ncol= max(max_ncol), nrow=0), stringsAsFactors =F)
            colnames(FINAL_BMTCFG) <- paste("col_", c(1:ncol(FINAL_BMTCFG)), sep="")
            
       for (matt in 1:length(BMTCFG_SUBTABLES_ECONOMIC_FORECAST) ) {

       
     if ( BMTCFG_SUBTABLES_ECONOMIC_FORECAST[matt] != "mat_cfg_assessment_tools" ) {
              obj <- get(BMTCFG_SUBTABLES_ECONOMIC_FORECAST[matt])    
              
#              BMTCFG_SUBTABLES <<- c("mat_cfg_general", "mat_cfg_F", "mat_cfg_S", "mat_cfg_SF", "mat_cfg_species_settings", "mat_cfg_assessment_tools", "mat_cfg_ALADYM_sim", "mat_cfg_general_fore",  "mat_cfg_EffortData", "mat_cfg_LandingData", "mat_cfg_REF_points", "mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress")
#
#              
#              if ( !(BMTCFG_SUBTABLES_ECONOMIC_PARAMS[matt] %in% c("mat_cfg_general_fore",  "mat_cfg_EffortData", "mat_cfg_LandingData","mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress", "mat_cfg_EconomicIndicator")) ) {     
#              obj <- rbind(colnames(obj), obj)
#              }
              to_add <- data.frame(cbind(obj, matrix("", ncol=(ncol(FINAL_BMTCFG)-ncol(obj)), nrow=nrow(obj))) )
           colnames(to_add) <- paste("col_", c(1:ncol(FINAL_BMTCFG)), sep="")
               FINAL_BMTCFG <- rbind(FINAL_BMTCFG, to_add)     
         } 
    
     }

#write.table(FINAL_BMTCFG,   paste(mat_cfg_general[1,3],  "/HR", mat_cfg_scenario_settings_fore[2,2],"-", mat_cfg_scenario_settings_fore[2,3], "/bmtconfig - HR", mat_cfg_scenario_settings_fore[2,2],"-", mat_cfg_scenario_settings_fore[2,3], ".csv", sep=""), sep=";", row.names=F, col.names = F, na="")

return(FINAL_BMTCFG)
}