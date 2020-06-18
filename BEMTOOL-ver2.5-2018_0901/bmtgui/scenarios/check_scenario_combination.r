# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


check_scenario_combination<-function() {

allowed_scenario <- FALSE

checked_rules <- rep(FALSE, 7)

    checked_rules[1] <- gtkToggleButtonGetActive(bmt_chk_hr_statusquo) 
         checked_rules[2] <- gtkToggleButtonGetActive(bmt_chk_hr_behavioural_module) 
             checked_rules[3] <- gtkToggleButtonGetActive(bmt_chk_hr_change_selectivity) 
                 checked_rules[4] <- gtkToggleButtonGetActive(bmt_chk_hr_change_effort) 
                     checked_rules[5] <- gtkToggleButtonGetActive(bmt_chk_hr_change_f_by_fleet) 
                         checked_rules[6] <- gtkToggleButtonGetActive(bmt_chk_hr_change_total_f) 
                             checked_rules[7] <- gtkToggleButtonGetActive(bmt_chk_hr_set_tac) 
                                  checked_rules[8] <- gtkToggleButtonGetActive(bmt_chk_hr_MEY) 
                           
    if (any(checked_rules[])) {
    
    
if ( (all(checked_rules[8]) & (all(!checked_rules[c(1:7)]))) )  {
        print("Selected MEY CALCULATION", quote=F)
        allowed_scenario <- TRUE
          BMT_SCENARIO <<- 12
           MEY_CALCULATION <<- TRUE
} else if ( (checked_rules[1] & (all(!checked_rules[2:8]))) ) {
        print(paste("Selected ex-scenario", BMT_HR_STATUS_QUO, "[STATUS QUO]", sep=" "), quote=F)       
        allowed_scenario <- TRUE
              BMT_SCENARIO <<-  BMT_HR_STATUS_QUO
                MEY_CALCULATION <<- F
} else if ( (checked_rules[2] & (all(!checked_rules[c(1, 3:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_STATUS_QUO_BEHAVIOURAL, "[BEHAVIOURAL MODULE]", sep=" "), quote=F)    
        allowed_scenario <- TRUE
              BMT_SCENARIO <<-  BMT_HR_STATUS_QUO_BEHAVIOURAL
                              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[c(2,3)]) & (all(!checked_rules[c(1,4:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL, "[CHANGE SELECTIVITY WITH BEHAVIOURAL MODULE]", sep=" "), quote=F)    
        allowed_scenario <- TRUE
              BMT_SCENARIO <<- BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL
                              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[c(2,4)]) & (all(!checked_rules[c(1,3,5:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, "[CHANGE FISHING EFFORT WITH BEHAVIOURAL MODULE]", sep=" "), quote=F)
        allowed_scenario <- TRUE
              BMT_SCENARIO <<-   BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL
                              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[c(2,3,4)]) & (all(!checked_rules[c(1,5:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL, "[CHANGE SELECTIVITY & FISHING EFFORT WITH BEHAVIOURAL MODULE]", sep=" "), quote=F)
        allowed_scenario <- TRUE
              BMT_SCENARIO <<- BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL
                              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[c(3,4)]) & (all(!checked_rules[c(1,2,5:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, "[CHANGE SELECTIVITY & FISHING EFFORT]", sep=" "), quote=F)
        allowed_scenario <- TRUE
              BMT_SCENARIO <<- BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT
              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[3]) & (all(!checked_rules[c(1,2,4:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_SELECTIVITY, "[CHANGE SELECTIVITY]", sep=" "), quote=F)
        allowed_scenario <- TRUE
              BMT_SCENARIO <<- BMT_HR_CHANGE_SELECTIVITY
              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[4]) & (all(!checked_rules[c(1:3,5:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_FISHEFFORT, "[CHANGE FISHING EFFORT]", sep=" "), quote=F)
        allowed_scenario <- TRUE
              BMT_SCENARIO <<-  BMT_HR_CHANGE_FISHEFFORT
              MEY_CALCULATION <<- F
} else if ( (all(checked_rules[5]) & (all(!checked_rules[c(1:4, 6:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_FISHMORTALITY, "[CHANGE FISHING MORTALITY BY FLEET]", sep=" "), quote=F)
        allowed_scenario <- TRUE
        BMT_SCENARIO <<-  BMT_HR_CHANGE_FISHMORTALITY
        MEY_CALCULATION <<- F
} else if ( (all(checked_rules[6]) & (all(!checked_rules[c(1:5, 7:8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_CHANGE_TOTAL_FISHMORTALITY, "[CHANGE TOTAL FISHING MORTALITY]", sep=" "), quote=F)
        allowed_scenario <- TRUE
        BMT_SCENARIO <<-  BMT_HR_CHANGE_TOTAL_FISHMORTALITY
        MEY_CALCULATION <<- F
} else if ( (all(checked_rules[7]) & (all(!checked_rules[c(1:6, 8)]))) )  {
        print(paste("Selected ex-scenario", BMT_HR_TAC_VARIATION, "[TAC VARIATION]", sep=" "), quote=F)
        allowed_scenario <- TRUE
        BMT_SCENARIO <<-  BMT_HR_TAC_VARIATION
        MEY_CALCULATION <<- F
}

print(ifelse(allowed_scenario, "OK", "NOT ALLOWED!"))

if (!allowed_scenario) {
            gtkToggleButtonSetActive(bmt_chk_hr_statusquo, F) 
         gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, F) 
             gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, F) 
               gtkToggleButtonSetActive(bmt_chk_hr_change_effort, F) 
            gtkToggleButtonSetActive(bmt_chk_hr_change_f_by_fleet, F) 
            gtkToggleButtonSetActive(bmt_chk_hr_change_total_f, F) 
             gtkToggleButtonSetActive(bmt_chk_hr_set_tac, F)
              gtkToggleButtonSetActive(bmt_chk_hr_MEY, F)
           showError("Combination of management rules not allowed!")
           BMT_SCENARIO <<- -1
}
                                                         
} else {
                showError("Select at least one management rule!")
              BMT_SCENARIO <<- -1
}

return(BMT_SCENARIO)
}