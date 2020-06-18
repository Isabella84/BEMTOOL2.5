# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






for (no_check in 1:length(single_values_to_check_forecast)) {
if (go_on) {

if (single_values_to_check_forecast[[no_check]]$string == "Target F in FORECAST" ) {

   if (gtkToggleButtonGetActive(chkFMSY)) {
      check_res <- check_single_value(single_values_to_check_forecast[[no_check]]$string, single_values_to_check_forecast[[no_check]]$term) 
   if (check_res$result == "KO") {
   go_on <- FALSE
   showError(check_res$msg)
   }
  } 
   
   } else if (single_values_to_check_forecast[[no_check]]$string == "Target year in FORECAST" ) {

   if (gtkToggleButtonGetActive(chkFMSY)) {
      check_res <- check_single_value(single_values_to_check_forecast[[no_check]]$string, single_values_to_check_forecast[[no_check]]$term) 
   if (check_res$result == "KO") {
   go_on <- FALSE
   showError(check_res$msg)
   }
  } 
   
   }  else if (single_values_to_check_forecast[[no_check]]$string == "Constant recruitment in FORECAST") {
	   	 if (gtkToggleButtonGetActive(radio_forecast_recruits_costant))  {
	   	 check_res <- check_single_value(single_values_to_check_forecast[[no_check]]$string, single_values_to_check_forecast[[no_check]]$term) 
	        if (check_res$result == "KO") {
   go_on <- FALSE
   showError(check_res$msg)
   }
	 }
	 }
   }
   }


   if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION")) {
       checkentryM <- gtkToggleButtonGetActive(radio_Zentry)  
   } else {
       checkentryM <- ifelse(new_aldSimulation@enteringMortality =="Z", T, F) 
   }
  

if (checkentryM & !gtkToggleButtonGetActive(chkFMSY)) {

for (fs in 1:length(FLEETSEGMENTS_names) ) {
check_ <- check_input("SELECTIVITY_TABLE_FORE", FleetList_forecast[[fs]]@selectivity.vector)
if (check_$result == "KO" & go_on ) {
     go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}
}
}




for (fs in 1:length(FLEETSEGMENTS_names) ) {

if ( FleetList_forecast[[fs]]@discard.calculation == "YES" & go_on ) {

if ( FleetList_forecast[[fs]]@discard.datatype == "Reverse ogive") {
     check_ <- check_input("DISCARD_TABLE_FORE", FleetList_forecast[[fs]]@discard.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}

}
}

}


if (!gtkToggleButtonGetActive(chkFMSY)) {

for (fs in 1:length(FLEETSEGMENTS_names) ) {

if (!checkentryM) {
 check_ <- check_input("FISHINGEFFORT_VECTOR_FORE", FleetList_forecast[[fs]]@fishingeffort.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}
}

 check_ <- check_input("PPRODUCTION_VECTOR_FORE", FleetList_forecast[[fs]]@pproduction.vector)
     if (check_$result == "KO" & go_on) {
   #  go_on <- FALSE
   # just a warning
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}


#if ( all(FleetList_forecast[[fs]]@production.vector[,2:13] == 0)  & go_on) {
#    go_on <- FALSE
#    showError(paste(FleetList_forecast[[fs]]@fleetname, ": in PRODUCTION .csv file all values are equal to 0!"))
#}

if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY) {

if (! gtkToggleButtonGetActive(radio_fishingcoeff)) {

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@vessels.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@vessels.vector[1:current_year,2:13]
}

if ( all(matrix_to_be_checked[,] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for VESSELS are equal to 0!"))
}

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@days.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@days.vector[1:current_year,2:13]
}


if ( all(matrix_to_be_checked[,] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for DAYS are equal to 0!"))
}

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@gt.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@gt.vector[1:current_year,2:13]
}

if ( all(matrix_to_be_checked[,] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for GTs are equal to 0!"))
}
}
 }


}  

}
 