# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







#
#
#
#
#
# ---------------------- Run simulation action
run_simulation <- function(widget, window) {      
suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/check_list.r", sep="") ) )
go_on <- TRUE

if (length(FLEETSEGMENTS_names) == 0) {
    go_on <- FALSE
   showError("At least one fleet has to be defined!")

}

for (no_check in 1:length(single_values_to_check_simulation)) {
if (go_on) {

if (gtkToggleButtonGetActive(chkCalibration)) {
   if (single_values_to_check_simulation[[no_check]]$string == "min value in CALIBRATION" | single_values_to_check_simulation[[no_check]]$string == "max value in CALIBRATION") {
      check_res <- check_single_value(single_values_to_check_simulation[[no_check]]$string, single_values_to_check_simulation[[no_check]]$term) 
   if (check_res$result == "KO" & go_on) {
   go_on <- FALSE
   showError(check_res$msg)
   }
   }
} else {
 if (single_values_to_check_simulation[[no_check]]$string != "min value in CALIBRATION" & single_values_to_check_simulation[[no_check]]$string != "max value in CALIBRATION") {
    check_res <- check_single_value(single_values_to_check_simulation[[no_check]]$string, single_values_to_check_simulation[[no_check]]$term) 
   if (check_res$result == "KO" & go_on) {
   go_on <- FALSE
   showError(check_res$msg)
   }
   }
}
}
}

check_ <- check_input("RECRUITMENT_VECTOR", stockrecruitment.SRvector)
if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(check_$msg)
}

if (gtkToggleButtonGetActive(radio_Zentry)) {
 check_ <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.males )
if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(check_$msg)
}

 check_ <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.females )
if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(check_$msg)
}

for (fs in 1:length(FLEETSEGMENTS_names) ) {
check_ <- check_input("SELECTIVITY_TABLE", FleetList_simulation[[fs]]@selectivity.vector)
if (check_$result == "KO" & go_on ) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}

}

}

if (go_on) {
if (gtkComboBoxGetActiveText(combo_Mtype_M) != "M constant") {
if (!is.null(mortality.Mvector.females) ) {
check_ <- check_input("NATURAL_MORTALITY_VECTOR_F", mortality.Mvector.females)
if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(check_$msg)
}
} else {
      showError("All the values in the NATURAL_MORTALITY vector for FEMALES are equal to 0!")
}
}

}

if (go_on) {
 if (gtkComboBoxGetActiveText(combo_Mtype_M) != "M constant") {
if (!is.null(mortality.Mvector.males) )  {
check_ <- check_input("NATURAL_MORTALITY_VECTOR_M", mortality.Mvector.males)
if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(check_$msg)
}   
} else {
          showError("All the values in the NATURAL_MORTALITY vector for MALES are equal to 0!")
}

}
}

check_F_all_segments <- c()

for (fs in 1:length(FLEETSEGMENTS_names) ) {

if ( FleetList_simulation[[fs]]@discard.calculation == "YES" & go_on) {      
if (FleetList_simulation[[fs]]@discard.datatype == "Reverse ogive" ) {
     check_ <- check_input("DISCARD_TABLE", FleetList_simulation[[fs]]@discard.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}
} else {
     check_ <- check_input("DISCARD_EXTERNAL_VECTOR_TABLE_F", FleetList_simulation[[fs]]@discard_extvector.F.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}  else {
     check_ <- check_input("DISCARD_EXTERNAL_VECTOR_TABLE_M", FleetList_simulation[[fs]]@discard_extvector.M.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}
}
}
}

 check_ <- check_input("FISHINGEFFORT_VECTOR", FleetList_simulation[[fs]]@fishingeffort.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}

 check_ <- check_input("PPRODUCTION_VECTOR", FleetList_simulation[[fs]]@pproduction.vector)
     if (check_$result == "KO" & go_on) {
     go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ":", check_$msg))
}



if (FleetList_simulation[[fs]]@production.datatype == "Production data") {
if ( all(FleetList_simulation[[fs]]@production.vector[,3:14] == 0)  & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ": in PRODUCTION .csv file all values are equal to 0!"))
}
}

if (! gtkToggleButtonGetActive(radio_fishingcoeff)) {
  
if ( all(FleetList_simulation[[fs]]@vessels.vector[,3:14] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ": all values for VESSELS are equal to 0!"))
}

if ( all(FleetList_simulation[[fs]]@days.vector[,3:14] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ": all values for DAYS are equal to 0!"))
}


if ( all(FleetList_simulation[[fs]]@gt.vector[,3:14] == 0) & go_on) {
    go_on <- FALSE
    showError(paste(FleetList_simulation[[fs]]@fleetname, ": all values for GTs are equal to 0!"))
}

}


 if (gtkToggleButtonGetActive(radio_Fentry) ){
  fm_M <- FleetList_simulation[[fs]]@fishingmortality.M.vector
  if ( ( all(is.na(fm_M[,2:ncol(fm_M)] )) | all(fm_M[,2:ncol(fm_M)] == 0 )  ) & go_on) {
       check_F_all_segments <- c(check_F_all_segments, F)
      # showError(paste(FleetList_simulation[[fs]]@fleetname, ": values NA in FISHING MORTALITY for MALES!"))
  } else {
      check_F_all_segments <- c(check_F_all_segments, T)
  }
#if ( all(fm_M[,2:ncol(fm_M)] == 0) & go_on) {
#     check_F_all_segments <- c(check_F_all_segments, FALSE)
#    #showError(paste(FleetList_simulation[[fs]]@fleetname, ": all values in FISHING MORTALITY for MALES!"))
#}
#}

  fm_F <- FleetList_simulation[[fs]]@fishingmortality.F.vector
if ( (all(is.na(fm_F[,2:ncol(fm_F)] )) | all((fm_F[,2:ncol(fm_F)] == 0))) & go_on) {
       check_F_all_segments <- c(check_F_all_segments, FALSE)
       #showError(paste(FleetList_simulation[[fs]]@fleetname, ": values NA in FISHING MORTALITY for FEMALES are equal to 0!"))
  } else {
      check_F_all_segments <- c(check_F_all_segments, T)
  } #else {
#if ( all(fm_F[,2:ncol(fm_F)] == 0) & go_on) {
#       check_F_all_segments <- c(check_F_all_segments, FALSE)
#}
#}

} 

}  # end fleet segments loop

# print(check_F_all_segments)

if (!is.null(check_F_all_segments)) {
if (all(!check_F_all_segments)) {
    go_on <- FALSE
    showError("FISHING MORTALITY is equal to 0 (or Not Available) in all the fleet segments!")
 }
 }
 
 
if (go_on) {

if (IN_BEMTOOL) {
BMT_STATE <<- "WAIT"
}
new_aldPopulation <<- setPopulationfromGUI(new_aldPopulation)
new_aldSimulation <<- setSimulationfromGUI(new_aldSimulation)


 #gtkWidgetHide(object)
#main_window$Hide() 

source(paste(ALADYM_home, "/src/runALADYMsimulation.r", sep="") )

reload_fleetsegment_fore_info()

}
} 
