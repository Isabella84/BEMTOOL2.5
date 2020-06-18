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
# Function that load parameters of a fleet segment selected in the combobox (FORECAST) taking all the parameters from the R object
#           
loadFleetsegment_foreintoGUI<-function(object) {


# object = FleetList_forecast[[1]]

if (object@EffortF.relationship$relationship_type[1] == "L") {
      gtkComboBoxSetActive(combo_EffortF, (which(EFFORT_F_TYPE == EFFORT_F_TYPE[1])-1) ) 
} else {
     gtkComboBoxSetActive(combo_EffortF, (which(EFFORT_F_TYPE == EFFORT_F_TYPE[2])-1) ) 
}

  gtkEntrySetText(entry_EffortF_a, as.numeric(as.character(object@EffortF.relationship$a[1])) )
    gtkEntrySetText(entry_EffortF_b,  as.numeric(as.character(object@EffortF.relationship$b[1])) )

if (length(new_aldSimulation@enteringMortality) != 0) {
if (new_aldSimulation@enteringMortality == "Z") {

 if (object@selectivity.mode == "params") {
 
fleet.selectivity_fore <<- object@selectivity.vector

if (!is.null(fleet.selectivity_fore) ) {
if (nrow(fleet.selectivity_fore) > 0) {
   reload_selectivity_fore()
} else {
   reload_EMPTY_selectivity_fore()
}
} else {
  reload_EMPTY_selectivity_fore()
}
} else {
   reload_EMPTY_selectivity_fore()
}
#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.selectivityTable.r", sep=""))  )  


 if (object@selectivity.mode == "age") {
 
SelectivityAgeF_fore_matrix <<- object@SelectivityAge.F.vector

if (!is.null(SelectivityAgeF_fore_matrix) ) {
if (nrow(SelectivityAgeF_fore_matrix) > 0) {
   reload_SelectivityAgeF_fore()
} else {
   reload_EMPTY_SelectivityAgeF_fore()
}
} else {
  reload_EMPTY_SelectivityAgeF_fore()
}


SelectivityAgeM_fore_matrix <<- object@SelectivityAge.M.vector

if (!is.null(SelectivityAgeM_fore_matrix) ) {
if (nrow(SelectivityAgeM_fore_matrix) > 0) {
   reload_SelectivityAgeM_fore()
} else {
   reload_EMPTY_SelectivityAgeM_fore()
}
} else {
  reload_EMPTY_SelectivityAgeM_fore()
}

} else {

  reload_EMPTY_SelectivityAgeF_fore()
  reload_EMPTY_SelectivityAgeM_fore()
}



 if (object@selectivity.mode == "length") {
 
SelectivityLengthF_fore_matrix <<- object@SelectivityLength.F.vector

if (!is.null(SelectivityLengthF_fore_matrix) ) {
if (nrow(SelectivityLengthF_fore_matrix) > 0) {
   reload_SelectivityLengthF_fore()
} else {
   reload_EMPTY_SelectivityLengthF_fore()
}
} else {
  reload_EMPTY_SelectivityLengthF_fore()
}


SelectivityLengthM_fore_matrix <<- object@SelectivityLength.M.vector

if (!is.null(SelectivityLengthM_fore_matrix) ) {
if (nrow(SelectivityLengthM_fore_matrix) > 0) {
   reload_SelectivityLengthM_fore()
} else {
   reload_EMPTY_SelectivityLengthM_fore()
}
} else {
  reload_EMPTY_SelectivityLengthM_fore()
}

} #else {
#
#  reload_EMPTY_SelectivityLengthF_fore()
#  reload_EMPTY_SelectivityLengthM_fore()
#}



}
}


fleet.discard_extvector_F_fore <<- NULL
  fleet.discard_extvector_M_fore <<- NULL
  fleet.discard_fore <<- NULL
  gtkComboBoxSetActive(combo_discard_fore, (which(DISCARD_CALC == object@discard.calculation)-1) )
  
if (object@discard.calculation == "YES") {
# DISCARD revense ogive
if (object@discard.datatype == "External vector") {
  gtkToggleButtonSetActive(radio_discard_vector_fore, TRUE)
fleet.discard_extvector_F_fore <<- object@discard_extvector.F.vector
fleet.discard_extvector_M_fore <<- object@discard_extvector.M.vector
reload_discard_extvector_M_fore()
reload_discard_extvector_F_fore()
reload_EMPTY_discard_fore()
} else {
reload_EMPTY_discard_extvector_M_fore()
reload_EMPTY_discard_extvector_F_fore()
  gtkToggleButtonSetActive(radio_discard_revogive_fore, TRUE)
fleet.discard_fore <<- object@discard.vector
#fleet.discard_fore[,3] <- as.numeric(as.character( fleet.discard_fore[,3]))
#fleet.discard_fore[,4] <- as.numeric(as.character( fleet.discard_fore[,4]))
reload_discard_fore()

}
} else {
reload_EMPTY_discard_extvector_M_fore()
reload_EMPTY_discard_extvector_F_fore()
reload_EMPTY_discard_fore()
}



  gtkEntrySetText(entry_survivability_param1_males_fore, "")
      gtkEntrySetText(entry_survivability_param2_females_fore, "")

if (object@discard.survivability.calculation == "Y") {
     gtkComboBoxSetActive(combo_discard_survival_rate, 0 )
     
  if (object@discard.survivability.datatype == "C" ) {
        gtkToggleButtonSetActive(radio_survivability_constant_fore, TRUE)      
  } else {
        gtkToggleButtonSetActive(radio_survivability_ogive_fore, TRUE)  
  }
   gtkEntrySetText(entry_survivability_param1_males_fore, as.numeric(as.character(object@discard.survivability.params[1,1])))
      gtkEntrySetText(entry_survivability_param2_females_fore, as.numeric(as.character(object@discard.survivability.params[1,2])))
  
  } else {
     gtkComboBoxSetActive(combo_discard_survival_rate_fore, 1 )
  }



  escape_surv_extvector_Mtable_fore <<- NULL
      escape_surv_extvector_Ftable_fore <<- NULL
      gtkEntrySetText(entry_escape_survivability_males_fore, "")
      gtkEntrySetText(entry_escape_survivability_females_fore, "")    
      gtkEntrySetText(entry_survivability_ogive_param1_fore, "")    
      gtkEntrySetText(entry_survivability_ogive_param2_fore, "") 
          
if (object@escape.survivability.calculation == "Y") { 

     gtkComboBoxSetActive(combo_escape_survival_rate_fore, 0 )
     
  if (object@escape.survivability.datatype == "C" ) {
        gtkToggleButtonSetActive(radio_escape_survivability_constant_fore, TRUE)
        
       gtkEntrySetText(entry_escape_survivability_males_fore, as.numeric(as.character(object@escape.survivability.constant[1,1])))
      gtkEntrySetText(entry_escape_survivability_females_fore, as.numeric(as.character(object@escape.survivability.constant[1,2])))    
              
  } else {
        gtkToggleButtonSetActive(radio_escape_survivability_size_fore, TRUE)  
        
        if (object@escape.survivability.DOS.datatype == "O") {
                gtkToggleButtonSetActive(radio_escape_survivability_size_O_fore, TRUE)
        
             gtkEntrySetText(entry_survivability_ogive_param1_fore, as.numeric(as.character(object@escape.survivability.DOS.ogiveparams[1,1])))    
             gtkEntrySetText(entry_survivability_ogive_param2_fore, as.numeric(as.character(object@escape.survivability.DOS.ogiveparams[1,2])))  
              reload_EMPTY_escape_survival_extvector_F_fore()
              reload_EMPTY_escape_survival_extvector_M_fore()
                 
        } else {
             gtkToggleButtonSetActive(radio_escape_survivability_size_EV_fore, TRUE)

              escape_surv_extvector_Ftable_fore <<- object@escape.survivability.DOS.ext_vect.F     
              reload_escape_survival_extvector_F_fore() 
              escape_surv_extvector_Mtable_fore <<- object@escape.survivability.DOS.ext_vect.M 
              reload_escape_survival_extvector_M_fore() 
        }   
  }  
  } else {
     gtkComboBoxSetActive(combo_escape_survival_rate_fore, 1 )
       reload_EMPTY_escape_survival_extvector_F_fore()
       reload_EMPTY_escape_survival_extvector_M_fore()
  } 




fleet.VESSELS_fore <<- object@vessels.vector
if (!is.null(fleet.VESSELS_fore)) {
if (nrow(fleet.VESSELS_fore) == 0) {
reload_EMPTY_VESSELS_fore_table() 
} else {
reload_VESSELS_fore_table() 
}
} else {
reload_EMPTY_VESSELS_fore_table() 
}
#source(paste(ALADYM_home, "/gui/forecast/fishery.vesselsTable_fore.r", sep=""))

fleet.DAYS_fore <<- object@days.vector 
if (!is.null(fleet.DAYS_fore)) {
if (nrow(fleet.DAYS_fore) == 0) {
reload_EMPTY_DAYS_fore_table() 
} else {
reload_DAYS_fore_table() 
}
} else {
reload_EMPTY_DAYS_fore_table() 
}

#source(paste(ALADYM_home, "/gui/forecast/fishery.daysTable_fore.r", sep=""))

fleet.GT_fore <<- object@gt.vector

if (!is.null(fleet.GT_fore)) {
if (nrow(fleet.GT_fore) == 0) {
reload_EMPTY_GT_fore_table() 
} else {
reload_GT_fore_table() 
}
} else {
reload_EMPTY_GT_fore_table() 
}

#source(paste(ALADYM_home, "/gui/forecast/fishery.gtTable_fore.r", sep=""))

fleet.FISHINGEFFORT_fore <<- object@fishingeffort.vector

if (!is.null(fleet.FISHINGEFFORT_fore)) {
if (nrow(fleet.FISHINGEFFORT_fore) == 0) {
reload_EMPTY_FISHINGEFFORT_fore_table() 
} else {
reload_FISHINGEFFORT_fore_table() 
}
} else {
reload_EMPTY_FISHINGEFFORT_fore_table() 
}

#source(paste(ALADYM_home, "/gui/forecast/fishery.fishingeffortTable_fore.r", sep=""))

fleet.lan_obligation <<- NULL
if (object@discard.calculation == "YES") {
fleet.lan_obligation_fore <<- object@landing.obligation.vector
} else {
fleet.lan_obligation_fore <<- NULL
}

if (!is.null(fleet.lan_obligation_fore)) {
if (nrow(fleet.lan_obligation_fore) == 0) {
reload_EMPTY_lan_obligation_fore_table() 
} else {
reload_lan_obligation_fore_table() 
} 
} else {
reload_EMPTY_lan_obligation_fore_table() 
}

print("Fleet segment for forecast loaded in GUI!", quote=F)
deactivate_FishingEffort_unused_params_fore()

gtkComboBoxSetActive(combo_Scenariotype, which(SCENARIO_TYPE == object@scenario.reduction)-1)

#print("dopo il caricamento dell'interfaccia (FUTURO)")
#try(  print(FleetList_simulation[[1]]@fishingmortality.M.vector)  )

if (IN_BEMTOOL) {
  lockFisheryValues_fore()
}
   deactivate_Discard_unused_params_fore()
   deactivate_Survivability_unused_params_fore()  
   deactive_survivability_O_C_fore()
   deactivate_escape_Survivability_unused_params_fore()
   deactive_survivability_C_S_fore()
   deactive_survivability_O_EV_fore()
   deactivate_activate_Selectivity_input_table_fore()
 }
 