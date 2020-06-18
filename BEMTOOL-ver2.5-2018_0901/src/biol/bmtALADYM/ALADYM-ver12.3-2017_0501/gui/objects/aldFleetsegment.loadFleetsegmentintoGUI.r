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
# Function that load parameters of a fleet segment selected in the combobox (SIMULATION) taking all the parameters from the R object
#
loadFleetsegmentintoGUI<-function(object) {

gtkWidgetSetSensitive(main_window, FALSE)                  # -------------------------------
wnd <- showMessage(paste("        Loading",object@fleetname,"...        "))   #  --------------------------------------



#print("prima di caricare l'interfaccia")
#try(  print(FleetList_simulation[[1]]@fishingmortality.M.vector)  )

# object = FleetList_simulation[[1]]
 gtkEntrySetText(entryGearName, object@fleetname)

if (length(new_aldSimulation@enteringMortality) != 0) {
if (new_aldSimulation@enteringMortality == "Z") {

 if (object@selectivity.mode == "params") {
  gtkToggleButtonSetActive(radio_selectivity_params, TRUE)
 
fleet.selectivity <<- object@selectivity.vector

if (!is.null(fleet.selectivity) ) {
if (nrow(fleet.selectivity) > 0) {
   reload_selectivity()
} else {
   reload_EMPTY_selectivity()
}
} else {
  reload_EMPTY_selectivity()
}
} else {
   reload_EMPTY_selectivity()
}
#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.selectivityTable.r", sep=""))  )  


 if (object@selectivity.mode == "age") {
 
  gtkToggleButtonSetActive(radio_selectivity_vector_age, TRUE)  
SelectivityAgeF_matrix <<- object@SelectivityAge.F.vector

if (!is.null(SelectivityAgeF_matrix) ) {
if (nrow(SelectivityAgeF_matrix) > 0) {
   reload_SelectivityAgeF()
} else {
   reload_EMPTY_SelectivityAgeF()
}
} else {
  reload_EMPTY_SelectivityAgeF()
}


SelectivityAgeM_matrix <<- object@SelectivityAge.M.vector

if (!is.null(SelectivityAgeM_matrix) ) {
if (nrow(SelectivityAgeM_matrix) > 0) {
   reload_SelectivityAgeM()
} else {
   reload_EMPTY_SelectivityAgeM()
}
} else {
  reload_EMPTY_SelectivityAgeM()
}

} else {

  reload_EMPTY_SelectivityAgeF()
  reload_EMPTY_SelectivityAgeM()
}



 if (object@selectivity.mode == "length") {
 
  gtkToggleButtonSetActive(radio_selectivity_vector_length, TRUE)  
SelectivityLengthF_matrix <<- object@SelectivityLength.F.vector

if (!is.null(SelectivityLengthF_matrix) ) {
if (nrow(SelectivityLengthF_matrix) > 0) {
   reload_SelectivityLengthF()
} else {
   reload_EMPTY_SelectivityLengthF()
}
} else {
  reload_EMPTY_SelectivityLengthF()
}


SelectivityLengthM_matrix <<- object@SelectivityLength.M.vector

if (!is.null(SelectivityLengthM_matrix) ) {
if (nrow(SelectivityLengthM_matrix) > 0) {
   reload_SelectivityLengthM()
} else {
   reload_EMPTY_SelectivityLengthM()
}
} else {
  reload_EMPTY_SelectivityLengthM()
}

} #else {
#
#  reload_EMPTY_SelectivityLengthF()
#  reload_EMPTY_SelectivityLengthM()
#}



} else {
  reload_EMPTY_SelectivityLengthM()
  reload_EMPTY_SelectivityLengthF()
  reload_EMPTY_SelectivityAgeF()
  reload_EMPTY_SelectivityAgeM()
  reload_EMPTY_selectivity()

}
}

# PRODUCTION

fleet.production.seed <- object@production.vector$seed[1] 
fleet.production <<- object@production.vector[,colnames(object@production.vector) != "seed"] 
gtkEntrySetText(entry_Production_seedvalue, fleet.production.seed)

if (!is.null(fleet.production)) {
if (nrow(fleet.production) == 0) {
reload_EMPTY_production_table() 
} else {
reload_production_table() 
}
} else {
reload_EMPTY_production_table() 
}


fleet.monthlyDiscard.seed <- object@monthly.discard.vector$seed[1] 
fleet.monthlyDiscard <<- object@monthly.discard.vector[,colnames(object@monthly.discard.vector) != "seed"] 
gtkEntrySetText(entry_monthlyDiscard_seedvalue, fleet.monthlyDiscard.seed)

if (!is.null(fleet.monthlyDiscard)) {
if (nrow(fleet.monthlyDiscard) == 0) {
reload_EMPTY_monthlyDiscard_table() 
} else {
reload_monthlyDiscard_table() 
}
} else {
reload_EMPTY_monthlyDiscard_table() 
}



# P PRODUCTION

fleet.pproduction.seed <- object@pproduction.vector$seed[1] 
fleet.pproduction <<- object@pproduction.vector[,colnames(object@pproduction.vector) != "seed"] 
gtkEntrySetText(entry_pProduction_seedvalue, fleet.pproduction.seed)

if (!is.null(fleet.pproduction)) {
if (nrow(fleet.pproduction) == 0) {
reload_EMPTY_pproduction_table() 
} else {
reload_pproduction_table() 
}
} else {
reload_EMPTY_pproduction_table() 
}

#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.pproductionTable.r", sep="")) )

if ( object@production.datatype  == "Production data" ) {
  gtkToggleButtonSetActive(radio_data, TRUE)
} else if ( object@production.datatype == "P production" ) {
  gtkToggleButtonSetActive(radio_pp, TRUE)
}

# VESSELS
fleet.VESSELS.seed <- object@vessels.vector$seed[1] 
fleet.VESSELS <<- object@vessels.vector[,colnames(object@vessels.vector) != "seed"] 
gtkEntrySetText(entry_VESSELS_seedvalue, fleet.VESSELS.seed)

if (nrow(fleet.VESSELS) == 0) {
reload_EMPTY_VESSELS_table() 
} else {
reload_VESSELS_table() 
}

#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.vesselsTable.r", sep=""))  )

# DAYS

fleet.DAYS.seed <- object@days.vector$seed[1] 
fleet.DAYS <<- object@days.vector[,colnames(object@days.vector) != "seed"] 
gtkEntrySetText(entry_DAYS_seedvalue, fleet.DAYS.seed)

if (nrow(fleet.DAYS) == 0) {
reload_EMPTY_DAYS_table() 
} else {
reload_DAYS_table() 
}

#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.daysTable.r", sep="")) )

# GT

fleet.GT.seed <- object@gt.vector$seed[1] 
fleet.GT <<- object@gt.vector[,colnames(object@gt.vector) != "seed"] 
gtkEntrySetText(entry_GT_seedvalue, fleet.GT.seed)

if (nrow(fleet.GT) == 0) {
reload_EMPTY_GT_table() 
} else {
reload_GT_table() 
}

#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.gtTable.r", sep="")) )

# FISHINGEFFORT

fleet.FISHINGEFFORT.seed <- object@fishingeffort.vector$seed[1] 
fleet.FISHINGEFFORT <<- object@fishingeffort.vector[,colnames(object@fishingeffort.vector) != "seed"] 
gtkEntrySetText(entry_FISHINGEFFORT_seedvalue, fleet.FISHINGEFFORT.seed)

if (nrow(fleet.FISHINGEFFORT) == 0) {
reload_EMPTY_FISHINGEFFORT_table() 
} else {
reload_FISHINGEFFORT_table() 
}

#suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.effort.fishingeffortTable.r", sep="")) )


if ( object@effort.datatype  == "Effort data" ) {
  gtkToggleButtonSetActive(radio_effortdata, TRUE)
} else if ( object@effort.datatype == "Fishing coefficient" ) {
  gtkToggleButtonSetActive(radio_fishingcoeff, TRUE)
}


#fleet.lan_obligation.seed <- object@landing.obligation.vector[1,2] 
#gtkEntrySetText(entry_lan_obligation_seedvalue, fleet.lan_obligation.seed)


  fleet.lan_obligation <<- NULL
if (object@discard.calculation == "YES") {
fleet.lan_obligation <<- object@landing.obligation.vector
}

if (is.null(fleet.lan_obligation)) {
reload_EMPTY_lan_obligation_table() 
} else {
reload_lan_obligation_table() 
}

#suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.landingobligationTable.r", sep="")) )


if (length(new_aldSimulation@enteringMortality) != 0) {
if (new_aldSimulation@enteringMortality == "F") {
# fishingmortalityM
mortality.Fvector.males <<- object@fishingmortality.M.vector  
#suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableM.r", sep="")) )

reload_fishingmortalityM()
# fishingmortalityF
mortality.Fvector.females <<- object@fishingmortality.F.vector
#suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableF.r", sep="")) )
reload_fishingmortalityF()

 if ( new_aldSimulation@Ftype == "F") {
    gtkToggleButtonSetActive(radio_f_by_fleet, TRUE)
    
 }  else {
    gtkToggleButtonSetActive(radio_f_overall, TRUE)
    
    if (new_aldSimulation@Fsplittingtype == "CAA") {
          gtkToggleButtonSetActive(radio_catch_by_age_splitting, TRUE)
         catchAtAge.vector.males  <<- object@catchAtAge.M.vector  
          reload_catchAtAgeM()
          catchAtAge.vector.females  <<- object@catchAtAge.F.vector  
          reload_catchAtAgeF()
    } else {
          gtkToggleButtonSetActive(radio_production_splitting, TRUE)
             reload_EMPTY_catchAtAgeM()
                reload_EMPTY_catchAtAgeF()
          
    }
 }


}
} else {
    reload_EMPTY_fishingmortalityF()
    reload_EMPTY_fishingmortalityM()
}

  fleet.discard_extvector_F <<- NULL
  fleet.discard_extvector_M <<- NULL
  fleet.discard <<- NULL
  gtkComboBoxSetActive(combo_discard, (which(DISCARD_CALC == object@discard.calculation)-1) )
  
if (object@discard.calculation == "YES") {
# DISCARD revense ogive

if (object@discard.datatype == "External vector") {
  gtkToggleButtonSetActive(radio_discard_vector, TRUE)
fleet.discard_extvector_F <<- object@discard_extvector.F.vector
fleet.discard_extvector_M <<- object@discard_extvector.M.vector
reload_discard_extvector_M()
reload_discard_extvector_F()
reload_EMPTY_discard_table()

} else {
reload_EMPTY_discard_extvector_M()
reload_EMPTY_discard_extvector_F()

gtkToggleButtonSetActive(radio_discard_revogive, TRUE)
fleet.discard <<- object@discard.vector
fleet.discard[,3] <- as.numeric(as.character( fleet.discard[,3]))
fleet.discard[,4] <- as.numeric(as.character( fleet.discard[,4]))
reload_discard_table()

}
} else {
reload_EMPTY_discard_extvector_M()
reload_EMPTY_discard_extvector_F()
reload_EMPTY_discard_table()
}

# suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.discard_extvectorTableF.r", sep="")) )
# suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.discard_extvectorTableM.r", sep="")) )
# suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.discardTable.r", sep="")) )

    gtkEntrySetText(entry_survivability_param1_males, "")
      gtkEntrySetText(entry_survivability_param2_females, "")

if (object@discard.survivability.calculation == "Y") {
     gtkComboBoxSetActive(combo_discard_survival_rate, 0 )
     
  if (object@discard.survivability.datatype == "C" ) {
        gtkToggleButtonSetActive(radio_survivability_constant, TRUE)      
  } else {
        gtkToggleButtonSetActive(radio_survivability_ogive, TRUE)  
  }
   gtkEntrySetText(entry_survivability_param1_males, as.numeric(as.character(object@discard.survivability.params[1,1])))
      gtkEntrySetText(entry_survivability_param2_females, as.numeric(as.character(object@discard.survivability.params[1,2])))
  
  } else {
     gtkComboBoxSetActive(combo_discard_survival_rate, 1 )
  }
   
   
   
      escape_surv_extvector_Mtable <<- NULL
      escape_surv_extvector_Ftable <<- NULL
      gtkEntrySetText(entry_escape_survivability_males, "")
      gtkEntrySetText(entry_escape_survivability_females, "")    
      gtkEntrySetText(entry_survivability_ogive_param1, "")    
      gtkEntrySetText(entry_survivability_ogive_param2, "") 
   
          
if (object@escape.survivability.calculation == "Y") { 

     gtkComboBoxSetActive(combo_escape_survival_rate, 0 )
     
  if (object@escape.survivability.datatype == "C" ) {
        gtkToggleButtonSetActive(radio_escape_survivability_constant, TRUE)
        
       gtkEntrySetText(entry_escape_survivability_males, as.numeric(as.character(object@escape.survivability.constant[1,1])))
      gtkEntrySetText(entry_escape_survivability_females, as.numeric(as.character(object@escape.survivability.constant[1,2]))) 
      
      reload_EMPTY_escape_survival_extvectorF() 
      reload_EMPTY_escape_survival_extvectorM()   
              
  } else {
        gtkToggleButtonSetActive(radio_escape_survivability_size, TRUE)  
        
        if (object@escape.survivability.DOS.datatype == "O") {
                gtkToggleButtonSetActive(radio_escape_survivability_size_O, TRUE)
        
             gtkEntrySetText(entry_survivability_ogive_param1, as.numeric(as.character(object@escape.survivability.DOS.ogiveparams[1,1])))    
             gtkEntrySetText(entry_survivability_ogive_param2, as.numeric(as.character(object@escape.survivability.DOS.ogiveparams[1,2])))  
        reload_EMPTY_escape_survival_extvectorF() 
        reload_EMPTY_escape_survival_extvectorM()   
                 
        } else {
             gtkToggleButtonSetActive(radio_escape_survivability_size_EV, TRUE)

              escape_surv_extvector_Ftable <<- object@escape.survivability.DOS.ext_vect.F     
              reload_escape_survival_extvectorF() 
              escape_surv_extvector_Mtable <<- object@escape.survivability.DOS.ext_vect.M 
              reload_escape_survival_extvectorM() 
        }   
  }  
  } else {
     gtkComboBoxSetActive(combo_escape_survival_rate, 1 )
       reload_EMPTY_escape_survival_extvectorF()
       reload_EMPTY_escape_survival_extvectorM()
  } 
  
  deactivate_escape_Survivability_unused_params()
  deactive_survivability_C_S()
  deactive_survivability_O_EV()
  
                
#        suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.escape_survival_extvectorTableF.r", sep="")) )
#        suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.escape_survival_extvectorTableM.r", sep="")) )
        
#     mortality.Fvector.males <<- NULL
#     mortality.Fvector.females <<- NULL
#      
#if (new_aldSimulation@enteringMortality == "F") {
#mortality.Fvector.males <<- object@fishingmortality.M.vector  
#mortality.Fvector.females <<- object@fishingmortality.F.vector
#}
#
#suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableM.r", sep="")) )
#suppressWarnings( source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableF.r", sep="")) )

print("Fleet segment loaded in GUI!", quote=F)

if (!IN_BEMTOOL) {
   deactivate_Discard_unused_params()
   deactivate_Pproduction_unused_params()
   deactivate_FishingM_unused_params()
   deactivate_FishingEffort_unused_params()
   deactivate_Survivability_unused_params()  
   deactive_survivability_O_C()
   deactivate_escape_Survivability_unused_params()
   deactive_survivability_C_S()
   deactive_survivability_O_EV()
   deactivate_activate_Selectivity_input_table()
} else {
   lockFisheryValues()
   deactivate_FishingM_unused_params()
   deactivate_Discard_unused_params()
   deactivate_Survivability_unused_params()  
   deactive_survivability_O_C()
   deactivate_escape_Survivability_unused_params()
   deactive_survivability_C_S()
   deactive_survivability_O_EV()
   deactivate_activate_Selectivity_input_table()
}

#print("dopo il caricamento dell'interfaccia")
#try(  print(FleetList_simulation[[1]]@fishingmortality.M.vector)  )

wnd$destroy()                                        
gtkWidgetSetSensitive(main_window, TRUE)                

#showMessageOK("        Fleet segment loaded!        ")

}
