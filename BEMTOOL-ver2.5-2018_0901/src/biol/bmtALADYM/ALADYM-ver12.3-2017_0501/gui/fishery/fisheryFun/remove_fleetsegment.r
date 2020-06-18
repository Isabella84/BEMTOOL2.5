# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





remove_fleetsegment <- function(w) {
gtkWidgetSetSensitive(main_window, FALSE)
print(".......................................... [fisheryFun.r] --> remove_fleetsegment()", quote=F)
 index_to_remove = -1
  selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
  wnd <- showMessage(paste("        Removing ", selected, "...        ", sep=""))


index_to_remove <- which(FLEETSEGMENTS_names == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
print(paste("Fleetsegment to be removed: ", selected, "[",index_to_remove,"]", sep=""), quote=F)

      FLEETSEGMENTS_names <<- FLEETSEGMENTS_names[-index_to_remove]
      FleetList_simulation <<- FleetList_simulation[-index_to_remove]
      FleetList_forecast <<- FleetList_simulation
# loadFleetsegmentintoGUI(FleetList_simulation[[1]])      

gtkComboBoxRemoveText(combo_fleetsegments, (index_to_remove-1))
gtkComboBoxRemoveText(combo_fleetsegments_fore, (index_to_remove-1))

if (length(FLEETSEGMENTS_names) >0) {
gtkComboBoxSetActive(combo_fleetsegments, 0 )
} else {
clear_FisheryGUI()
}
 
wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK(paste("        ", selected, "removed!        "))
}
