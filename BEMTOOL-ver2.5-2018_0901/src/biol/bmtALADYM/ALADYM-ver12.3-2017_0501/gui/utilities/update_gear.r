# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ---------------------- Create and add gear to the list
update_gear <- function(w) {
#print(".......................................... [update_gear.r] --> update_gear()")
# new_aldFleetsegment <<- new(Class="aldFleetsegment") 
# new_aldFleetsegment <<- setFleetsegmentfromGUI(new_aldFleetsegment)

gtkWidgetSetSensitive(main_window, FALSE)
index_to_update = -1
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))


#index_to_update = -1
#selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_update <- which(FLEETSEGMENTS_names == selected)    

print(paste("Fleetsegment simulation to be updated: ", selected, "[",index_to_update,"]", sep=""))
FleetList_simulation[[index_to_update]] <<- setFleetsegmentfromGUI(FleetList_simulation[[index_to_update]])

FleetList_forecast[[index_to_update]]@discard.calculation <<- FleetList_simulation[[index_to_update]]@discard.calculation 

gtkComboBoxRemoveText(combo_fleetsegments, index_to_update-1)   
gtkComboBoxRemoveText(combo_fleetsegments_fore, index_to_update-1)

string_fleet <- gtkEntryGetText(entryGearName)
m <- regexec("[[:alnum:]+[:punct:]+[:blank:]+]+", string_fleet)

gtkComboBoxInsertText(combo_fleetsegments, index_to_update-1, as.character(regmatches(string_fleet, m)))
gtkComboBoxInsertText(combo_fleetsegments_fore, index_to_update-1, as.character(regmatches(string_fleet, m)))

FLEETSEGMENTS_names[index_to_update] <<- as.character(regmatches(string_fleet, m))

gtkComboBoxSetActive(combo_fleetsegments, index_to_update-1 )
 
# FleetList_simulation <<- c(FleetList_simulation, new_aldFleetsegment)
# FleetList_forecast <<- FleetList_simulation
# return(new_aldFleetsegment@fleetname)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK(paste("        ", selected, "saved!        "))
}
