# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





loadDiscardsfromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_discards_fore <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_discards_fore <- read.csv( "C:\\BEMTOOL GSA18\\GSA18 input files\\FORECAST\\ALADYM Discard - PAPE LON_forecast.csv", sep=";", na.strings = "")  
 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading discard for forecast...        ") 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
   discards_fs <- loaded_discards_fore[as.character(loaded_discards_fore$fleet_segment) == FLEETSEGMENTS_names[fs],]
   # selectivity_fs[1,1] <- ""

fleet.discard_fore <<- discards_fs[,1:4]

FleetList_forecast[[fs]]@discard.vector <<- fleet.discard_fore
  
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected ) 
fleet.discard_fore <<- FleetList_forecast[[index_to_load]]@discard.vector 
reload_discard_fore() 
# loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Discard for forecast loaded!        ")
} 
}