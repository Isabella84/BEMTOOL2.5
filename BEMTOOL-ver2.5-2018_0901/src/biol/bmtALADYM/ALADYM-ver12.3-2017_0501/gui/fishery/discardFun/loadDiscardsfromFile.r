# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadDiscardsfromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_discard <- read.csv(dialog$getFilename(), sep=";", na.strings = "") 

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading discard...        ") 
# loaded_discard <- read.csv( "C:\\discard PAPE LON.csv", sep=";", na.strings = "")  

for (fs in 1:length(FLEETSEGMENTS_names) ) {
   discard_fs <- loaded_discard[as.character(loaded_discard$fleet_segment) == FLEETSEGMENTS_names[fs],]
   discard_fs[1,1] <- ""

fleet.discard <<- discard_fs[, 1:4]

FleetList_simulation[[fs]]@discard.vector <<- fleet.discard 
FleetList_simulation[[fs]]@discard.calculation <<- as.character(discard_fs$discard[1])
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Discard loaded!        ")
} 
}
