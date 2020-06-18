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
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of stock-recruitment are 
# saved and the loading of those values in the table
# ------------------------------------------------------------------------------
#
load_lan_obligationfromFile <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_lanObl <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading landing obligation...        ")

loaded_lanObl$landing_obligation <- as.character(loaded_lanObl$landing_obligation)
if (nrow(loaded_lanObl[as.character(loaded_lanObl$landing_obligation) == "NA",]) >0 ) {
loaded_lanObl$landing_obligation[as.character(loaded_lanObl$landing_obligation) == "NA"] <- "N" 
}
 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
  loaded_lanObl_fs <- data.frame(loaded_lanObl[as.character(loaded_lanObl$fleet_segment) == FLEETSEGMENTS_names[fs],]) 

lanObl_matrix <- data.frame(matrix("N",nrow=length(years), ncol=(length(MONTHS)+1)))
colnames(lanObl_matrix) <- c("year",	MONTHS)
lanObl_matrix$year <- years

for (yea in 1:length(years) ) {
this_land <- as.character(loaded_lanObl_fs$landing_obligation[(1+(yea-1)*12):((yea)*12)])
# print(vess)
    lanObl_matrix[yea,2:13] <- this_land
}

FleetList_simulation[[fs]]@landing.obligation.vector <<- lanObl_matrix 
}


selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected)  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Landing obligation loaded!        ")
} 
} 
