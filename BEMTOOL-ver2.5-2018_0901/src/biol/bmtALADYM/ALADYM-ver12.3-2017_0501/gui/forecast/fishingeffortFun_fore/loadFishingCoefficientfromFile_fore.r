# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





loadFishingCoefficientfromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_fishcoeff <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_fishcoeff <- read.csv( "C:\\FACCHINI_MT\\Aladym_ r_9.2\\ALADYM - EFFORT DATA.csv", sep=";", na.strings = "")  

for (fs in 1:length(FLEETSEGMENTS_names) ) {
  fishcoeff_fs <- loaded_fishcoeff[as.character(loaded_fishcoeff$fleet_segment) == FLEETSEGMENTS_names[fs],]

fishcoeffMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
fico <- fishcoeff_fs$FISHING_COEFFICIENT[(1+(yea-1)*12):((yea)*12)]
# print(vess)

    fishcoeffMatr <- rbind(fishcoeffMatr, c(years_forecast[yea],fico))  
}
colnames(fishcoeffMatr) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@fishingeffort.vector <<- fishcoeffMatr 

}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])

dialog$destroy()
} else {
dialog$destroy()
}
}
