# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadmonthlyDiscardfromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_monthlyDiscard <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_production <- read.csv( "C:\\FACCHINI_MT\\Aladym_ r_9.2\\ALADYM - EFFORT DATA.csv", sep=";", na.strings = "")  

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading monthly discard...        ")

for (fs in 1:length(FLEETSEGMENTS_names) ) {
  monthlyDiscard_fs <- loaded_monthlyDiscard[as.character(loaded_monthlyDiscard$fleet_segment) == FLEETSEGMENTS_names[fs],]

monthlyDiscardMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
monthlyD <- monthlyDiscard_fs$DISCARD[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    monthlyDiscardMatr <- rbind(monthlyDiscardMatr, c(years[yea], monthlyDiscard_fs$DISCARD[1], monthlyD))  
} else {
    monthlyDiscardMatr <- rbind(monthlyDiscardMatr, c(years[yea],"", monthlyD))  
}
}
colnames(monthlyDiscardMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@monthly.discard.vector <<- monthlyDiscardMatr 

}

prod_data <- get_production_data()
disc_data <- get_discard_data()
prod_data$Production <- prod_data$Production  +  disc_data$Discard
p_production_mat <- P_production_calc.gui(prod_data,(length(years)*12+1))
#    for (i in 1:forecast){
#    INP$p_Production[i,] = p_production_mat[i,]
#    }

for (fs in 1:length(FLEETSEGMENTS_names) ) {
  pp_fs <- as.numeric(as.character(p_production_mat[,fs]))

ppMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
pipro <- pp_fs[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    ppMatr <- rbind(ppMatr, c(years[yea], pp_fs[1], pipro))  
} else {
    ppMatr <- rbind(ppMatr, c(years[yea],"", as.numeric(as.character(pipro))) ) 
}
}
colnames(ppMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@pproduction.vector <<- ppMatr 

}


selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected)  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Monthly discard loaded!        ")
} 
}
