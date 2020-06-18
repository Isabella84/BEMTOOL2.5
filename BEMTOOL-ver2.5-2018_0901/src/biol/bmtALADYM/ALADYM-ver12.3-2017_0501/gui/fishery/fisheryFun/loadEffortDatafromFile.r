# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadEffortDatafromFile<-function(w) {

if (length(FLEETSEGMENTS_names) == 0) {
   showError("At least one fleet has to be defined!")
} else {

dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {


loaded_effortvariables <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_effortvariables <- read.csv( "C:\\FACCHINI_MT\\Aladym_ r_9.2\\ALADYM - EFFORT DATA.csv", sep=";", na.strings = "")  

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading effort data...        ")


for (fs in 1:length(FLEETSEGMENTS_names) ) {
  data_fs <- loaded_effortvariables[as.character(loaded_effortvariables$fleet_segment) == FLEETSEGMENTS_names[fs],]

vesselsMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
vess <- data_fs$VESSELS[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    vesselsMatr <- rbind(vesselsMatr, c(years[yea], data_fs$VESSELS[1], vess))  
} else {
    vesselsMatr <- rbind(vesselsMatr, c(years[yea],"", vess))  
}
}
colnames(vesselsMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@vessels.vector <<- vesselsMatr 


daysMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
dayss <- data_fs$DAYS[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    daysMatr <- rbind(daysMatr, c(years[yea], data_fs$DAYS[1], dayss))  
} else {
    daysMatr <- rbind(daysMatr, c(years[yea],"", dayss))  
}
}
colnames(daysMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@days.vector <<- daysMatr 


gtMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
gts <- data_fs$GT[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    gtMatr <- rbind(gtMatr, c(years[yea], data_fs$GT[1], gts))  
} else {
    gtMatr <- rbind(gtMatr, c(years[yea],"", gts))  
}
}
colnames(gtMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@gt.vector <<- gtMatr 

}

eff_data <- get_effort_data()
fact_mat <- fact_calc.gui(eff_data,"N", years, (length(years)*12+1))    
#for (i in 1:forecast){
#        for (gear in 1:length(FLEETSEGMENTS_names) ) {
#        INP$Fishing_efforts[i,gear] = fact_mat[i,gear]
#        }
#    }
    
for (fs in 1:length(FLEETSEGMENTS_names) ) {
  
fishcoeff_fs <- as.numeric(as.character(fact_mat[,fs] ))

fishcoeffMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
fico <- fishcoeff_fs[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea], fishcoeff_fs[1], fico))  
} else {
    fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea],"", fico))  
}
}
colnames(fishcoeffMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@fishingeffort.vector <<- fishcoeffMatr 

}    

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])  

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Effort data loaded!        ")
} 

}
}
