# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadEffortDatafromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_effortvariables <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_effortvariables <- read.csv( "C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\input\\EFFORT DATA-forecast fishing ban.csv", sep=";", na.strings = "")  
 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading effort data for forecast...        ")

for (fs in 1:length(FLEETSEGMENTS_names) ) {
  data_fs <- loaded_effortvariables[as.character(loaded_effortvariables$fleet_segment) == FLEETSEGMENTS_names[fs],]

vesselsMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
vess <- data_fs$VESSELS[(1+(yea-1)*12):((yea)*12)]
# print(vess)
    vesselsMatr <- rbind(vesselsMatr, c(years_forecast[yea],vess))  
}
colnames(vesselsMatr) <- c("year", MONTHS)
FleetList_forecast[[fs]]@vessels.vector <<- vesselsMatr 


daysMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
dayss <- data_fs$DAYS[(1+(yea-1)*12):((yea)*12)]
# print(vess)
    daysMatr <- rbind(daysMatr, c(years_forecast[yea],dayss))  
}
colnames(daysMatr) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@days.vector <<- daysMatr 


gtMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
gts <- data_fs$GT[(1+(yea-1)*12):((yea)*12)]
# print(vess)
    gtMatr <- rbind(gtMatr, c(years_forecast[yea], gts))  

}
colnames(gtMatr) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@gt.vector <<- gtMatr 

}

eff_data_fore <- get_effort_data_fore()
#print(eff_data_fore)
fact_mat <- fact_calc.gui(eff_data_fore,"N", c(years), ( (length(years))*12+1))   # simulation 
#print("------------------------------------------------------ dopo chiamata presente")
#print(fact_mat)
   effF <- data.frame(matrix("", nrow=length(FLEETSEGMENTS_names), ncol=3), stringsAsFactors=F)
   colnames(effF) <- c("relationship_type", "a", "b")

   for (g in 1:length(FLEETSEGMENTS_names)) {
   effF$relationship_type[g] <- FleetList_forecast[[g]]@EffortF.relationship$relationship_type #INP$Forecast_reduction[num_ORD_fleetsegm]
      effF$a[g] <- as.numeric(as.character(FleetList_forecast[[g]]@EffortF.relationship$a ) )
        effF$b[g] <- as.numeric(as.character(FleetList_forecast[[g]]@EffortF.relationship$b) )
    }   
    
       GLO$L_number <-  ( (length(c(years, years_forecast)))*12)
       
       
eff_data_fore <- get_effort_data_fore()
# print(eff_data_fore)  
fact_mat_fore <- fact_calc.gui(eff_data_fore,"Y", c(years, years_forecast), ( (length(years))*12+1), effF)
#print("------------------------------------------------------ dopo chiamata futuro")
fact_mat_fore[c(1:((length(years))*12+1)), ] <- fact_mat 
#print(fact_mat)  
#for (i in 1:forecast){
#        for (gear in 1:length(FLEETSEGMENTS_names) ) {
#        INP$Fishing_efforts[i,gear] = fact_mat[i,gear]
#        }
#    }


for (fs in 1:length(FLEETSEGMENTS_names) ) {
  
fishcoeff_fs_fore <- as.numeric(as.character(fact_mat_fore[,fs] ))

fishcoeffMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
fico_fore <- fishcoeff_fs_fore[(2+((yea+length(years))-1)*12):((yea+length(years))*12+1)]

# print(vess)
#if (yea==1) {
#    fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea], fishcoeff_fs[1], fico))  
#} else {
    fishcoeffMatr_fore <- rbind(fishcoeffMatr_fore, c(years_forecast[yea], fico_fore))  
#}
}
colnames(fishcoeffMatr_fore) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@fishingeffort.vector <<- fishcoeffMatr_fore 

}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Effort data for forecast loaded!        ")
} 
}