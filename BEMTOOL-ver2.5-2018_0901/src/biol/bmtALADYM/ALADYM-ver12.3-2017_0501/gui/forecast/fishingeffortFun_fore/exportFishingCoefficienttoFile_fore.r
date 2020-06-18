# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




exportFishingCoefficienttoFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

# save_selectivity_path = "C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\test_export_fish_coeff_fore.csv"
save_prodata_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}
if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving fishing coefficient for FORECAST...        ")

all_proddata <- data.frame(matrix(nrow=0, ncol=4))
colnames(all_proddata) <- c("year",	"month",	"FISHING_COEFFICIENT",	"fleet_segment") 

if (gtkToggleButtonGetActive(radio_effortdata)) {
eff_data_fore <- get_effort_data_fore()
fact_mat <- fact_calc.gui(eff_data_fore,"N", c(years), ( (length(years))*12+1))    
fact_mat_fore <- fact_calc.gui(eff_data_fore,"Y", c(years, years_forecast), ( (length(years))*12+1))
fact_mat_fore[c(1:((length(years))*12+1)), ] <- fact_mat  

for (fs in 1:length(FLEETSEGMENTS_names) ) {

proddata_table <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=4 ))
heading <-  c("year",	"month",	"FISHING_COEFFICIENT",	"fleet_segment") 
colnames(proddata_table) <- heading

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   #years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years_forecast))
   #months_rep <- c("seed", months_rep)
   proddata_table$year <- years_rep
   proddata_table$month <- months_rep
   proddata_table$FISHING_COEFFICIENT <- as.numeric(as.character(fact_mat_fore[c(((length(years))*12+2):nrow(fact_mat_fore)), fs]))
   proddata_table$fleet_segment <- FLEETSEGMENTS_names[fs]
all_proddata <- rbind(all_proddata, proddata_table)   
}

} else {


for (fs in 1:length(FLEETSEGMENTS_names) ) {

proddata_table <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=4 ))
heading <-  c("year",	"month",	"FISHING_COEFFICIENT",	"fleet_segment") 
colnames(proddata_table) <- heading

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   #years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years_forecast))
   #months_rep <- c("seed", months_rep)
   proddata_table$year <- years_rep
   proddata_table$month <- months_rep
   
   for (yef in 1:length(years_forecast) ) {
     proddata_table$FISHING_COEFFICIENT[( (yef-1)*12+1) : (yef*12)] <- as.numeric(as.character(FleetList_forecast[[fs]]@fishingeffort.vector[yef,2:13]))
   }
   
   proddata_table$fleet_segment <- FLEETSEGMENTS_names[fs]
all_proddata <- rbind(all_proddata, proddata_table)   
}

}

#all_proddata$P_PRODUCTION <-  p_production_mat$Production[1:(1+length(years)*12)]



#to_add <- data.frame(matrix(NA, nrow=nrow(selectivity_table), ncol=(5-selectivity_params$n_par)))
#selectivity_table <- cbind(selectivity_table, to_add)
#selectivity_table <- cbind(selectivity_table, rep(sel_type, nrow(selectivity_table)))
#selectivity_table <- cbind(selectivity_table, rep(FLEETSEGMENTS_names[fs], nrow(selectivity_table)))
#colnames(selectivity_table) <- c("year", "month", "param1", "param2", "param3", "param4", "param5", "sel_type", "fleet_segment") 
#all_selectivities <- rbind(all_selectivities, selectivity_table)
#}
write.table(all_proddata, file=save_prodata_path, sep=";", row.names=FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Fishing coefficient for FORECAST saved!        ")

} 
}
