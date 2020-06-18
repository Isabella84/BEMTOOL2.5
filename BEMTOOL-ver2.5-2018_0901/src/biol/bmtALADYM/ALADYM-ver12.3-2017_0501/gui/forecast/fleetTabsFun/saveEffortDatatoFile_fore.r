# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveEffortDatatoFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

# save_selectivity_path = "C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver_pre-beta\\saved_selectivity.csv"
save_ed_path <- dialog$getFilename()

all_ed <- data.frame(matrix(nrow=0, ncol=6 ))
colnames(all_ed) <- c("year",	"month",	"VESSELS",	"DAYS",	"GT",	"fleet_segment") 

eff_data_fore <- get_effort_data_fore()

for (fs in 1:length(FLEETSEGMENTS_names) ) {

fs_object <- FleetList_forecast[[fs]]

ed_table <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=6 ))
heading <-  c("year",	"month",	"VESSELS",	"DAYS",	"GT",	"fleet_segment") 
colnames(ed_table) <- heading

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   #years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years_forecast))
   #months_rep <- c("seed", months_rep)
   ed_table$year <- years_rep
   ed_table$month <- months_rep

all_ed <- rbind(all_ed, ed_table)   
}

all_ed$VESSELS <-  eff_data_fore$Vessels[eff_data_fore$Month >= 2+length(years)*12]# [(2+length(years)*12):(1+length(c(years,years_forecast))*12)]
all_ed$DAYS <-  eff_data_fore$Days[eff_data_fore$Month >= 2+length(years)*12]# [(2+length(years)*12):(1+length(c(years,years_forecast))*12)]
all_ed$GT <-  eff_data_fore$GT[eff_data_fore$Month >= 2+length(years)*12] # [(2+length(years)*12):(1+length(c(years,years_forecast))*12)]

fs_names <- c()
for (fs in 1:length(FLEETSEGMENTS_names)) {
     fs_names <- c(fs_names, rep(FLEETSEGMENTS_names[fs], (length(years_forecast)*12)) )     
}

all_ed$fleet_segment <-  fs_names


#to_add <- data.frame(matrix(NA, nrow=nrow(selectivity_table), ncol=(5-selectivity_params$n_par)))
#selectivity_table <- cbind(selectivity_table, to_add)
#selectivity_table <- cbind(selectivity_table, rep(sel_type, nrow(selectivity_table)))
#selectivity_table <- cbind(selectivity_table, rep(FLEETSEGMENTS_names[fs], nrow(selectivity_table)))
#colnames(selectivity_table) <- c("year", "month", "param1", "param2", "param3", "param4", "param5", "sel_type", "fleet_segment") 
#all_selectivities <- rbind(all_selectivities, selectivity_table)
#}
write.table(all_ed, file=save_ed_path, sep=";", row.names=FALSE)

dialog$destroy()
} else {
dialog$destroy()
}
}
