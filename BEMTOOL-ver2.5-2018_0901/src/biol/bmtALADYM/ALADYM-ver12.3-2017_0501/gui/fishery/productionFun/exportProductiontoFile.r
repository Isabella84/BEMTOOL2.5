# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




exportProductiontoFile<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

# save_selectivity_path = "C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver_pre-beta\\saved_selectivity.csv"
save_prodata_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving production...        ")

all_proddata <- data.frame(matrix(nrow=0, ncol=4 ))
colnames(all_proddata) <- c("year",	"month",	"PRODUCTION",	"fleet_segment") 

prod_data <- get_production_data()

for (fs in 1:length(FLEETSEGMENTS_names) ) {

fs_object <- FleetList_simulation[[fs]]

proddata_table <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=4 ))
heading <-  c("year",	"month",	"PRODUCTION",	"fleet_segment") 
colnames(proddata_table) <- heading

 years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   proddata_table$year <- years_rep
   proddata_table$month <- months_rep

all_proddata <- rbind(all_proddata, proddata_table)   
}

all_proddata$PRODUCTION <-  prod_data$Production[1:((1+length(years)*12)*length(FLEETSEGMENTS_names))]

all_proddata$fleet_segment <- prod_data$Gear[1:((1+length(years)*12)*length(FLEETSEGMENTS_names))]

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
wnd <- showMessageOK("        Production saved!        ")
} 
}
