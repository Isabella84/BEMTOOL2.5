# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveDiscardstoFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])

if (dialog$run() == GtkResponseType["accept"]) {
# save_discard_path = "C:\\saved_discard.csv"
save_discard_path <- dialog$getFilename()

all_discards <- data.frame(matrix(nrow=0, ncol=6 ))
colnames(all_discards) <- c("year", "month", "L50", "L75_L25", "discard", "fleet_segment") 

for (fs in 1:length(FLEETSEGMENTS_names) ) {

fs_object <- FleetList_forecast[[fs]]
discard_table <- data.frame(matrix(nrow=(length(years.forecast)*12), ncol=4))

heading <- c("year", "month", "L50", "L75_L25") 

colnames(discard_table) <- heading

 years_rep <- rep(years.forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years.forecast))
   discard_table$year <- years_rep
   discard_table$month <- months_rep
 
for (i in 1:nrow(discard_table)) {
  for (m in 1:((length(years.forecast)*12))) {            # ALADYM_GUI_fleets[[1]]$aldFleets[[1]]@discard.vector
      discard_table[m, 3] <- fs_object@discard.vector[m,3] 
      discard_table[m, 4] <- fs_object@discard.vector[m,4]     
  }
}

#to_add <- data.frame(matrix(NA, nrow=nrow(discard_table), ncol=5))
#discard_table <- cbind(discard_table, to_add)
discard_table <- cbind(discard_table, rep(fs_object@discard.calculation, nrow(discard_table))) 
discard_table <- cbind(discard_table, rep(FLEETSEGMENTS_names[fs], nrow(discard_table)))
colnames(discard_table) <- c("year", "month", "L50", "L75_L25", "discard", "fleet_segment") 
all_discards <- rbind(all_discards, discard_table)
}

write.table(all_discards, file=save_discard_path, sep=";", row.names=FALSE)
dialog$destroy()

} else {
dialog$destroy()
}
}
