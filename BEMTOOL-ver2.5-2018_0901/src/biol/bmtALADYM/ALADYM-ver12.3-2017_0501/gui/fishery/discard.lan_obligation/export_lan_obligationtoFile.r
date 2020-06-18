# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




export_lan_obligationtoFile<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

# save_selectivity_path = "C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver_pre-beta\\saved_selectivity.csv"
save_lanObl_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving landing obligation...        ")

all_lanObl <- data.frame(matrix(nrow=0, ncol=4 ))
colnames(all_lanObl) <- c("year",	"month",	"landing_obligation",	"fleet_segment") 

for (fs in 1:length(FLEETSEGMENTS_names) ) {

lanObl_table <- data.frame(matrix(nrow=(length(years)*12), ncol=4 ))
heading <-  c("year",	"month",	"landing_obligation",	"fleet_segment") 
colnames(lanObl_table) <- heading

 lanObl_table$fleet_segment <-  FLEETSEGMENTS_names[fs]

 years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years))
   lanObl_table$year <- years_rep
   lanObl_table$month <- months_rep

   landObl_fs <- data.frame(FleetList_simulation[[fs]]@landing.obligation.vector)

   if (nrow(landObl_fs) > 0) {
for (yea in years) {
    lanObl_table$landing_obligation[lanObl_table$year == yea]  <- as.character(landObl_fs[landObl_fs$year == yea, 2:13])
} 
} else {
    lanObl_table$landing_obligation <- "N"
}

all_lanObl <- data.frame(rbind(all_lanObl, lanObl_table) )  
}

write.table(all_lanObl, file=save_lanObl_path, sep=";", row.names=FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Landing obligation saved!        ")
} 
}
