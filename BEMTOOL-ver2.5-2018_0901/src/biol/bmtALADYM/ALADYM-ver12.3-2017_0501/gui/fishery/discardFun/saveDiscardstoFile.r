# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveDiscardstoFile<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

# save_discards_path = "C:\\prova_salvataggio_discards.csv"
save_discards_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving discard...        ")

all_discards <- data.frame(matrix(nrow=0, ncol=6 ))
colnames(all_discards) <- c("year", "month", "L50", "L75_L25", "discard", "fleet_segment") 

for (fs in 1:length(FLEETSEGMENTS_names) ) {

fs_object <- FleetList_simulation[[fs]]
   # selectivity_fs <- read.csv("C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver_pre-beta\\ALADYM selectivity - MULL BAR.csv", sep=";" )
#   selectivity_fs[1,1] <- ""

# sel_type <- which( SELECTIVITY_TYPE == fs_object@selectivity.method)

#if ( sel_type == 1 ) {
#  table_sel_params <- list(n_par = 2, param1 = "SL50%", param2 = "SL75%-SL25%")  
#} else if (sel_type == 2) {
#  table_sel_params <- list(n_par = 3, param1 = "SL50%", param2 = "SL75%-SL25%", param3 = "DL50%") 
#} else if (sel_type == 3) {
#  table_sel_params <- list(n_par = 2, param1 = "mean", param2 = "st.dev") 
#} else if (sel_type == 4) {
#  table_sel_params <- list(n_par = 2, param1 = "mean", param2 = "st.dev") 
#} else if (sel_type == 5) {
#table_sel_params <- list(n_par = 5, param1 = "mean1", param2 = "st.dev1", param3 = "mean2", param4 = "st.dev2", param5 = "b") 
#} else if (sel_type == 6) {
#  table_sel_params <- list(n_par = 3, param1 = "mean1", param2 = "st.dev1", param3 = "st.dev2") 
#}

discard_table <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=4))

   heading <- c("year", "month", "L50", "L75_L25") 

colnames(discard_table) <- heading

 years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   discard_table$year <- years_rep
   discard_table$month <- months_rep
 
 
for (i in 1:nrow(discard_table)) {
  for (m in 1:((length(years)*12)+1)) {            # ALADYM_GUI_fleets[[1]]$aldFleets[[1]]@discard.vector
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

write.table(all_discards, file=save_discards_path, sep=";", row.names=FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Discard saved!        ")

} 
}
