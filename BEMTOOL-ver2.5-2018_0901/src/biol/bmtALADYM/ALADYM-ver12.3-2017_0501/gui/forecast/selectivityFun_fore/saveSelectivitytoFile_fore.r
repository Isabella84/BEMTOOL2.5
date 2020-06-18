# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveSelectivitytoFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
# save_selectivity_path = "C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver_pre-beta\\saved_selectivity.csv"
save_selectivity_path <- dialog$getFilename()

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}


if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving Selectivity...        ")

all_selectivities <- data.frame(matrix(nrow=0, ncol=9 ))
colnames(all_selectivities) <- c("year", "month", "param1", "param2", "param3", "param4", "param5", "sel_type", "fleet_segment") 

for (fs in 1:length(FLEETSEGMENTS_names) ) {

fs_object <- FleetList_forecast[[fs]]

#sel_type <- which( SELECTIVITY_TYPE == fs_object@selectivity.method)
#
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

#selectivity_table <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=(selectivity_params$n_par +2) ))
# if (selectivity_params$n_par == 2) {
#   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
# } else if (selectivity_params$n_par == 3) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
# } else if (selectivity_params$n_par == 5) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
# } 
#colnames(selectivity_table) <- heading
#
# years_rep <- rep(years.forecast, 12)
#   years_rep <- years_rep[order(years_rep)]
#  # years_rep <- c("", years_rep)
#   months_rep <- rep(MONTHS, length(years.forecast))
#   # months_rep <- c("seed", months_rep)
#   selectivity_table$year <- years_rep
#   selectivity_table$month <- months_rep
 
 
#for (i in 1:nrow(selectivity_table)) {
#  for (m in 1:((length(years.forecast)*12))) {
#      selectivity_table[m, 3] <- fs_object@selectivity.vector[m,3] 
#      selectivity_table[m, 4] <-  fs_object@selectivity.vector[m,4] 
#     
#     if (selectivity_params$n_par > 2) {
#          selectivity_table[m, 5] <- fs_object@selectivity.vector[m,5] 
#      }
#     if (selectivity_params$n_par > 4) {
#          selectivity_table[m, 6] <- fs_object@selectivity.vector[m,6] 
#          selectivity_table[m, 7] <- fs_object@selectivity.vector[m,7] 
# }  
#      
#  }
#}

selectivity_table <- fs_object@selectivity.vector

#to_add <- data.frame(matrix(NA, nrow=nrow(selectivity_table), ncol=(5-selectivity_params$n_par)))
#selectivity_table <- cbind(selectivity_table, to_add)
#selectivity_table <- cbind(selectivity_table, rep(sel_type, nrow(selectivity_table)))
selectivity_table <- cbind(selectivity_table, rep(FLEETSEGMENTS_names[fs], nrow(selectivity_table)))
colnames(selectivity_table) <- c("year", "month", "param1", "param2", "param3", "param4", "param5", "sel_type", "fleet_segment") 
all_selectivities <- rbind(all_selectivities, selectivity_table)
}

write.table(all_selectivities, file=save_selectivity_path, sep=";", row.names=FALSE)

wnd$destroy() 
  
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("Selectivity saved!")
} 

}
