# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadSelectivityfromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

loaded_selectivity <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
# loaded_selectivity <- read.csv( "C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\BEMTOOL-ver1.0_2012\\caso di studio GSA 18\\selectivity - MULL BAR_fore.csv", sep=";", na.strings = "")  

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

 if (vai) {
for (fs in 1:length(FLEETSEGMENTS_names) ) {
   selectivity_fs <- loaded_selectivity[as.character(loaded_selectivity$fleet_segment) == FLEETSEGMENTS_names[fs],]
   # selectivity_fs[1,1] <- ""

#if ( selectivity_fs$sel_type[1]==1 ) {
#  selectivity_params <- list(n_par = 2, param1 = "SL50%", param2 = "SL75%-SL25%") 
#  FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method   
#} else if ( selectivity_fs$sel_type[1]==2) {
#  selectivity_params <- list(n_par = 3, param1 = "SL50%", param2 = "SL75%-SL25%", param3 = "DL50%") 
#  FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method    
#} else if ( selectivity_fs$sel_type[1]==3) {
#  selectivity_params <- list(n_par = 2, param1 = "mean", param2 = "st.dev") 
#    FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method     
#} else if ( selectivity_fs$sel_type[1]==4) {
#  selectivity_params <- list(n_par = 2, param1 = "mean", param2 = "st.dev")
#   FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method      
#} else if ( selectivity_fs$sel_type[1]==5) {
#  selectivity_params <- list(n_par = 5, param1 = "mean1", param2 = "st.dev1", param3 = "mean2", param4 = "st.dev2", param5 = "b") 
#    FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method     
#} else if ( selectivity_fs$sel_type[1]==6) {
#  selectivity_params <- list(n_par = 3, param1 = "mean1", param2 = "st.dev1", param3 = "st.dev2") 
#    FleetList_forecast[[fs]]@selectivity.method <<- FleetList_simulation[[fs]]@selectivity.method     
#}

FleetList_forecast[[fs]]@selectivity.vector <<- selectivity_fs[, 1:(ncol(selectivity_fs)-1)]  
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Selectivity loaded!        ")
} 

}
