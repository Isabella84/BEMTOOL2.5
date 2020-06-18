# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


bmt_reload_fleetsegment_info_fore<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_effort_r4)
  
  index_to_update <- which(BMT_FLEETSEGMENTS == selected)
  
if (!is.null(selected)) {

if (length(EFFORT_NUMBER_list_fore) > 0) {
bmt_fleet.NUMBER_r4 <<- EFFORT_NUMBER_list_fore[[index_to_update]]
bmt_fleet.DAY_r4 <<-  EFFORT_DAY_list_fore[[index_to_update]]
} else {
bmt_fleet.NUMBER_r4 <<- NULL
bmt_fleet.DAY_r4 <<-  NULL
}

bmt_reload_NUMBER_r4_table()
bmt_reload_DAY_r4_table()
 
 }
 
 
} 
