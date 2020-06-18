# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





bmt_reload_landing_species_info<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments)
  
  index_to_update <- which(BMT_FLEETSEGMENTS == selected)
  
 
   index_to_load_spe = -1
  selected_spe <- gtkComboBoxGetActiveText(bmt_eco_landing_species)
  
  index_to_update_spe <- which(BMT_SPECIES == selected_spe)
    
if (!is.null(selected_spe)) {
 
 LANDING_list <<- LANDING_list_all[[index_to_update]]

 bmt_fleet.LANDING <<- LANDING_list[[index_to_update_spe]]

bmt_reload_LANDING_table()
 
 }
 
 
} 
