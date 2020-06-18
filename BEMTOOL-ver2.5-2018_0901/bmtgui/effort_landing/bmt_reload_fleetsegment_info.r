# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




bmt_reload_fleetsegment_info<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments)
  
  index_to_update <- which(BMT_FLEETSEGMENTS == selected)
  
     for (choice in BMT_SPECIES) { 
      gtkComboBoxRemoveText(bmt_eco_landing_species, 0)
    }
    
 for (choice in BMT_SPECIES) { 
     bmt_eco_landing_species$appendText(choice)  
    }
    
gtkComboBoxSetActive(bmt_eco_landing_species, 0 )    
  
if (!is.null(selected)) {
 
bmt_fleet.KW <<- EFFORT_KW_list[[index_to_update]]
bmt_fleet.GT <<- EFFORT_GT_list[[index_to_update]]
bmt_fleet.NUMBER <<- EFFORT_NUMBER_list[[index_to_update]]
bmt_fleet.DAY <<- EFFORT_DAY_list[[index_to_update]]

bmt_reload_NUMBER_table()
bmt_reload_DAY_table()
bmt_reload_GT_table()
bmt_reload_KW_table()


 LANDING_list <<- LANDING_list_all[[index_to_update]]

 bmt_fleet.LANDING <<- LANDING_list[[1]]

bmt_reload_LANDING_table()
 
 }
 
 
} 

