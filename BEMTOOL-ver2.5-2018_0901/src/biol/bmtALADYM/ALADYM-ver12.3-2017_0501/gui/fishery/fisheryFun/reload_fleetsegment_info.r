# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




reload_fleetsegment_info<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
if (!is.null(selected)) {
  # gtkWidgetSetSensitive(button_savechanges_fleet, TRUE)  
 index_to_load <- which(FLEETSEGMENTS_names == selected )
 loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]]) 

   #if ( !IN_BEMTOOL ) {  
  #gtkComboBoxSetActive(combo_fleetsegments_fore, which(FLEETSEGMENTS_names == selected)-1)
  # }
   
 } else {
   clear_FisheryGUI(w)  
 }
# print(FleetList_simulation[[1]]@fishingmortality.M.vector)
  # print("END....................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
}
