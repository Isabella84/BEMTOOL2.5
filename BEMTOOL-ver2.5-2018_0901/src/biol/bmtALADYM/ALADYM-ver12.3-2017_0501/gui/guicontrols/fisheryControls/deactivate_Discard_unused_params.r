# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_Discard_unused_params <-function(w) {
 select_index = -1
 
gtkWidgetSetSensitive(combo_discard, TRUE)

 
  gtkWidgetSetSensitive(radio_discard_revogive, TRUE) 
  gtkWidgetSetSensitive(radio_discard_vector, TRUE)
  gtkWidgetSetSensitive(lbl_option_discard, TRUE) 
                           
          gtkWidgetSetSensitive(lbl_RO, TRUE)   
          gtkWidgetSetSensitive(discards.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, TRUE) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, TRUE) 
        gtkWidgetSetSensitive(lbl_EV, TRUE) 
          gtkWidgetSetSensitive(lbl_EV_fem, TRUE) 
            gtkWidgetSetSensitive(lbl_EV_mal, TRUE) 
            
                                   gtkWidgetSetSensitive(lan_obligation.treeview , T)  


#if ((IN_BEMTOOL & phase == "FORECAST") | (!IN_BEMTOOL) )  {
#if (exists("vboxDiscard_fore")) {
# gtkWidgetSetSensitive(vboxDiscard_fore, TRUE)
# }
# if (exists("button_load_discard_fore")) {
# gtkWidgetSetSensitive(button_load_discard_fore, TRUE)
# }
# if (exists("button_saveall_discards_fore")) {
# gtkWidgetSetSensitive(button_saveall_discards_fore, TRUE) 
# }   
#  }

gtkWidgetSetSensitive(button_load_discard, TRUE)
gtkWidgetSetSensitive(button_saveall_discards, TRUE)

selected <- gtkComboBoxGetActiveText(combo_discard)
select_index <- which(DISCARD_CALC == selected)
#print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))
#
#select_fs <- gtkComboBoxGetActiveText(combo_fleetsegments)
## select_index_fs <- which(BMT_FLEETSEGMENTS[associated_fleetsegment_indices] == select_fs ) 
#
#select_index_fs <- which(FLEETSEGMENTS_names == select_fs )
#
#FleetList_simulation[[select_index_fs]]@discard.calculation <<- selected
#
#print(FleetList_simulation[[select_index_fs]]@discard.calculation)

if (select_index == 2 |  select_index == 3) {

  gtkWidgetSetSensitive(radio_discard_revogive, F) 
  gtkWidgetSetSensitive(radio_discard_vector, F)
    gtkWidgetSetSensitive(lbl_option_discard, F) 

         gtkWidgetSetSensitive(lbl_RO, F)   
          gtkWidgetSetSensitive(discards.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV, F) 
          gtkWidgetSetSensitive(lbl_EV_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_mal, F) 
            
                       gtkWidgetSetSensitive(lan_obligation.treeview , F)  

 gtkWidgetSetSensitive(button_load_discard, FALSE)
 gtkWidgetSetSensitive(button_saveall_discards, FALSE) 

 gtkWidgetSetSensitive(radio_discard_revogive, FALSE) 
  gtkWidgetSetSensitive(radio_discard_vector, FALSE) 

# if ((IN_BEMTOOL & phase == "FORECAST") | (!IN_BEMTOOL) )  {
#if (exists("vboxDiscard_fore")) {
# gtkWidgetSetSensitive(vboxDiscard_fore, FALSE)
# }
# if (exists("button_load_discard_fore")) {
# gtkWidgetSetSensitive(button_load_discard_fore, FALSE)
# }
# if (exists("button_saveall_discards_fore")) {
# gtkWidgetSetSensitive(button_saveall_discards_fore, FALSE) 
# } 
# }

} else {
      gtkWidgetSetSensitive(radio_discard_revogive, T) 
  gtkWidgetSetSensitive(radio_discard_vector, T)
    gtkWidgetSetSensitive(lbl_option_discard, T) 
    
      gtkWidgetSetSensitive(lan_obligation.treeview , T)  
  
      if (  gtkToggleButtonGetActive(radio_discard_revogive) ) {
         gtkWidgetSetSensitive(lbl_RO, TRUE)   
          gtkWidgetSetSensitive(discards.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV, F) 
          gtkWidgetSetSensitive(lbl_EV_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_mal, F) 
    } else {
               gtkWidgetSetSensitive(lbl_RO, F)   
          gtkWidgetSetSensitive(discards.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, T) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, T) 
        gtkWidgetSetSensitive(lbl_EV, T) 
          gtkWidgetSetSensitive(lbl_EV_fem, T) 
            gtkWidgetSetSensitive(lbl_EV_mal, T)
    }
   
 gtkWidgetSetSensitive(button_load_discard, TRUE)
 gtkWidgetSetSensitive(button_saveall_discards, TRUE) 

#  if ((IN_BEMTOOL & phase == "FORECAST") | (!IN_BEMTOOL) )  {       
#if (exists("vboxDiscard_fore")) {
# gtkWidgetSetSensitive(vboxDiscard_fore, TRUE)
# }
# if (exists("button_load_discard_fore")) {
# gtkWidgetSetSensitive(button_load_discard_fore, TRUE)
# }
# if (exists("button_saveall_discards_fore")) {
# gtkWidgetSetSensitive(button_saveall_discards_fore, TRUE) 
# }  
#} 
 
}

}



deactive_RO_EV<- function(w) {

selected <- gtkComboBoxGetActiveText(combo_discard)
select_index <- which(DISCARD_CALC == selected)

if (select_index == 2 |  select_index == 3) { 
     gtkWidgetSetSensitive(lbl_RO, F)   
          gtkWidgetSetSensitive(discards.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV, F) 
          gtkWidgetSetSensitive(lbl_EV_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_mal, F) 
} else {

    if (  gtkToggleButtonGetActive(radio_discard_revogive) ) {
         gtkWidgetSetSensitive(lbl_RO, TRUE)   
          gtkWidgetSetSensitive(discards.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV, F) 
          gtkWidgetSetSensitive(lbl_EV_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_mal, F) 
    } else {
               gtkWidgetSetSensitive(lbl_RO, F)   
          gtkWidgetSetSensitive(discards.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F.treeview, T) 
      gtkWidgetSetSensitive( discards_extvector_M.treeview, T) 
        gtkWidgetSetSensitive(lbl_EV, T) 
          gtkWidgetSetSensitive(lbl_EV_fem, T) 
            gtkWidgetSetSensitive(lbl_EV_mal, T)
    }
  }
}