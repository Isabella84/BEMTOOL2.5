# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





deactivate_Discard_unused_params_fore <-function(w) {

  gtkWidgetSetSensitive(radio_discard_revogive_fore, TRUE) 
  gtkWidgetSetSensitive(radio_discard_vector_fore, TRUE)
  gtkWidgetSetSensitive(lbl_option_discard_fore, TRUE) 
                           
          gtkWidgetSetSensitive(lbl_RO_fore, TRUE)   
          gtkWidgetSetSensitive(discards_fore.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, TRUE) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, TRUE) 
        gtkWidgetSetSensitive(lbl_EV_fore, TRUE) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, TRUE) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, TRUE) 
            
            gtkWidgetSetSensitive(lan_obligation_fore.treeview , T)  

gtkWidgetSetSensitive(button_load_discard_fore, TRUE)
gtkWidgetSetSensitive(button_saveall_discards_fore, TRUE)

 select_index = -1
 selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
 
   if (!is.null(selected)) {
select_index <- which(FLEETSEGMENTS_names == selected )

if (as.character(FleetList_forecast[[select_index]]@discard.calculation) == "0" | as.character(FleetList_forecast[[select_index]]@discard.calculation) == "NA" ) {

  gtkWidgetSetSensitive(radio_discard_revogive_fore, F) 
  gtkWidgetSetSensitive(radio_discard_vector_fore, F)
    gtkWidgetSetSensitive(lbl_option_discard_fore, F) 

         gtkWidgetSetSensitive(lbl_RO_fore, F)   
          gtkWidgetSetSensitive(discards_fore.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV_fore, F) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, F) 

                       gtkWidgetSetSensitive(lan_obligation_fore.treeview , F)  

 gtkWidgetSetSensitive(button_load_discard_fore, FALSE)
 gtkWidgetSetSensitive(button_saveall_discards_fore, FALSE) 

 gtkWidgetSetSensitive(radio_discard_revogive_fore, FALSE) 
  gtkWidgetSetSensitive(radio_discard_vector_fore, FALSE) 

} else {
      gtkWidgetSetSensitive(radio_discard_revogive_fore, T) 
  gtkWidgetSetSensitive(radio_discard_vector_fore, T)
    gtkWidgetSetSensitive(lbl_option_discard_fore, T) 
    
      gtkWidgetSetSensitive(lan_obligation_fore.treeview , T)  
  
      if (  FleetList_forecast[[select_index]]@discard.datatype == "Reverse ogive"  ) {
         gtkWidgetSetSensitive(lbl_RO_fore, TRUE)   
          gtkWidgetSetSensitive(discards_fore.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV_fore, F) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, F) 

    } else {
               gtkWidgetSetSensitive(lbl_RO_fore, F)   
          gtkWidgetSetSensitive(discards_fore.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, T) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, T) 
        gtkWidgetSetSensitive(lbl_EV_fore, T) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, T) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, T) 

    }
   
 gtkWidgetSetSensitive(button_load_discard_fore, TRUE)
 gtkWidgetSetSensitive(button_saveall_discards_fore, TRUE) 
 
}

}

}



deactive_RO_EV_fore <- function(w) {

 select_index = -1
 selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)

  if (!is.null(selected)) {
select_index <- which(FLEETSEGMENTS_names == selected )

if ( as.character(FleetList_forecast[[select_index]]@discard.calculation) == "0" | as.character(FleetList_forecast[[select_index]]@discard.calculation) == "NA"  ) { 
     gtkWidgetSetSensitive(lbl_RO_fore, F)   
          gtkWidgetSetSensitive(discards_fore.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV_fore, F) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, F) 
 
} else {

    if (  FleetList_forecast[[select_index]]@discard.datatype == "reverse ogive"  ) {
         gtkWidgetSetSensitive(lbl_RO_fore, TRUE)   
          gtkWidgetSetSensitive(discards_fore.treeview, TRUE)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, F) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, F) 
        gtkWidgetSetSensitive(lbl_EV_fore, F) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, F) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, F) 

    } else {
               gtkWidgetSetSensitive(lbl_RO_fore, F)   
          gtkWidgetSetSensitive(discards_fore.treeview, F)
          
    gtkWidgetSetSensitive( discards_extvector_F_fore.treeview, T) 
      gtkWidgetSetSensitive( discards_extvector_M_fore.treeview, T) 
        gtkWidgetSetSensitive(lbl_EV_fore, T) 
          gtkWidgetSetSensitive(lbl_EV_fore_fem, T) 
            gtkWidgetSetSensitive(lbl_EV_fore_mal, T) 

    }
  }
  
  }
}