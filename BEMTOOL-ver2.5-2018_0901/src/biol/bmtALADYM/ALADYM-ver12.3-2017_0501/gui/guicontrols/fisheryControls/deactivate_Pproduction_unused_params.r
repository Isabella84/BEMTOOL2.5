# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_Pproduction_unused_params <-function(w){
gtkWidgetSetSensitive(hboxProduction, TRUE)
gtkWidgetSetSensitive(hboxPproduction, TRUE)
  gtkWidgetSetSensitive( productions.treeview, TRUE)
   gtkWidgetSetSensitive( pproductions.treeview, TRUE)
   
      gtkWidgetSetSensitive( vboxmonthlyDiscard, TRUE) 
   
 gtkWidgetSetSensitive(button_load_production, TRUE)
 gtkWidgetSetSensitive(button_load_p_production, TRUE)
 gtkWidgetSetSensitive(button_exp_production, TRUE)
 
  gtkWidgetSetSensitive(button_load_discard_data, TRUE)
   gtkWidgetSetSensitive(button_exp_discard_data, TRUE)
   
if ( gtkToggleButtonGetActive(radio_data) ) {
  gtkWidgetSetSensitive( productions.treeview, TRUE)
   gtkWidgetSetSensitive(pproductions.treeview, FALSE)
                                   gtkWidgetSetSensitive(hboxProduction, T)
                                         gtkWidgetSetSensitive( vboxmonthlyDiscard, TRUE) 
                                gtkWidgetSetSensitive(hboxPproduction, F)
     
  gtkWidgetSetSensitive(button_load_production, TRUE) 
  gtkWidgetSetSensitive(button_load_p_production, FALSE)
  gtkWidgetSetSensitive(button_exp_production, TRUE)

    gtkWidgetSetSensitive(button_load_discard_data, TRUE)
   gtkWidgetSetSensitive(button_exp_discard_data, TRUE)
 
} else if ( gtkToggleButtonGetActive(radio_pp) ) {
  gtkWidgetSetSensitive( productions.treeview, FALSE)
   gtkWidgetSetSensitive(pproductions.treeview, TRUE)
                                   gtkWidgetSetSensitive(hboxProduction, F)
                                         gtkWidgetSetSensitive( vboxmonthlyDiscard, F) 
                                gtkWidgetSetSensitive(hboxPproduction, T)
  
  gtkWidgetSetSensitive(button_load_production, FALSE) 
  gtkWidgetSetSensitive(button_load_p_production, TRUE)
  gtkWidgetSetSensitive(button_exp_production, FALSE)
  
      gtkWidgetSetSensitive(button_load_discard_data, FALSE)
   gtkWidgetSetSensitive(button_exp_discard_data, FALSE)

    
} 

}
