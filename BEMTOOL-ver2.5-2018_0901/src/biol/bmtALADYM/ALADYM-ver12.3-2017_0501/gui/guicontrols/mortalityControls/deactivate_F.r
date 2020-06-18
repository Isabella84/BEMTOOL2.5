# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



deactivate_F <-function(w){
  gtkWidgetSetSensitive(vboxFishingMortality, FALSE)
  gtkWidgetSetSensitive(h_frame_totalmortality, TRUE)
#  gtkWidgetSetSensitive(button_load_F, FALSE)
  gtkWidgetSetSensitive(vboxFisherySelectivity, TRUE) 
  
     if ((!IN_BEMTOOL) | (IN_BEMTOOL & phase=="FORECAST")) {
      gtkWidgetSetSensitive(vboxFisherySelectivity_fore, TRUE)
  gtkWidgetSetSensitive(button_load_selectivity_fore, TRUE)
     gtkWidgetSetSensitive(button_saveall_selectivity_fore, TRUE)  
     } 
  
    gtkWidgetSetSensitive(button_load_selectivity, TRUE) 
#  gtkWidgetSetSensitive(button_saveall_Fmortalities, FALSE)  
  gtkWidgetSetSensitive(button_saveall_selectivity, TRUE)
  new_aldSimulation@enteringMortality <<- "Z" 
}
