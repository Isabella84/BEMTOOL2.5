# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_FishingEffort_unused_params <-function(w){
 gtkWidgetSetSensitive(VESSELS.treeview, TRUE)
       gtkWidgetSetSensitive(entry_VESSELS_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_VESSELS, TRUE)
 
  gtkWidgetSetSensitive(DAYS.treeview, TRUE)
      gtkWidgetSetSensitive(entry_DAYS_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_DAYS, TRUE)
      
  gtkWidgetSetSensitive(GT.treeview, TRUE)
    gtkWidgetSetSensitive(entry_GT_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_GT, TRUE)

  gtkWidgetSetSensitive(FISHINGEFFORT.treeview, TRUE)
    gtkWidgetSetSensitive( btn_load_seed_FISHINGEFFORT, TRUE)
      gtkWidgetSetSensitive(  entry_FISHINGEFFORT_seedvalue, TRUE)
  
  gtkWidgetSetSensitive(button_load_fishingcoeff, TRUE)
    gtkWidgetSetSensitive(button_load_effortdata, TRUE) 
     # gtkWidgetSetSensitive(button_exp_fishingcoeff, TRUE)
    gtkWidgetSetSensitive(button_exp_effortdata, TRUE)  
    
     if ((!IN_BEMTOOL) | (IN_BEMTOOL & phase=="FORECAST")) {
    gtkWidgetSetSensitive(button_load_fc_fore, TRUE)
    gtkWidgetSetSensitive(button_load_effortdata_fore, TRUE) 
    gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
    gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
    gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
    gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)
    gtkWidgetSetSensitive(button_saveall_effortdata_fore, TRUE)
   # gtkWidgetSetSensitive(button_saveall_fc_fore, TRUE)
    }

if ( gtkToggleButtonGetActive(radio_effortdata) ) {
  gtkWidgetSetSensitive(VESSELS.treeview, TRUE)
         gtkWidgetSetSensitive(entry_VESSELS_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_VESSELS, TRUE)
  
  gtkWidgetSetSensitive(DAYS.treeview, TRUE)
        gtkWidgetSetSensitive(entry_DAYS_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_DAYS, TRUE)
  
  gtkWidgetSetSensitive(GT.treeview, TRUE)
      gtkWidgetSetSensitive(entry_GT_seedvalue, TRUE)
      gtkWidgetSetSensitive(btn_load_seed_GT, TRUE)
  

  gtkWidgetSetSensitive(FISHINGEFFORT.treeview, FALSE)
      gtkWidgetSetSensitive( btn_load_seed_FISHINGEFFORT, FALSE)
      gtkWidgetSetSensitive(  entry_FISHINGEFFORT_seedvalue, FALSE)

      gtkWidgetSetSensitive(button_load_effortdata, TRUE)                   
  gtkWidgetSetSensitive(button_load_fishingcoeff, FALSE)
           # gtkWidgetSetSensitive(button_exp_fishingcoeff, FALSE)    
           gtkWidgetSetSensitive(button_exp_effortdata, TRUE)   
      
   if ((!IN_BEMTOOL) | (IN_BEMTOOL & phase=="FORECAST")) {
        gtkWidgetSetSensitive(button_load_fc_fore, FALSE)
    gtkWidgetSetSensitive(button_load_effortdata_fore, TRUE)

        gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, FALSE)
       gtkWidgetSetSensitive(button_saveall_effortdata_fore, TRUE)
    #gtkWidgetSetSensitive(button_saveall_fc_fore, FALSE)
    }
    
} else if ( gtkToggleButtonGetActive(radio_fishingcoeff) ) {
  gtkWidgetSetSensitive(VESSELS.treeview, FALSE)
   gtkWidgetSetSensitive(entry_VESSELS_seedvalue, FALSE)
    gtkWidgetSetSensitive(btn_load_seed_VESSELS, FALSE)
  
  gtkWidgetSetSensitive(DAYS.treeview, FALSE)
    gtkWidgetSetSensitive(entry_DAYS_seedvalue, FALSE)
      gtkWidgetSetSensitive(btn_load_seed_DAYS, FALSE)
  
  gtkWidgetSetSensitive(GT.treeview, FALSE)
      gtkWidgetSetSensitive(entry_GT_seedvalue, FALSE)
      gtkWidgetSetSensitive(btn_load_seed_GT, FALSE)
            
  gtkWidgetSetSensitive(FISHINGEFFORT.treeview, TRUE)
      gtkWidgetSetSensitive( btn_load_seed_FISHINGEFFORT, TRUE)
      gtkWidgetSetSensitive(  entry_FISHINGEFFORT_seedvalue, TRUE)

      gtkWidgetSetSensitive(button_load_effortdata, FALSE)     
  gtkWidgetSetSensitive(button_load_fishingcoeff, TRUE)
          #  gtkWidgetSetSensitive(button_exp_fishingcoeff, TRUE)
    gtkWidgetSetSensitive(button_exp_effortdata, FALSE) 
    
     if ((!IN_BEMTOOL) | (IN_BEMTOOL & phase=="FORECAST")) {
        gtkWidgetSetSensitive(button_load_fc_fore, TRUE)
    gtkWidgetSetSensitive(button_load_effortdata_fore, FALSE)
         gtkWidgetSetSensitive(button_saveall_effortdata_fore, FALSE)
   # gtkWidgetSetSensitive(button_saveall_fc_fore, TRUE)
    gtkWidgetSetSensitive(VESSELS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(GT_fore.treeview, FALSE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)
  }
  
} 

}
