# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



deactivate_economic_labour_sorting<- function(w) {

  if ( gtkToggleButtonGetActive(bmt_chk_economic_labour_sorting_coeff) ) {
       
             gtkWidgetSetSensitive(bmt_vboxlabour_sorting, T)            
  }   else {
         gtkWidgetSetSensitive(bmt_vboxlabour_sorting, F)

  }

}


activate_input_sorting<- function(w) {

  if ( gtkToggleButtonGetActive(radio_sorting_equalToDiscardRate) ) {
#          gtkWidgetSetSensitive(bmt_vboxlabour_sorting_table,F) 
 gtkWidgetSetSensitive(bmt_vboxdiscard_cost,F) 

  }   else {
#          gtkWidgetSetSensitive(bmt_vboxlabour_sorting_table,T) 
 gtkWidgetSetSensitive(bmt_vboxdiscard_cost,T) 
     }

}







activate_deactivate_price_tables<- function(choice) {

        gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, F)
         gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
          gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F) 
          gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F) 


 if (choice == PRICE_MODELS$model_name[1]) { #  "BIRDMOD"  
        gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, T)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, T)
         gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
          gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F) 
          gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F) 
                                  gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F)

 } else if (choice == PRICE_MODELS$model_name[2]) {   # "MEFISTO" 
         gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_import, T)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, T)
        gtkWidgetSetSensitive(lbl_price_elast_MW, T)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, T)  
        gtkWidgetSetSensitive(lbl_price_importweight, T)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, T) 
                                  gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F)
 
 } else if (choice == PRICE_MODELS$model_name[3]) {    # "FISHRENT" 
          gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, F)
            gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
        gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F)
                  gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F)
                                  gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F) 

 } else if (choice == PRICE_MODELS$model_name[4]) {     # "BEMMFISH"
          gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, T)
            gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
        gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F)
                  gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F)
                                  gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F) 
          
 } else if (choice == PRICE_MODELS$model_name[5]) {     # previous year
          gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, F)
            gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
        gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F)
                  gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F) 
                          gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, F)


}  else if (choice == PRICE_MODELS$model_name[6]) {     # constant
          gtkWidgetSetSensitive(bmt_vboxprice_elast_landing_byfleet, F)
            gtkWidgetSetSensitive(lbl_price_elast_landing_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_import, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_import, F) 
        gtkWidgetSetSensitive(lbl_price_elast_MW, F)
        gtkWidgetSetSensitive(bmt_vboxprice_elast_MW, F)
                  gtkWidgetSetSensitive(lbl_price_importweight, F)
        gtkWidgetSetSensitive(bmt_vboxprice_importweight, F)  
                  gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_landing, T)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_landing, T)
}

}






activate_deactivate_price_tables_discard<- function(choice) {

        gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_discard, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_discard, F) 


 if (choice == PRICE_MODELS_DISCARD$model_name[1]) { #  "BIRDMOD"  
        gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, T)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, T)
        gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_discard, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_discard, F) 
          
 } else if (choice == PRICE_MODELS_DISCARD$model_name[2]) {     # previous year
                 gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_discard, F)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_discard, F) 


}  else if (choice == PRICE_MODELS_DISCARD$model_name[3]) {     # constant
               gtkWidgetSetSensitive(bmt_vboxprice_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(lbl_price_elast_discard_byfleet, F)
        gtkWidgetSetSensitive(bmt_vboxprice_costant_byfleet_discard, T)
            gtkWidgetSetSensitive(lbl_price_costant_byfleet_discard, T) 
}

}




activate_deactivate_varcost_tables<- function(choice) {

 if (choice == VARCOST_MODELS$model_name[1]) { #  "BIRDMOD"  
   gtkWidgetSetSensitive(lbl_cost_fuelprice, F)
      gtkWidgetSetSensitive(bmt_cost_fuelprice.sw , F)

 } else if (choice == VARCOST_MODELS$model_name[2]) {   # "MEFISTO" 
   gtkWidgetSetSensitive(lbl_cost_fuelprice, T)
      gtkWidgetSetSensitive(bmt_cost_fuelprice.sw , T)
 
 } else if (choice == VARCOST_MODELS$model_name[3]) {    # "FISHRENT" 
   gtkWidgetSetSensitive(lbl_cost_fuelprice, T)
      gtkWidgetSetSensitive(bmt_cost_fuelprice.sw , T)

 } else if (choice == VARCOST_MODELS$model_name[4]) {     # "BEMMFISH"
      gtkWidgetSetSensitive(lbl_cost_fuelprice, F)
      gtkWidgetSetSensitive(bmt_cost_fuelprice.sw , F)
    
 }

}





activate_deactivate_fleetDYN<- function(w) {

gtkWidgetSetSensitive(bmt_behav_dyn.sw,F) 

  if ( gtkToggleButtonGetActive(chk_behav_dyn) ) {
          gtkWidgetSetSensitive(bmt_behav_dyn.sw,T) 
  }   else {
          gtkWidgetSetSensitive(bmt_behav_dyn.sw,F) 
     }

}



activate_deactivate_fleetACT<- function(w) {

gtkWidgetSetSensitive(bmt_behav_act.sw,F) 

  if ( gtkToggleButtonGetActive(chk_behav_act) ) {
          gtkWidgetSetSensitive(bmt_behav_act.sw,T) 
  }   else {
          gtkWidgetSetSensitive(bmt_behav_act.sw,F) 
     }

}





activate_deactivate_techPROGR<- function(w) {

gtkWidgetSetSensitive(bmt_behav_progr.sw ,F) 

  if ( gtkToggleButtonGetActive(chk_behav_progr) ) {
          gtkWidgetSetSensitive(bmt_behav_progr.sw ,T) 
  }   else {
          gtkWidgetSetSensitive(bmt_behav_progr.sw ,F) 
     }

}
