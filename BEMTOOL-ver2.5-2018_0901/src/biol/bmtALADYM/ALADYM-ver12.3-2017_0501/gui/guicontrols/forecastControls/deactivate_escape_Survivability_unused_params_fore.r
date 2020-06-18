# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





deactivate_escape_Survivability_unused_params_fore <-function(w) {

  select_index = -1
  
 gtkWidgetSetSensitive(lbl_option_escape_survivability_fore, TRUE)
gtkWidgetSetSensitive(tbl_escape_surviv_fore, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_size_fore, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_constant_fore, TRUE)
  




  
  selected <- gtkComboBoxGetActiveText(combo_escape_survival_rate_fore)




selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)

 
if (selected == "YES" ) {
      if (length(select_index_fleet) != 0) {
       FleetList_forecast[[select_index_fleet]]@escape.survivability.calculation <<- "Y"
      }
gtkWidgetSetSensitive(lbl_option_escape_survivability_fore, TRUE)
gtkWidgetSetSensitive(tbl_escape_surviv_fore, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_size_fore, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_constant_fore, TRUE)







gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, F)
gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, F)

} else {


      if (length(select_index_fleet) != 0) {
FleetList_forecast[[select_index_fleet]]@escape.survivability.calculation <<- "N"
}

gtkWidgetSetSensitive(lbl_option_escape_survivability_fore, F)
gtkWidgetSetSensitive(tbl_escape_surviv_fore, F)
gtkWidgetSetSensitive(radio_escape_survivability_size_fore, F)
gtkWidgetSetSensitive(radio_escape_survivability_constant_fore, F)

gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, F)
gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, F)

}
}





deactive_survivability_C_S_fore <- function(w) {







selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)
  







 gtkWidgetSetSensitive(lbl_escape_surv_C_fore, T)
  gtkWidgetSetSensitive(h_escape_surv_cost_M_fore, T)
   gtkWidgetSetSensitive(h_escape_surv_cost_F_fore, T)
    gtkWidgetSetSensitive(lbl_DOS_fore, T)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O_fore,T)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV_fore, T)
gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, T)
gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, T)

            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1_fore, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1_fore,T)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2_fore, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2_fore, T)

             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M_fore, T)
        gtkWidgetSetSensitive(escape_survival_extvector_M_fore.sw, T)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F_fore, T)
        gtkWidgetSetSensitive(escape_survival_extvector_F_fore.sw, T)


  if (  gtkToggleButtonGetActive(radio_escape_survivability_size_fore) ) {
    
          if (length(select_index_fleet) != 0) {
    FleetList_forecast[[select_index_fleet]]@escape.survivability.datatype <<- "DOS"
    }


 gtkWidgetSetSensitive(lbl_escape_surv_C_fore, F)
  gtkWidgetSetSensitive(h_escape_surv_cost_M_fore, F)
   gtkWidgetSetSensitive(h_escape_surv_cost_F_fore, F)
    gtkWidgetSetSensitive(lbl_DOS_fore, T)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O_fore, T)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV_fore, T)
   gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, F)
          gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, F)
    
            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1_fore, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1_fore, T)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2_fore, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2_fore, T)

             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M_fore, T)
        gtkWidgetSetSensitive(escape_survival_extvector_M_fore.sw, T)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F_fore, T)
        gtkWidgetSetSensitive(escape_survival_extvector_F_fore.sw, T)

    } else {


     if (length(select_index_fleet) != 0) {
    FleetList_forecast[[select_index_fleet]]@escape.survivability.datatype <<- "C"
    }

     gtkWidgetSetSensitive(lbl_escape_surv_C_fore, T)
  gtkWidgetSetSensitive(h_escape_surv_cost_M_fore, T)
   gtkWidgetSetSensitive(h_escape_surv_cost_F_fore, T)
    gtkWidgetSetSensitive(lbl_DOS_fore, F)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O_fore, F)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV_fore, F)
            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1_fore, F)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1_fore, F)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2_fore, F)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2_fore, F)

	   gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, F)
          gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, F)

	   
             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M_fore, F)
        gtkWidgetSetSensitive(escape_survival_extvector_M_fore.sw, F)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F_fore, F)
        gtkWidgetSetSensitive(escape_survival_extvector_F_fore.sw, F)

    }


}





deactive_survivability_O_EV_fore <- function(w) {




selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


      gtkWidgetSetSensitive(h_escape_surv_size_option_O_fore, T)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV_fore, T)
        gtkWidgetSetSensitive( escape_survival_extvector_F_fore.treeview, T)
           gtkWidgetSetSensitive( escape_survival_extvector_M_fore.treeview, T)


   gtkWidgetSetSensitive(button_load_survivability_vect_age, T)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, T)
		   		   
selected <- gtkComboBoxGetActiveText(combo_escape_survival_rate_fore)


    if (  gtkToggleButtonGetActive(radio_escape_survivability_size_fore) ) {
	
        if (length(select_index_fleet) != 0) {
    FleetList_forecast[[select_index_fleet]]@escape.survivability.datatype <<- "DOS"
    }
      
      if (gtkToggleButtonGetActive(radio_escape_survivability_size_O_fore) ) {
      
            if (length(select_index_fleet) != 0) {
          FleetList_forecast[[select_index_fleet]]@escape.survivability.DOS.datatype <<- "O"
          }
      
  gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, F)
gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, F)
      
         gtkWidgetSetSensitive(h_escape_surv_size_option_O_fore, T)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV_fore, F)
               gtkWidgetSetSensitive( escape_survival_extvector_F_fore.treeview, F)
           gtkWidgetSetSensitive( escape_survival_extvector_M_fore.treeview, F)
      } else {
      
            if (length(select_index_fleet) != 0) {
         FleetList_forecast[[select_index_fleet]]@escape.survivability.DOS.datatype <<- "V"
         FleetList_forecast[[select_index_fleet]]@escape.survivability.DOS.ext_vect.F <<- get_table("ESCAPE_SURVIVABILITY_EXT_VECTOR_F_FORE")
         FleetList_forecast[[select_index_fleet]]@escape.survivability.DOS.ext_vect.M <<- get_table("ESCAPE_SURVIVABILITY_EXT_VECTOR_M_FORE")
         }
              
    gtkWidgetSetSensitive(button_load_survivability_vect_age_fore, T)
          gtkWidgetSetSensitive(button_export_survivability_vect_age_fore, T)
              
               gtkWidgetSetSensitive(h_escape_surv_size_option_O_fore, F)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV_fore, T)
                       gtkWidgetSetSensitive( escape_survival_extvector_F_fore.treeview, T)
           gtkWidgetSetSensitive( escape_survival_extvector_M_fore.treeview, T)
      }

    } 
    }