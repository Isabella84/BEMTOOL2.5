# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





deactivate_escape_Survivability_unused_params <-function(w) {

 select_index = -1
 

gtkWidgetSetSensitive(lbl_option_escape_survivability, TRUE)
gtkWidgetSetSensitive(tbl_escape_surviv, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_size, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_constant, TRUE)



selected <- gtkComboBoxGetActiveText(combo_escape_survival_rate)

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


if (selected == "YES" ) {
      if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@discard.survivability.calculation <<- "Y"
      }

gtkWidgetSetSensitive(lbl_option_escape_survivability, TRUE)
gtkWidgetSetSensitive(tbl_escape_surviv, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_size, TRUE)
gtkWidgetSetSensitive(radio_escape_survivability_constant, TRUE)


gtkWidgetSetSensitive(button_load_survivability_vect_age, F)
gtkWidgetSetSensitive(button_export_survivability_vect_age, F)

} else {

if (length(select_index_fleet) != 0) {
FleetList_simulation[[select_index_fleet]]@escape.survivability.calculation <<- "N"
}

gtkWidgetSetSensitive(lbl_option_escape_survivability, F)
gtkWidgetSetSensitive(tbl_escape_surviv, F)
gtkWidgetSetSensitive(radio_escape_survivability_size, F)
gtkWidgetSetSensitive(radio_escape_survivability_constant, F)
gtkWidgetSetSensitive(button_load_survivability_vect_age, F)
gtkWidgetSetSensitive(button_export_survivability_vect_age, F)

}
}





deactive_survivability_C_S <- function(w) {

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)

 gtkWidgetSetSensitive(lbl_escape_surv_C, T)
  gtkWidgetSetSensitive(h_escape_surv_cost_M, T)
   gtkWidgetSetSensitive(h_escape_surv_cost_F, T)
    gtkWidgetSetSensitive(lbl_DOS, T)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O, T)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV, T)
      
        gtkWidgetSetSensitive(button_load_survivability_vect_age, T)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, T)
      
            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1, T)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2, T)

             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M, T)
        gtkWidgetSetSensitive(escape_survival_extvector_M.sw, T)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F, T)
        gtkWidgetSetSensitive(escape_survival_extvector_F.sw, T)


    if (  gtkToggleButtonGetActive(radio_escape_survivability_size) ) {
    
          if (length(select_index_fleet) != 0) {
    FleetList_simulation[[select_index_fleet]]@escape.survivability.datatype <<- "DOS"
    }

 gtkWidgetSetSensitive(lbl_escape_surv_C, F)
  gtkWidgetSetSensitive(h_escape_surv_cost_M, F)
   gtkWidgetSetSensitive(h_escape_surv_cost_F, F)
    gtkWidgetSetSensitive(lbl_DOS, T)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O, T)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV, T)
       gtkWidgetSetSensitive(button_load_survivability_vect_age, F)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, F)
      
            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1, T)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2, T)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2, T)

             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M, T)
        gtkWidgetSetSensitive(escape_survival_extvector_M.sw, T)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F, T)
        gtkWidgetSetSensitive(escape_survival_extvector_F.sw, T)

    } else {

     if (length(select_index_fleet) != 0) {
    FleetList_simulation[[select_index_fleet]]@escape.survivability.datatype <<- "C"
    }

     gtkWidgetSetSensitive(lbl_escape_surv_C, T)
  gtkWidgetSetSensitive(h_escape_surv_cost_M, T)
   gtkWidgetSetSensitive(h_escape_surv_cost_F, T)
    gtkWidgetSetSensitive(lbl_DOS, F)
     gtkWidgetSetSensitive(radio_escape_survivability_size_O, F)
      gtkWidgetSetSensitive(radio_escape_survivability_size_EV, F)
            # h_escape_surv_size_option_O
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param1, F)
       gtkWidgetSetSensitive(entry_survivability_ogive_param1, F)
       gtkWidgetSetSensitive(lbl_option_survivability_ogive_param2, F)
       gtkWidgetSetSensitive(entry_survivability_ogive_param2, F)
       
gtkWidgetSetSensitive(button_load_survivability_vect_age, F)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, F)


             # h_escape_surv_size_option_EV
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_M, F)
        gtkWidgetSetSensitive(escape_survival_extvector_M.sw, F)
        gtkWidgetSetSensitive(lbl_esc_surv_DOS_EV_F, F)
        gtkWidgetSetSensitive(escape_survival_extvector_F.sw, F)

    }

#  }
}






deactive_survivability_O_EV <- function(w) {


selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


      gtkWidgetSetSensitive(h_escape_surv_size_option_O, T)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV, T)
        gtkWidgetSetSensitive( escape_survival_extvector_F.treeview, T)
           gtkWidgetSetSensitive( escape_survival_extvector_M.treeview, T)
           

                   gtkWidgetSetSensitive(button_load_survivability_vect_age, T)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, T)



selected <- gtkComboBoxGetActiveText(combo_escape_survival_rate)

    if (  gtkToggleButtonGetActive(radio_escape_survivability_size) ) {

        if (length(select_index_fleet) != 0) {
    FleetList_simulation[[select_index_fleet]]@escape.survivability.datatype <<- "DOS"

    }
      
      if (gtkToggleButtonGetActive(radio_escape_survivability_size_O) ) {
      
            if (length(select_index_fleet) != 0) {
          FleetList_simulation[[select_index_fleet]]@escape.survivability.DOS.datatype <<- "O"
          }
      
              gtkWidgetSetSensitive(button_load_survivability_vect_age, F)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, F)
      
         gtkWidgetSetSensitive(h_escape_surv_size_option_O, T)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV, F)
               gtkWidgetSetSensitive( escape_survival_extvector_F.treeview, F)
           gtkWidgetSetSensitive( escape_survival_extvector_M.treeview, F)
      } else {
      
            if (length(select_index_fleet) != 0) {
         FleetList_simulation[[select_index_fleet]]@escape.survivability.DOS.datatype <<- "V"
         FleetList_simulation[[select_index_fleet]]@escape.survivability.DOS.ext_vect.F <<- get_table("ESCAPE_SURVIVABILITY_EXT_VECTOR_F")
         FleetList_simulation[[select_index_fleet]]@escape.survivability.DOS.ext_vect.M <<- get_table("ESCAPE_SURVIVABILITY_EXT_VECTOR_M")
         }
              
    gtkWidgetSetSensitive(button_load_survivability_vect_age, T)
          gtkWidgetSetSensitive(button_export_survivability_vect_age, T)
              
               gtkWidgetSetSensitive(h_escape_surv_size_option_O, F)
        gtkWidgetSetSensitive(h_escape_surv_size_option_EV, T)
                       gtkWidgetSetSensitive( escape_survival_extvector_F.treeview, T)
           gtkWidgetSetSensitive( escape_survival_extvector_M.treeview, T)
      }

    } 
}