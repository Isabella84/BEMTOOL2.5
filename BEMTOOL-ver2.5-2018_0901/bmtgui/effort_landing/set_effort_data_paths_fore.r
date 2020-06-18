# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_effort_data_paths_fore <- function(w) {
   all_OK_values <- c()

  # -------------------------------------------------------- LIFE SPAN     
          vessels_file <- gtkEntryGetText(entry_effort_vessels_path_fore)
         if (vessels_file == "" )  {
              all_OK_values <- c(all_OK_values, T)
              showError("Fishing vessels file not found!")
              vessels_file <- ""
          } else {
              all_OK_values <- c(all_OK_values, T)
          }

       if (all(all_OK_values)  )  {
           days_file <- gtkEntryGetText(entry_effort_days_path_fore)
         if (days_file == "" )  {
              all_OK_values <- c(all_OK_values, T)
              showError("Average Days at sea file not found!")
              days_file <- ""
          } else {
              all_OK_values <- c(all_OK_values, T)
          }
      }

            if (all(all_OK_values)  )  {
               mat_cfg_scenario_settings_fore_options <<- vector(mode="list", length=2)

               mat_cfg_scenario_settings_fore_options[[1]] <<- vessels_file
       mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_file 
       
            
                    load_effort_window_fore$destroy()
 
                    set_effort_data_lists_fore()
      
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_effort_r4)
index_to_update <- which(BMT_FLEETSEGMENTS == selected)       
                  
bmt_fleet.NUMBER_r4 <<-   EFFORT_NUMBER_list_fore[[index_to_update]]
bmt_fleet.DAY_r4 <<-    EFFORT_DAY_list_fore[[index_to_update]]
 
                    bmt_reload_NUMBER_r4_table()
                    bmt_reload_DAY_r4_table()

            }     

}