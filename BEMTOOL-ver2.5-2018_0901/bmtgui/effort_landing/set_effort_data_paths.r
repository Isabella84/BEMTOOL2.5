# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_effort_data_paths <- function(w) {
   all_OK_values <- c()

  # -------------------------------------------------------- LIFE SPAN     
          vessels_file <- gtkEntryGetText(entry_effort_vessels_path)
         if (vessels_file == "" )  {
              all_OK_values <- c(all_OK_values, F)
              showError("Select the fishing vessels file!")
          } else {
              all_OK_values <- c(all_OK_values, T)
          }

       if (all(all_OK_values)  )  {
           days_file <- gtkEntryGetText(entry_effort_days_path)
         if (days_file == "" )  {
              all_OK_values <- c(all_OK_values, F)
              showError("Select the average Days at sea file!")
          } else {
              all_OK_values <- c(all_OK_values, T)
          }
      }

             if (all(all_OK_values)  )  {
           gt_file <- gtkEntryGetText(entry_effort_gt_path)
         if (gt_file == "" )  {
              all_OK_values <- c(all_OK_values, F)
              showError("Select the average GT file!")
          } else {
              all_OK_values <- c(all_OK_values, T)
          }
      }
      
             if (all(all_OK_values)  )  {
           kw_file <- gtkEntryGetText(entry_effort_kw_path)
         if (kw_file == "" )  {
              all_OK_values <- c(all_OK_values, F)
              showError("Select the average Kw file!")
          } else {
              all_OK_values <- c(all_OK_values, T)
          }
      }

      
            if (all(all_OK_values)  )  {
             eff_temp <- mat_cfg_EffortData
                eff_temp[2, 2] <- vessels_file
                  eff_temp[2, 3]  <- days_file
                    eff_temp[2, 4]  <- gt_file
                      eff_temp[2, 5]  <- kw_file
                    mat_cfg_EffortData  <<- eff_temp   
                    load_effort_window$destroy()
 
                    set_effort_data_lists()
      
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments)
index_to_update <- which(BMT_FLEETSEGMENTS == selected)       
                  
bmt_fleet.KW <<-    EFFORT_KW_list[[index_to_update]]
bmt_fleet.GT <<-   EFFORT_GT_list[[index_to_update]]
bmt_fleet.NUMBER <<-   EFFORT_NUMBER_list[[index_to_update]]
bmt_fleet.DAY <<-    EFFORT_DAY_list[[index_to_update]]
 
                    bmt_reload_NUMBER_table()
                    bmt_reload_DAY_table()
                    bmt_reload_GT_table()
                    bmt_reload_KW_table()
            }     

}