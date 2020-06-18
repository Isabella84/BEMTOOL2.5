# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


loadRulesintoGUI<- function() {

gtkToggleButtonSetActive(bmt_chk_hr_statusquo, F) 
    gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, F) 
    gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, F) 
     gtkToggleButtonSetActive(bmt_chk_hr_change_effort, F) 
 gtkToggleButtonSetActive(bmt_chk_hr_change_f_by_fleet, F) 
     gtkToggleButtonSetActive(bmt_chk_hr_change_total_f, F) 
    gtkToggleButtonSetActive(bmt_chk_hr_set_tac, F) 
   gtkToggleButtonSetActive(bmt_chk_hr_MEY, F)  #                     
  
       gtkEntrySetText(bmt_entry_scenario_name ,  mat_cfg_scenario_settings_fore[2,3])
                           
if ( MEY_CALCULATION) {
       gtkToggleButtonSetActive(bmt_chk_hr_MEY, T)
         all_the_fleets_mey <- c(  BMT_FLEETSEGMENTS, "ALL")
       fl_to_select_ <- (which(all_the_fleets_mey == as.character(mat_cfg_scenario_settings_fore_options$MEY.fleet) )-1) 
       print(fl_to_select_)
       gtkComboBoxSetActive(bmt_combo_fleetsegments_MEY_r8, fl_to_select_)
       
       if (mat_cfg_scenario_settings_fore_options$MEY.effort_variable == "VESSELS")  {
            gtkToggleButtonSetActive(radio_scenario_r8_effort_vars_vessel, T) 
       } else {
            gtkToggleButtonSetActive(radio_scenario_r8_effort_vars_day, T)
       }
     gtkEntrySetText(bmt_entry_scenario_r8_years ,  mat_cfg_scenario_settings_fore_options$MEY.nb_years)
    
} else if ( BMT_SCENARIO == BMT_HR_STATUS_QUO) {       # 5
     gtkToggleButtonSetActive(bmt_chk_hr_statusquo, T)   
      
} else if (   BMT_SCENARIO == BMT_HR_STATUS_QUO_BEHAVIOURAL )  {    # 11 
       gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, T) 
             
} else if ( BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL )  {  # 8 
           gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, T)   
                gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, T)    
           
} else if (  BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL )  {      # 9 
                  gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, T)   
                gtkToggleButtonSetActive(bmt_chk_hr_change_effort, T)  
       
#set_effort_data_lists_fore()
#gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0)   
          
} else if (   BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL )  {   # 10
                 gtkToggleButtonSetActive(bmt_chk_hr_behavioural_module, T)   
                gtkToggleButtonSetActive(bmt_chk_hr_change_effort, T) 
                 gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, T)
                 
#set_effort_data_lists_fore()
#gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0)  
                                         
} else if (   BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT )  {      # 7
                gtkToggleButtonSetActive(bmt_chk_hr_change_effort, T) 
                 gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, T)   
                 
# set_effort_data_lists_fore()
#gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0)   

} else if (  BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY )  {    # 1
           gtkToggleButtonSetActive(bmt_chk_hr_change_selectivity, T) 
             
} else if (  BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT )  {   # 2  
           gtkToggleButtonSetActive(bmt_chk_hr_change_effort, T)
           
#set_effort_data_lists_fore()
#gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0)   
                        
} else if (  BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY )  {   # 3  
           gtkToggleButtonSetActive(bmt_chk_hr_change_f_by_fleet, T)
          
      gtkEntrySetText(bmt_entry_timespan_r5 ,  mat_cfg_scenario_settings_fore_options$HR3.timespan) 
             gtkEntrySetText(bmt_entry_reduction_r5 ,  mat_cfg_scenario_settings_fore_options$HR3.reduction) 
             
             input_table_r5 <<-  data.frame(mat_cfg_scenario_settings_fore_options$HR3.fleet_reductions)  
             input_table_r5[,1] <<- BMT_FLEETSEGMENTS
             input_table_r5[,2] <<- as.numeric(as.character( input_table_r5[,2]))
             input_table_r5[,3] <<- as.numeric(as.character( input_table_r5[,3]))
             input_table_r5[,4] <<- as.numeric(as.character( input_table_r5[,4]))
             colnames(input_table_r5) <<- c("fleet_segment", "Reduction scenario", "% vessel red.", "% days reduction")
             reload_input_table_r5()      
                   
} else if (  BMT_SCENARIO == BMT_HR_CHANGE_TOTAL_FISHMORTALITY )  {   # 4 
           gtkToggleButtonSetActive(bmt_chk_hr_change_total_f, T)
         
            gtkEntrySetText(bmt_entry_timespan_r6 ,  mat_cfg_scenario_settings_fore_options$HR4.timespan) 
             gtkEntrySetText(bmt_entry_reduction_r6 ,  mat_cfg_scenario_settings_fore_options$HR4.reduction)  
           
             input_table_r6 <<-  data.frame(mat_cfg_scenario_settings_fore_options$HR4.fleet_reductions)  
             input_table_r6[,1] <<- BMT_FLEETSEGMENTS
             input_table_r6[,2] <<- as.character( input_table_r6[,2])
             input_table_r6[,3] <<- as.numeric(as.character( input_table_r6[,3]))
             input_table_r6[,4] <<- as.numeric(as.character( input_table_r6[,4]))
             colnames(input_table_r6) <<- c("fleet_segment", "to be reduced", "% vessel red.", "% days reduction")
             reload_input_table_r6() 
             
             input_table_r6_species_settings <<-  data.frame(mat_cfg_scenario_settings_fore_options$HR4.species_settings)  
             input_table_r6_species_settings[,1] <<- BMT_SPECIES
             input_table_r6_species_settings[,2] <<- as.numeric(as.character( input_table_r6_species_settings[,2]))
             input_table_r6_species_settings[,3] <<- as.numeric(as.character( input_table_r6_species_settings[,3]))
             input_table_r6_species_settings[,4] <<- as.numeric(as.character( input_table_r6_species_settings[,4]))
             colnames(input_table_r6_species_settings) <<- c("species", "% landing/catch", "SRR model", "F setting", "SSR", "Average years")
             reload_input_table_r6_species_settings()     
                                            
                   
} else if (  BMT_SCENARIO == BMT_HR_TAC_VARIATION )  {   # 6  
           gtkToggleButtonSetActive(bmt_chk_hr_set_tac, T)
         
   gtkComboBoxSetActive(bmt_combo_optionTAC_r7, (which(c("Option 1", "Option 2", "Option 3") == as.character(paste("Option", mat_cfg_scenario_settings_fore_options$HR6.optionTAC)) )-1) )    
   gtkComboBoxSetActive(bmt_combo_TAC_species_r7, (which(BMT_SPECIES ==  BMT_SPECIES[mat_cfg_scenario_settings_fore_options$HR6.species] )-1) )
   
             input_table_r7 <<-  data.frame(mat_cfg_scenario_settings_fore_options$HR6.fleet_quotas)  
             input_table_r7[,1] <<- BMT_FLEETSEGMENTS
             input_table_r7[,2] <<- as.numeric(as.character( input_table_r7[,2]) )
             colnames(input_table_r7) <<- c("fleet_segment", "% for the quota splitting")   
               print(input_table_r7)
             reload_input_table_r7() 
             
             # update table indices     :   $HR6.abundance_indices                       
            # update table tac    :   $HR6.previous_tac             
}


#if (! (BMT_SCENARIO %in% c(BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL)) )   {
#
#EFFORT_NUMBER_list_fore <<- list(NULL)
#EFFORT_DAY_list_fore <<- list(NULL)
#      for (fl in 1:(length(BMT_FLEETSEGMENTS)-1) ) {
#            EFFORT_NUMBER_list_fore <<- c(EFFORT_NUMBER_list_fore, list(NULL))
#             EFFORT_DAY_list_fore <<- c(EFFORT_DAY_list_fore, list(NULL))
#      }
#
#}
if ( (BMT_SCENARIO %in% c(BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL)) )   {
set_effort_data_lists_fore()
gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0) 
}


}

