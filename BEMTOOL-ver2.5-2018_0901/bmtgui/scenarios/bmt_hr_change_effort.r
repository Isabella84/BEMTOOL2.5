# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# aggiungere tutto a vbox_hr_change_effort

hbox_scenarios_r4 <- gtkHBox(FALSE, 5)

hbox_choose_fleet_r4 <- gtkHBox(FALSE, 5)


bmt_button_load_effortdata_r4 <- gtkButtonNewWithLabel(" Load effort data... ")
bmt_button_load_effortdata_r4$AddCallback("clicked", bmt_loadEffortData_fore)

bmt_button_savechanges_fleet_r4 <- gtkButtonNewWithLabel(" Apply and save... ")
bmt_button_savechanges_fleet_r4$AddCallback("clicked", setEffortLanding_fleet_settings_fore)

gSignalConnect(bmt_combo_fleetsegments_effort_r4, "changed", bmt_reload_fleetsegment_info_fore)

#if (!is.null(BMT_FLEETSEGMENTS)) {
#for (item in BMT_FLEETSEGMENTS) {
#  bmt_combo_fleetsegments_effort_r4$appendText(item)
#}
#}

hbox_reduction_mode_r4 <- gtkHBox(FALSE, 5)

bmt_entry_timespan_r4 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_timespan_r4, NUMERICAL_ENTRY_LENGTH)

bmt_entry_reduction_r4 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_reduction_r4, NUMERICAL_ENTRY_LENGTH)

hbox_reduction_mode_r4$packStart(gtkLabel("Time span"), expand = FALSE, fill = FALSE, padding = 5)
hbox_reduction_mode_r4$packStart(bmt_entry_timespan_r4, expand = FALSE, fill = FALSE, padding = 5)
hbox_reduction_mode_r4$packStart(gtkLabel("% reduction"), expand = FALSE, fill = FALSE, padding = 5)
hbox_reduction_mode_r4$packStart(bmt_entry_reduction_r4, expand = FALSE, fill = FALSE, padding = 5)
bmt_button_automatic_reduction_r4 <- gtkButtonNewWithLabel(" Apply reduction ")
bmt_button_automatic_reduction_r4$AddCallback("clicked", bmt_applyAutomaticReduction)
hbox_reduction_mode_r4$packStart(bmt_button_automatic_reduction_r4, expand = FALSE, fill = FALSE, padding = 5)

bmt_button_automatic_reduction_r4_reset <- gtkButtonNewWithLabel(" Reset reduction ")
bmt_button_automatic_reduction_r4_reset$AddCallback("clicked", bmt_resetAutomaticReduction)
hbox_reduction_mode_r4$packStart(bmt_button_automatic_reduction_r4_reset, expand = FALSE, fill = FALSE, padding = 5)

bmt_chk_hr_reduction_mode_r4 <- gtkCheckButton("Set % Reduction")
gSignalConnect(bmt_chk_hr_reduction_mode_r4, "toggled", activate_deactivate_reduction_mode_r4)

hbox_choose_fleet_r4_reduction_box <- gtkHBox()
hbox_choose_fleet_r4_reduction_box$packStart(bmt_chk_hr_reduction_mode_r4, expand = FALSE, fill = FALSE, padding = 15)
hbox_choose_fleet_r4_reduction_box$packStart(hbox_reduction_mode_r4, expand = FALSE, fill = FALSE, padding = 5)

hbox_choose_fleet_r4$packStart(bmt_combo_fleetsegments_effort_r4, expand = FALSE, fill = FALSE, padding = 10)
hbox_choose_fleet_r4$packStart(bmt_button_load_effortdata_r4, expand = FALSE, fill = FALSE, padding = 15)
hbox_choose_fleet_r4$packStart(bmt_button_savechanges_fleet_r4, expand = FALSE, fill = FALSE, padding = 5)

   gtkWidgetSetSensitive(hbox_reduction_mode_r4, F)
   gtkWidgetSetSensitive(bmt_button_load_effortdata_r4, T)



# --------------------------- number table
bmt_NUMBER_r4.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_NUMBER_r4.sw$setShadowType("etched-in")
bmt_NUMBER_r4.sw$setPolicy("automatic", "automatic")
bmt_NUMBER_r4.sw$SetUsize(400, 70)  
bmt_NUMBER_r4_list <<- list()
bmt_NUMBER_r4Index <<- 0
# create model
if (!is.null(BMT_YEARS_FORECAST))  {
bmt_NUMBER_r4.create_model()
# create tree view
bmt_NUMBER_r4.treeview <<- gtkTreeViewNewWithModel(bmt_NUMBER_r4.model)
bmt_NUMBER_r4.treeview$setRulesHint(TRUE)
bmt_NUMBER_r4.treeview$getSelection()$setMode("single")
bmt_NUMBER_r4.add_columns(bmt_NUMBER_r4.treeview)
bmt_NUMBER_r4.sw$add(bmt_NUMBER_r4.treeview)  
}  


# --------------------------- DAY table
bmt_DAY_r4.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_DAY_r4.sw$setShadowType("etched-in")
bmt_DAY_r4.sw$setPolicy("automatic", "automatic")
bmt_DAY_r4.sw$SetUsize(400, 70)  
bmt_DAY_r4_list <<- list()
bmt_DAY_r4Index <<- 0
# create model
if (!is.null(BMT_YEARS_FORECAST))  {
bmt_DAY_r4.create_model()
# create tree view
bmt_DAY_r4.treeview <<- gtkTreeViewNewWithModel(bmt_DAY_r4.model)
bmt_DAY_r4.treeview$setRulesHint(TRUE)
bmt_DAY_r4.treeview$getSelection()$setMode("single")
bmt_DAY_r4.add_columns(bmt_DAY_r4.treeview)
bmt_DAY_r4.sw$add(bmt_DAY_r4.treeview) 
} 
  
 
 BMTnotebook_effort_r4 <<- gtkNotebook()
BMTnotebook_effort_r4$setTabPos("top")
  
  
BMTnotebook_effort_r4$appendPage(bmt_NUMBER_r4.sw, gtkLabel(str=" Monthly fishing vessels "))
BMTnotebook_effort_r4$appendPage(bmt_DAY_r4.sw, gtkLabel(str=" Monthly average Days at sea "))
  
   # hbox_scenarios_r4$packStart(gtkLabel("Monthly VESSELS"), expand = T, fill = T, padding = 5) 
#  hbox_scenarios_r4$packStart( bmt_NUMBER_r4.sw, expand = T, fill = T, padding = 5)
#      hbox_scenarios_r4$packStart(gtkLabel("Monthly DAYS"), expand = T, fill = T, padding = 5)  
#    hbox_scenarios_r4$packStart(bmt_DAY_r4.sw, expand = T, fill = T, padding = 5)     
#                                     
         vbox_hr_change_effort$packStart(hbox_choose_fleet_r4, expand = FALSE, fill = FALSE, padding = 3)   
         vbox_hr_change_effort$packStart(hbox_choose_fleet_r4_reduction_box, expand = FALSE, fill = FALSE, padding = 0)   
                               
    hbox_scenarios_r4$packStart(BMTnotebook_effort_r4, expand = T, fill = T, padding = 3)       
        vbox_hr_change_effort$packStart(hbox_scenarios_r4, expand = FALSE, fill = FALSE, padding = 5)   

  
