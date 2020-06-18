# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# aggiungere tutto a vbox_hr_change_effort




# ---------------------------------------------------------------------------------

vbox_scenario_r8_fleet_combo <- gtkHBox()
vbox_scenario_r8_fleet_combo$packStart(gtkLabel("Involved fleet segments"), expand = FALSE, fill = FALSE, padding = 0)
vbox_scenario_r8_fleet_combo$packStart(bmt_combo_fleetsegments_MEY_r8, expand = FALSE, fill = FALSE, padding = 3)

vbox_scenario_r8_effort_vars <- gtkHBox()
vbox_scenario_r8_effort_vars$packStart(gtkLabel("Effort variables"), expand = FALSE, fill = FALSE, padding = 0)

   radio_scenario_r8_effort_vars_vessel <- gtkRadioButton()
#   gSignalConnect(radio_scenario_r8_effort_vars_vessel, "toggled", activate_input_sorting)
radio_scenario_r8_effort_vars_vessel$add(gtkLabel("Vessel"))
radio_scenario_r8_effort_vars_day <- gtkRadioButtonNewWithLabelFromWidget(radio_scenario_r8_effort_vars_vessel, "Day")
#   gSignalConnect(radio_sorting_fromVector, "toggled", activate_input_sorting)

vbox_scenario_r8_effort_vars$packStart(radio_scenario_r8_effort_vars_vessel, expand = FALSE, fill = FALSE, padding = 3)
vbox_scenario_r8_effort_vars$packStart(radio_scenario_r8_effort_vars_day, expand = FALSE, fill = FALSE, padding = 3)

vbox_scenario_r8_years <- gtkHBox()
vbox_scenario_r8_years$packStart(gtkLabel("Number of years"), expand = FALSE, fill = FALSE, padding = 0)

bmt_entry_scenario_r8_years <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_scenario_r8_years, NUMERICAL_ENTRY_LENGTH)

vbox_scenario_r8_years$packStart(bmt_entry_scenario_r8_years, expand = FALSE, fill = FALSE, padding = 5)

hbox_scenario_r8 <- gtkHBox()
hbox_scenario_r8$packStart(vbox_scenario_r8_fleet_combo , expand=F, F, 5) 
hbox_scenario_r8$packStart(vbox_scenario_r8_effort_vars , expand=F, F, 15) 
hbox_scenario_r8$packStart(vbox_scenario_r8_years , expand=F, F, 15) 

  
vbox_hr_MEY$packStart(hbox_scenario_r8 , expand = T, fill = FALSE, padding = 5)
 




  

  
