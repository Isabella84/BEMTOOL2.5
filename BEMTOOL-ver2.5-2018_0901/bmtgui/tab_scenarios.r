# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# initializating  objects

vbox_scenarios <- gtkVBox(FALSE, 5) 
hbox_scenarios <- gtkHBox(FALSE, 5) 

vbox_hr_statusquo <- gtkVBox(FALSE, 5) 
vbox_hr_behavioural_module <- gtkVBox(FALSE, 5) 
vbox_hr_change_selectivity <- gtkVBox(FALSE, 5) 
vbox_hr_change_effort <- gtkVBox(FALSE, 5) 
vbox_hr_change_f_by_fleet <- gtkVBox(FALSE, 5) 
vbox_hr_change_total_f <- gtkVBox(FALSE, 5) 
vbox_hr_set_tac <- gtkVBox(FALSE, 5)
vbox_hr_MEY <- gtkVBox(FALSE, 5) 
vbox_hr_effort_F <- gtkVBox(FALSE, 5) 

vbox_hr_statusquo_options <- gtkVBox(FALSE, 5) 
vbox_hr_behavioural_module_options <- gtkVBox(FALSE, 5) 
vbox_hr_change_selectivity_options <- gtkVBox(FALSE, 5)
vbox_hr_change_effort_options <- gtkVBox(FALSE, 5) 
vbox_hr_change_f_by_fleet_options <- gtkVBox(FALSE, 5) 
vbox_hr_change_total_f_options <- gtkVBox(FALSE, 5) 
vbox_hr_set_tac_options <- gtkVBox(FALSE, 5) 
vbox_hr_MEY_options <- gtkVBox(FALSE, 5) 
vbox_hr_effort_F_options <- gtkVBox(FALSE, 5) 

frame_scenario_statusquo <- gtkFrame(" Status quo ")  
frame_scenario_behavioural_module <- gtkFrame(" Behaviuoral module ")  
frame_scenario_change_selectivity <- gtkFrame(" Selectivity ")  
frame_scenario_change_effort <- gtkFrame(" Change of fishing effort ")  
frame_scenario_change_f_by_fleet <- gtkFrame(" Change of fishing mortality by fleet ")  
frame_scenario_change_total_f <- gtkFrame(" Change of total fishing mortality ")  
frame_scenario_set_tac <- gtkFrame(" Change\\introduction of TAC ")  
frame_scenario_MEY <- gtkFrame(" MEY calculation ") 
frame_scenario_effort_F <- gtkFrame(" Effort-F relationship ")   

hbox_scenario_statusquo <- gtkHBox(homogeneous = FALSE, 5) 
hbox_scenario_behavioural_module <- gtkHBox(homogeneous = FALSE, 5) 
hbox_scenario_change_selectivity <- gtkHBox(homogeneous = FALSE, 5)  
hbox_scenario_change_effort <- gtkHBox(homogeneous = FALSE, 5) 
hbox_scenario_change_f_by_fleet <- gtkHBox(homogeneous = FALSE, 5)
hbox_scenario_change_total_f <- gtkHBox(homogeneous = FALSE, 5)  
hbox_scenario_set_tac <- gtkHBox(homogeneous = FALSE, 5) 
hbox_scenario_MEY <- gtkHBox(homogeneous = FALSE, 5) 
hbox_scenario_effort_F <- gtkHBox(homogeneous = FALSE, 5) 

vbox_scenario_statusquo <- gtkVBox(FALSE, 5) 
vbox_scenario_behavioural_module <- gtkVBox(FALSE, 5)  
vbox_scenario_change_selectivity <- gtkVBox(FALSE, 5)   
vbox_scenario_change_effort <- gtkVBox(FALSE, 5) 
vbox_scenario_change_f_by_fleet <- gtkVBox(FALSE, 5) 
vbox_scenario_change_total_f <- gtkVBox(FALSE, 5)  
vbox_scenario_set_tac <- gtkVBox(FALSE, 5) 
vbox_scenario_MEY <- gtkVBox(FALSE, 5) 
vbox_scenario_effort_F <- gtkVBox(FALSE, 5) 


vbox_hr_statusquo_options$packStart(frame_scenario_statusquo, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_behavioural_module_options$packStart(frame_scenario_behavioural_module, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_change_selectivity_options$packStart(frame_scenario_change_selectivity, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_change_effort_options$packStart(frame_scenario_change_effort, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_change_f_by_fleet_options$packStart(frame_scenario_change_f_by_fleet, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_change_total_f_options$packStart(frame_scenario_change_total_f, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_set_tac_options$packStart(frame_scenario_set_tac, expand = FALSE, fill = FALSE, padding = 0)  
vbox_hr_MEY_options$packStart(frame_scenario_MEY, expand = FALSE, fill = FALSE, padding = 0)
vbox_hr_effort_F_options$packStart(frame_scenario_effort_F, expand = F, fill = T, padding = 0)    

vbox_scenario_statusquo$packStart(hbox_scenario_statusquo, expand = FALSE, fill = FALSE, padding = 5)  
vbox_scenario_behavioural_module$packStart(hbox_scenario_behavioural_module, expand = FALSE, fill = FALSE, padding = 5)  
vbox_scenario_change_selectivity$packStart(hbox_scenario_change_selectivity, expand = FALSE, fill = FALSE, padding = 5)    
vbox_scenario_change_effort$packStart(hbox_scenario_change_effort, expand = FALSE, fill = FALSE, padding = 5)   
vbox_scenario_change_f_by_fleet$packStart(hbox_scenario_change_f_by_fleet, expand = FALSE, fill = FALSE, padding = 5)  
vbox_scenario_change_total_f$packStart(hbox_scenario_change_total_f, expand = FALSE, fill = FALSE, padding = 5)   
vbox_scenario_set_tac$packStart(hbox_scenario_set_tac, expand = FALSE, fill = FALSE, padding = 5)  
vbox_scenario_MEY$packStart(hbox_scenario_MEY, expand = FALSE, fill = FALSE, padding = 5)  
vbox_scenario_effort_F$packStart(hbox_scenario_effort_F, expand = FALSE, fill = FALSE, padding = 5)  

frame_scenario_statusquo$add(vbox_hr_statusquo) 
frame_scenario_behavioural_module$add(vbox_hr_behavioural_module)
frame_scenario_change_selectivity$add(vbox_hr_change_selectivity)
frame_scenario_change_effort$add(vbox_hr_change_effort) 
frame_scenario_change_f_by_fleet$add(vbox_hr_change_f_by_fleet)
frame_scenario_change_total_f$add(vbox_hr_change_total_f)  
frame_scenario_set_tac$add(vbox_hr_set_tac)
frame_scenario_MEY$add(vbox_hr_MEY)
frame_scenario_effort_F$add(vbox_hr_effort_F)

bmt_chk_hr_statusquo <- gtkCheckButton("Status quo")
#gSignalConnect(bmt_chk_hr_statusquo, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_behavioural_module <- gtkCheckButton("Behavioural module")
gSignalConnect(bmt_chk_hr_behavioural_module, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_change_selectivity <- gtkCheckButton("Change in gear selectivity")
#gSignalConnect(bmt_chk_hr_change_selectivity, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_change_effort <- gtkCheckButton("Change fishing effort")
 gSignalConnect(bmt_chk_hr_change_effort, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_change_f_by_fleet <- gtkCheckButton("Change F by fleet segment")
 gSignalConnect(bmt_chk_hr_change_f_by_fleet, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_change_total_f <- gtkCheckButton("Change total F")
  gSignalConnect(bmt_chk_hr_change_total_f, "toggled", activate_deactivate_scenario_options)
bmt_chk_hr_set_tac <- gtkCheckButton("Variation of TAC")
 gSignalConnect(bmt_chk_hr_set_tac, "toggled", activate_deactivate_scenario_options)
 bmt_chk_hr_MEY <- gtkCheckButton("MEY calculation")
 gSignalConnect(bmt_chk_hr_MEY, "toggled", activate_deactivate_scenario_options)

  vbox_list_scenarios <- gtkVBox(FALSE, 5)
    vbox_all_options_scenarios <- gtkVBox(FALSE, 5)  

  hbox_scenario_name <- gtkHBox()
  
  bmt_entry_scenario_name <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_scenario_name, 15)

  hbox_scenario_name$packStart(gtkLabel("Scenario name") , expand = F, fill = F, padding = 5)
  hbox_scenario_name$packStart(bmt_entry_scenario_name , expand = F, fill = F, padding = 5)
 
   vbox_list_scenarios$packStart(hbox_scenario_name, expand = T, fill = T, padding = 5)            
  vbox_list_scenarios$packStart(bmt_chk_hr_statusquo, expand = T, fill = T, padding = 5)
    vbox_list_scenarios$packStart(bmt_chk_hr_behavioural_module, expand = T, fill = T, padding = 5)
      vbox_list_scenarios$packStart(bmt_chk_hr_change_selectivity, expand = T, fill = T, padding = 5)
        vbox_list_scenarios$packStart(bmt_chk_hr_change_effort, expand = T, fill = T, padding = 5)
          vbox_list_scenarios$packStart(bmt_chk_hr_change_f_by_fleet, expand = T, fill = T, padding = 5)
            vbox_list_scenarios$packStart(bmt_chk_hr_change_total_f, expand = T, fill = T, padding = 5)
              vbox_list_scenarios$packStart(bmt_chk_hr_set_tac, expand = T, fill = T, padding = 5)
                     vbox_list_scenarios$packStart(bmt_chk_hr_MEY, expand = T, fill = T, padding = 5)
                
  
  

  
bmt_tbl_scenarios <- gtkTable(7, 2, homogeneous = FALSE)
bmt_tbl_scenarios$SetRowSpacings(5)
bmt_tbl_scenarios$SetColSpacings(30)
bmt_tbl_scenarios$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_scenarios$Attach(hbox_scenario_name,i, i+1, j, j+1) 
j=j+1
bmt_tbl_scenarios$Attach(vbox_list_scenarios,i, i+1, j, j+1) 
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_behavioural_module,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_change_selectivity,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_change_effort,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_change_f_by_fleet,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_change_total_f,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(bmt_chk_hr_set_tac,i, i+1, j, j+1)
#

i=i+1  # column 1 
j=0
j=j+1 
bmt_tbl_scenarios$Attach(vbox_all_options_scenarios,i, i+1, j, j+1) 
#j=j+1
##bmt_tbl_scenarios$Attach(vbox_hr_behavioural_module_options,i, i+1, j, j+1)
#j=j+1
##bmt_tbl_scenarios$Attach(vbox_hr_change_selectivity_options,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(vbox_hr_change_effort_options,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(vbox_hr_change_f_by_fleet_options,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(vbox_hr_change_total_f_options,i, i+1, j, j+1)
#j=j+1
#bmt_tbl_scenarios$Attach(vbox_hr_set_tac_options,i, i+1, j, j+1)

hbox_all_options_scenarios_effort <- gtkHBox()
	hbox_all_options_scenarios_effort$packStart(vbox_hr_change_effort_options, expand = T, fill = T, padding = 0)
  	hbox_all_options_scenarios_effort$packStart(vbox_hr_effort_F_options, expand = T, fill = T, padding = 5)
      
	vbox_all_options_scenarios$packStart(hbox_all_options_scenarios_effort, expand = F, fill = F, padding = 0)
	#vbox_all_options_scenarios$packStart(vbox_hr_effort_F_options, expand = F, fill = F, padding = 0)
    vbox_all_options_scenarios$packStart(vbox_hr_change_f_by_fleet_options, expand = F, fill = F, padding = 0)
      vbox_all_options_scenarios$packStart(vbox_hr_change_total_f_options, expand = F, fill = F, padding = 0)
        vbox_all_options_scenarios$packStart(vbox_hr_set_tac_options, expand = F, fill = F, padding = 0)
               vbox_all_options_scenarios$packStart(vbox_hr_MEY_options, expand = F, fill = F, padding = 0)

hbox_tbl_scenarios <- gtkHBox()
hbox_tbl_scenarios$packStart(bmt_tbl_scenarios, expand = F, fill = T, padding = 10)

hbox_scenarios$packStart(hbox_tbl_scenarios, expand = T, fill = T, padding = 10)
vbox_scenarios$packStart(hbox_scenarios, expand = F, fill = F, padding = 0)

vbox_hr_statusquo_details <- gtkVBox(FALSE, 5)
hbox_hr_statusquo_details <- gtkHBox(FALSE, 5)
vbox_hr_statusquo_details$packStart(hbox_hr_statusquo_details, padding = 0)
hbox_hr_statusquo_details$packStart(gtkLabel("No input required"), padding = 10)

vbox_hr_behavioural_details <- gtkVBox(FALSE, 5)
hbox_hr_behavioural_details <- gtkHBox(FALSE, 5)
vbox_hr_behavioural_details$packStart(hbox_hr_behavioural_details, padding = 0)
hbox_hr_behavioural_details$packStart(gtkLabel("No input required"), padding = 10)

vbox_hr_change_selectivity_details <- gtkVBox(FALSE, 5)
hbox_hr_change_selectivity_details <- gtkHBox(FALSE, 5)
vbox_hr_change_selectivity_details$packStart(hbox_hr_change_selectivity_details, padding = 0)
hbox_hr_change_selectivity_details$packStart(gtkLabel("No input required"), padding = 10)

#vbox_hr_statusquo$packStart(vbox_hr_statusquo_details, padding = 2)
#vbox_hr_behavioural_module$packStart(vbox_hr_behavioural_details, padding = 2)
#vbox_hr_change_selectivity$packStart(vbox_hr_change_selectivity_details, padding = 2)

suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_change_effort.r", sep="")))
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_effort_F.r", sep="")))
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_change_f_byfleet.r", sep="")))		
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_change_total_f.r", sep="")))		
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_set_tac.r", sep="")))
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_MEY.r", sep="")))

gtkWidgetSetSensitive(vbox_hr_change_effort_options, F)
#gtkWidgetSetSensitive(vbox_hr_effort_F_options, F)		
gtkWidgetSetSensitive(vbox_hr_change_f_by_fleet_options, F)
gtkWidgetSetSensitive(vbox_hr_change_total_f_options, F)
gtkWidgetSetSensitive(vbox_hr_set_tac_options, F)
gtkWidgetSetSensitive(vbox_hr_MEY_options, F)				
