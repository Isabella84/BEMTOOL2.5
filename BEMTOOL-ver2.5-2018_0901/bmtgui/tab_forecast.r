# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# initializating  objects
bmt_forecast_executed_scenarios <- gtkComboBoxNewText()
gSignalConnect(bmt_forecast_executed_scenarios, "changed", load_selected_scenario_results)               
 
vbox_forecast <<- gtkVBox(FALSE, 5) 


hbox_forecast_choose_scenario <- gtkHBox(FALSE, 5) 

 hbox_forecast <- gtkHBox(homogeneous = FALSE, 5)  
 

 hbox_forecast_choose_scenario$packStart(gtkLabel("Select a scenario"), expand = F, fill = F, padding = 10)                 
 hbox_forecast_choose_scenario$packStart(bmt_forecast_executed_scenarios, expand = F, fill = F, padding = 10)                 
                                                
                                                   
btn_runEvaluation_bmt <- gtkButton()
gtkButtonSetLabel(btn_runEvaluation_bmt, "      Run evaluation graphs    ")
btn_runEvaluation_bmt$AddCallback("clicked", runEvaluation)
hbox_forecast_choose_scenario$packStart(btn_runEvaluation_bmt, FALSE, FALSE, 10)

btn_runTrafficLights_bmt <- gtkButton()
gtkButtonSetLabel(btn_runTrafficLights_bmt, "      Run traffic lights    ")
btn_runTrafficLights_bmt$AddCallback("clicked", runTrafficLights)
hbox_forecast_choose_scenario$packStart(btn_runTrafficLights_bmt, FALSE, FALSE, 10)                                                      
                                                   
                                                   
btn_goMCDA_bmt <- gtkButton()
gtkButtonSetLabel(btn_goMCDA_bmt, "      Go to MCDA...    ")
btn_goMCDA_bmt$AddCallback("clicked", goMCDA)
hbox_forecast_choose_scenario$packStart(btn_goMCDA_bmt, FALSE, FALSE, 10) 

  vbox_forecast$packStart(hbox_forecast_choose_scenario, expand = F, fill = F, padding = 5)  
 
 
# hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
vbox_container_fore <- gtkVBox(FALSE, 5)                             
# ----------------------------  vbox_container_fore$packStart(hbox_forecast, expand = T, fill = T, padding = 0)  
vbox_forecast$packStart(vbox_container_fore, expand = T, fill = T, padding = 0)           
