# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


vbox_economic_params_cost <- gtkVBox(FALSE, 5) 

lblVarCostFunction <<- gtkLabel() 
lblFixCostFunction <<- gtkLabel() 
lblCapCostFunction <<- gtkLabel() 

bmt_combo_varcost_models <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_varcost_models, "changed", setEconomicparams_varcost_settings)

for (item in VARCOST_MODELS$option_name) {
  bmt_combo_varcost_models$appendText(item)
}

bmt_hbox_combo_varcost_models  <- gtkVBox(F, 5) 
 bmt_hbox_combo_varcost_models$packStart(gtkLabel(" Simulation model ") , expand = F, F, 10)                                
bmt_hbox_combo_varcost_models$packStart(bmt_combo_varcost_models, expand = F, F, 0) 

bmt_hbox_combo_varcost_models$packStart(lblVarCostFunction, expand = F, F, 0) 



bmt_combo_fixedcost_models <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_fixedcost_models, "changed", setEconomicparams_fixedcost_settings)

for (item in FIXEDCOST_MODELS$option_name) {
  bmt_combo_fixedcost_models$appendText(item)
}

bmt_hbox_combo_fixedcost_models  <- gtkVBox(F, 5) 
 bmt_hbox_combo_fixedcost_models$packStart(gtkLabel(" Simulation model ") , expand = F, F, 10)                                
bmt_hbox_combo_fixedcost_models$packStart(bmt_combo_fixedcost_models, expand = F, F, 0)

bmt_hbox_combo_fixedcost_models$packStart(lblFixCostFunction, expand = F, F, 0) 


bmt_combo_capitalcost_models <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_capitalcost_models, "changed", setEconomicparams_capitalcost_settings)

for (item in CAPITALCOST_MODELS$option_name) {
  bmt_combo_capitalcost_models$appendText(item)
}

bmt_hbox_combo_capitalcost_models  <- gtkVBox(F, 5) 
 bmt_hbox_combo_capitalcost_models$packStart(gtkLabel(" Simulation model ") , expand = F, F, 10)                                
bmt_hbox_combo_capitalcost_models$packStart(bmt_combo_capitalcost_models, expand = F, F, 0)

bmt_hbox_combo_capitalcost_models$packStart(lblCapCostFunction, expand = F, F, 0) 




# --------------------------------------------------------------------------------- 1) cost_variable:	"Coefficient for variable costs"
bmt_vboxcost_variable <- gtkVBox(F, 5) 
bmt_hboxcost_variable  <- gtkHBox(F, 5) 

bmt_cost_variable.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_variable.sw$setShadowType("etched-in")
bmt_cost_variable.sw$setPolicy("automatic", "automatic")
bmt_cost_variable.sw$SetUsize(120, 80)  
bmt_cost_variable_list <<- list()
bmt_cost_variableIndex <<- 0
# create model
bmt_cost_variable.create_model()
# create tree view
bmt_cost_variable.treeview <<- gtkTreeViewNewWithModel(bmt_cost_variable.model)
bmt_cost_variable.treeview$setRulesHint(TRUE)
bmt_cost_variable.treeview$getSelection()$setMode("single")
bmt_cost_variable.add_columns(bmt_cost_variable.treeview)
bmt_cost_variable.sw$add(bmt_cost_variable.treeview) 


 # --------------------------------------------------------------------------------- 2)	cost_fuelprice:	"Time series for fuel price for the forecast period"

bmt_vboxcost_fuelprice <- gtkVBox(F, 5) 
bmt_hboxcost_fuelprice  <- gtkHBox(F, 5) 

bmt_cost_fuelprice.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_fuelprice.sw$setShadowType("etched-in")
bmt_cost_fuelprice.sw$setPolicy("automatic", "automatic")
bmt_cost_fuelprice.sw$SetUsize(120, 50)  
bmt_cost_fuelprice_list <<- list()
bmt_cost_fuelpriceIndex <<- 0
# create model
bmt_cost_fuelprice.create_model()
# create tree view
bmt_cost_fuelprice.treeview <<- gtkTreeViewNewWithModel(bmt_cost_fuelprice.model)
bmt_cost_fuelprice.treeview$setRulesHint(TRUE)
bmt_cost_fuelprice.treeview$getSelection()$setMode("single")
bmt_cost_fuelprice.add_columns(bmt_cost_fuelprice.treeview)
bmt_cost_fuelprice.sw$add(bmt_cost_fuelprice.treeview) 


lbl_cost_fuelprice <<- gtkLabel(" Time series of FUEL PRICE for the forecast period (fp)") 
lbl_cost_variable <<- gtkLabel(" Coefficient for VARIABLE costs ") 

bmt_vboxcost_variable_2tables <- gtkVBox(F, 5)    
bmt_vboxcost_variable_2tables$packStart(lbl_cost_variable , expand = F, F, 0)
bmt_vboxcost_variable_2tables$packStart(bmt_cost_variable.sw  , expand = F, F, 0)
bmt_vboxcost_variable_2tables$packStart(lbl_cost_fuelprice , expand = F, F, 0)
bmt_vboxcost_variable_2tables$packStart(bmt_cost_fuelprice.sw , expand = F, F, 0)


bmt_hboxcost_variable$packStart(bmt_hbox_combo_varcost_models , expand = F, F, 10) 
bmt_hboxcost_variable$packStart(bmt_vboxcost_variable_2tables, expand = T, T, 10) 

bmt_vboxcost_variable$packStart(bmt_hboxcost_variable , expand = F, T, 5)


# ------------------------------------------------------------------------------------ 8)	cost_fixed:	"Coefficient for fixed costs"
 
bmt_vboxcost_fixed <- gtkVBox(F, 5) 
bmt_hboxcost_fixed  <- gtkHBox(F, 5) 

bmt_cost_fixed.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_fixed.sw$setShadowType("etched-in")
bmt_cost_fixed.sw$setPolicy("automatic", "automatic")
bmt_cost_fixed.sw$SetUsize(120, 80)  
bmt_cost_fixed_list <<- list()
bmt_cost_fixedIndex <<- 0
# create model
bmt_cost_fixed.create_model()
# create tree view
bmt_cost_fixed.treeview <<- gtkTreeViewNewWithModel(bmt_cost_fixed.model)
bmt_cost_fixed.treeview$setRulesHint(TRUE)
bmt_cost_fixed.treeview$getSelection()$setMode("single")
bmt_cost_fixed.add_columns(bmt_cost_fixed.treeview)
bmt_cost_fixed.sw$add(bmt_cost_fixed.treeview) 

lbl_cost_fixed <<- gtkLabel(" Coefficient for FIXED costs ") 

bmt_vboxcost_fixed_table <- gtkVBox(F, 5)    
bmt_vboxcost_fixed_table$packStart(lbl_cost_fixed , expand = F, F, 0)
bmt_vboxcost_fixed_table$packStart(bmt_cost_fixed.sw  , expand = F, F, 0)

bmt_hboxcost_fixed$packStart(bmt_hbox_combo_fixedcost_models, expand = F, F, 10) 
bmt_hboxcost_fixed$packStart(bmt_vboxcost_fixed_table , expand = T, T, 10) 

bmt_vboxcost_fixed$packStart(bmt_hboxcost_fixed , expand = F, F, 5)


# ------------------------------------------------------------------------------------ 9)	cost_capital:	"Coefficient for capital costs"
 
bmt_vboxcost_capital <- gtkVBox(F, 5) 
bmt_hboxcost_capital  <- gtkHBox(F, 5) 

bmt_cost_capital.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_capital.sw$setShadowType("etched-in")
bmt_cost_capital.sw$setPolicy("automatic", "automatic")
bmt_cost_capital.sw$SetUsize(120, 80)  
bmt_cost_capital_list <<- list()
bmt_cost_capitalIndex <<- 0
# create model
bmt_cost_capital.create_model()
# create tree view
bmt_cost_capital.treeview <<- gtkTreeViewNewWithModel(bmt_cost_capital.model)
bmt_cost_capital.treeview$setRulesHint(TRUE)
bmt_cost_capital.treeview$getSelection()$setMode("single")
bmt_cost_capital.add_columns(bmt_cost_capital.treeview)
bmt_cost_capital.sw$add(bmt_cost_capital.treeview) 

 lbl_cost_capital <<- gtkLabel(" Coefficient for CAPITAL costs ") 
 
bmt_vboxcost_capital_table <- gtkVBox(F, 5)    
bmt_vboxcost_capital_table$packStart(lbl_cost_capital , expand = F, F, 0)
bmt_vboxcost_capital_table$packStart(bmt_cost_capital.sw  , expand = F, F, 0)

bmt_hboxcost_capital$packStart(bmt_hbox_combo_capitalcost_models , expand = F, T, 10) 
bmt_hboxcost_capital$packStart(bmt_vboxcost_capital_table , expand = T, T, 10)
 
bmt_vboxcost_capital$packStart(bmt_hboxcost_capital , expand = F, F, 5)




# ------------------------------------------------------------------------------------ 2)	indic_taxes:	"Additional taxes"
 
bmt_vboxcost_equipment <- gtkVBox(F, 5) 
bmt_hboxcost_equipment  <- gtkHBox(F, 5) 

bmt_cost_equipment.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_equipment.sw$setShadowType("etched-in")
bmt_cost_equipment.sw$setPolicy("automatic", "automatic")
bmt_cost_equipment.sw$SetUsize(120, 80)  
bmt_cost_equipment_list <<- list()
bmt_cost_equipmentIndex <<- 0
# create model
bmt_cost_equipment.create_model()
# create tree view
bmt_cost_equipment.treeview <<- gtkTreeViewNewWithModel(bmt_cost_equipment.model)
bmt_cost_equipment.treeview$setRulesHint(TRUE)
bmt_cost_equipment.treeview$getSelection()$setMode("single")
bmt_cost_equipment.add_columns(bmt_cost_equipment.treeview)
bmt_cost_equipment.sw$add(bmt_cost_equipment.treeview) 

 lbl_cost_equipment <<- gtkLabel(" New equipment costs ") 
 
bmt_vboxcost_equipment_table <- gtkVBox(F, 5)    
bmt_vboxcost_equipment_table$packStart(lbl_cost_equipment , expand = F, F, 0)
bmt_vboxcost_equipment_table$packStart(bmt_cost_equipment.sw  , expand = F, F, 0)

bmt_hboxcost_equipment$packStart(bmt_vboxcost_equipment_table, expand = T, T, 10) 
bmt_vboxcost_equipment$packStart(bmt_hboxcost_equipment , expand = F, F, 0)

# ---------------------------------------------------------------------------------------------------------



frame_economic_params_variablecosts <- gtkFrame(" VARIABLE costs ") 
 hbox_economic_params_variablecosts <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_variablecosts <- gtkVBox(FALSE, 5)        
 hbox_economic_params_variablecosts$packStart(frame_economic_params_variablecosts, expand = T, fill = T, padding = 10)                 
   vbox_economic_params_variablecosts$packStart(hbox_economic_params_variablecosts, expand = T, fill = T, padding = 0)                        
   frame_economic_params_variablecosts$add(bmt_vboxcost_variable)           

#
#
#   frame_economic_params_labourcosts <- gtkFrame(" LABOUR costs ") 
# hbox_economic_params_labourcosts <- gtkHBox(homogeneous = FALSE, 5)   
# vbox_economic_params_labourcosts <- gtkVBox(FALSE, 5)        
# hbox_economic_params_labourcosts$packStart(frame_economic_params_labourcosts, expand = T, fill = T, padding = 10)                 
#   vbox_economic_params_labourcosts$packStart(hbox_economic_params_labourcosts, expand = T, fill = T, padding = 5) 
#    frame_economic_params_labourcosts$add(bmt_vboxcost_crew_minwage)             

      frame_economic_params_fixedcosts <- gtkFrame(" FIXED costs ") 
 hbox_economic_params_fixedcosts <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_fixedcosts <- gtkVBox(FALSE, 5)        
 hbox_economic_params_fixedcosts$packStart(frame_economic_params_fixedcosts, expand = T, fill = T, padding = 10)                 
   vbox_economic_params_fixedcosts$packStart(hbox_economic_params_fixedcosts, expand = F, fill = F, padding = 5) 
 frame_economic_params_fixedcosts$add(bmt_vboxcost_fixed)           

       frame_economic_params_capitalcosts <- gtkFrame(" CAPITAL costs ") 
 hbox_economic_params_capitalcosts <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_capitalcosts <- gtkVBox(FALSE, 5)        
 hbox_economic_params_capitalcosts$packStart(frame_economic_params_capitalcosts, expand = T, fill = T, padding = 10)                 
   vbox_economic_params_capitalcosts$packStart(hbox_economic_params_capitalcosts, expand = F, fill = F, padding = 5) 
     frame_economic_params_capitalcosts$add(bmt_vboxcost_capital)       


    vbox_economic_params_cost$packStart(vbox_economic_params_variablecosts, expand = F, fill = F, padding = 0)    
           vbox_economic_params_cost$packStart(vbox_economic_params_fixedcosts, expand = F, fill = F, padding = 0)    
               vbox_economic_params_cost$packStart(vbox_economic_params_capitalcosts, expand = F, fill = F, padding = 0)   
#                      vbox_economic_params_cost$packStart(vbox_economic_params_labourcosts, expand = T, fill = T, padding = 10)     

vbox_economic_params_cost$packStart(bmt_vboxcost_equipment, expand = F, fill = F, padding = 0)   
