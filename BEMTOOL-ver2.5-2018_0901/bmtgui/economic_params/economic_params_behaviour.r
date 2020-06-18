# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


vbox_economic_params_behaviour <- gtkVBox(FALSE, 5) 

# ------------------------------------------------------------------------------------ 1)	behav_dyn:	"Coefficients for fleet dynamics"
 
bmt_vboxbehav_dyn <- gtkVBox(F, 5) 
bmt_hboxbehav_dyn  <- gtkHBox(F, 5) 

bmt_behav_dyn.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_behav_dyn.sw$setShadowType("etched-in")
bmt_behav_dyn.sw$setPolicy("automatic", "automatic")
bmt_behav_dyn.sw$SetUsize(120, 90)  
bmt_behav_dyn_list <<- list()
bmt_behav_dynIndex <<- 0
# create model
bmt_behav_dyn.create_model()
# create tree view
bmt_behav_dyn.treeview <<- gtkTreeViewNewWithModel(bmt_behav_dyn.model)
bmt_behav_dyn.treeview$setRulesHint(TRUE)
bmt_behav_dyn.treeview$getSelection()$setMode("single")
bmt_behav_dyn.add_columns(bmt_behav_dyn.treeview)
bmt_behav_dyn.sw$add(bmt_behav_dyn.treeview) 

 chk_behav_dyn <<- gtkCheckButton(" Activate fleet DYNAMICS ")
 
bmt_vboxbehav_dyn_table <- gtkVBox(F, 5)    
bmt_vboxbehav_dyn_table$packStart(chk_behav_dyn , expand = F, T, 10)
bmt_vboxbehav_dyn_table$packStart(bmt_behav_dyn.sw  , expand = F, T, 0)

bmt_vboxbehav_dyn$packStart(bmt_vboxbehav_dyn_table , expand = T, T, 0)




# ------------------------------------------------------------------------------------ 2)	behav_ACT:	"Coefficients for fleet ACTIVITY"
 
bmt_vboxbehav_act <- gtkVBox(F, 5) 
bmt_hboxbehav_act  <- gtkHBox(F, 5) 

bmt_behav_act.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_behav_act.sw$setShadowType("etched-in")
bmt_behav_act.sw$setPolicy("automatic", "automatic")
bmt_behav_act.sw$SetUsize(120, 90)  
bmt_behav_act_list <<- list()
bmt_behav_actIndex <<- 0
# create model
bmt_behav_act.create_model()
# create tree view
bmt_behav_act.treeview <<- gtkTreeViewNewWithModel(bmt_behav_act.model)
bmt_behav_act.treeview$setRulesHint(TRUE)
bmt_behav_act.treeview$getSelection()$setMode("single")
bmt_behav_act.add_columns(bmt_behav_act.treeview)
bmt_behav_act.sw$add(bmt_behav_act.treeview) 

chk_behav_act <<- gtkCheckButton(" Activate fleet ACTIVITY ") 
 
bmt_vboxbehav_act_table <- gtkVBox(F, 5)    
bmt_vboxbehav_act_table$packStart(chk_behav_act , expand = F, T, 10)
bmt_vboxbehav_act_table$packStart(bmt_behav_act.sw  , expand = F, T, 0)

bmt_vboxbehav_act$packStart(bmt_vboxbehav_act_table , expand = T, T, 0)



# ------------------------------------------------------------------------------------ 3)	behav_PROGR:	"Coefficients for Technological Progress"
 
bmt_vboxbehav_progr <- gtkVBox(F, 5) 
bmt_hboxbehav_progr  <- gtkHBox(F, 5) 

bmt_behav_progr.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_behav_progr.sw$setShadowType("etched-in")
bmt_behav_progr.sw$setPolicy("automatic", "automatic")
bmt_behav_progr.sw$SetUsize(120, 90)  
bmt_behav_progr_list <<- list()
bmt_behav_progrIndex <<- 0
# create model
bmt_behav_progr.create_model()
# create tree view
bmt_behav_progr.treeview <<- gtkTreeViewNewWithModel(bmt_behav_progr.model)
bmt_behav_progr.treeview$setRulesHint(TRUE)
bmt_behav_progr.treeview$getSelection()$setMode("single")
bmt_behav_progr.add_columns(bmt_behav_progr.treeview)
bmt_behav_progr.sw$add(bmt_behav_progr.treeview) 

 chk_behav_progr <<- gtkCheckButton(" Activate Technological Progress ")
 
bmt_vboxbehav_progr_table <- gtkVBox(F, 5)    
bmt_vboxbehav_progr_table$packStart(chk_behav_progr , expand = F, T, 10)
bmt_vboxbehav_progr_table$packStart(bmt_behav_progr.sw  , expand = F, T, 0)

bmt_vboxbehav_progr$packStart(bmt_vboxbehav_progr_table , expand = T, T, 0)

# ---------------------------------------------------------------------------------------------------------


hbox_economic_params_behaviour_container <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_params_behaviour_container <- gtkVBox(FALSE, 5)        
vbox_economic_params_behaviour_container$packStart(bmt_vboxbehav_dyn, expand = F, fill = F, padding = 10)   
vbox_economic_params_behaviour_container$packStart(bmt_vboxbehav_act, expand = F, fill = F, padding = 10)  
vbox_economic_params_behaviour_container$packStart(bmt_vboxbehav_progr, expand = F, fill = F, padding = 10)                
hbox_economic_params_behaviour_container$packStart(vbox_economic_params_behaviour_container, expand = T, fill = T, padding = 10) 

vbox_economic_params_behaviour$packStart(hbox_economic_params_behaviour_container, expand = T, fill = T, padding = 10)    

gSignalConnect(chk_behav_dyn, "toggled", activate_deactivate_fleetDYN)
gSignalConnect(chk_behav_act, "toggled", activate_deactivate_fleetACT)
gSignalConnect(chk_behav_progr, "toggled", activate_deactivate_techPROGR)

gtkToggleButtonSetActive(chk_behav_dyn, T)
gtkToggleButtonSetActive(chk_behav_act, T)
gtkToggleButtonSetActive(chk_behav_progr, T)

