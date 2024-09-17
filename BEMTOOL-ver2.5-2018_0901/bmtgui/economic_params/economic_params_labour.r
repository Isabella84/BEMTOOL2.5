# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


vbox_economic_params_labour_cost <- gtkVBox(FALSE, 5) 

# ------------------------------------------------------------------------------------ 3)	cost_crew_minwage:	"Crew share" +  "Min national wage"
 
bmt_vboxcost_crew_minwage <- gtkVBox(F, 5) 
bmt_hboxcost_crew_minwage  <- gtkHBox(F, 5) 

bmt_cost_crew_minwage.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_cost_crew_minwage.sw$setShadowType("etched-in")
bmt_cost_crew_minwage.sw$setPolicy("automatic", "automatic")
bmt_cost_crew_minwage.sw$SetUsize(120, 80)  
bmt_cost_crew_minwage_list <<- list()
bmt_cost_crew_minwageIndex <<- 0
# create model
bmt_cost_crew_minwage.create_model()
# create tree view
bmt_cost_crew_minwage.treeview <<- gtkTreeViewNewWithModel(bmt_cost_crew_minwage.model)
bmt_cost_crew_minwage.treeview$setRulesHint(TRUE)
bmt_cost_crew_minwage.treeview$getSelection()$setMode("single")
bmt_cost_crew_minwage.add_columns(bmt_cost_crew_minwage.treeview)
bmt_cost_crew_minwage.sw$add(bmt_cost_crew_minwage.treeview) 

bmt_hboxcost_crew_minwage_table <- gtkHBox(F, 5)
bmt_hboxcost_crew_minwage_table$packStart(bmt_cost_crew_minwage.sw  , expand = T, T, 5)

bmt_vboxcost_crew_minwage_table <- gtkVBox(F, 5)    
bmt_vboxcost_crew_minwage_table$packStart(bmt_hboxcost_crew_minwage_table  , expand = F, F, 5)
bmt_vboxcost_crew_minwage$packStart(bmt_vboxcost_crew_minwage_table , expand = F, F, 0)


# ------------------------------------------------------------------------------------ 4)	labour_fuel:	"fuel costs"
 
bmt_vboxlabour_fuel <- gtkVBox(F, 5) 
bmt_hboxlabour_fuel  <- gtkHBox(F, 5) 

bmt_labour_fuel.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_labour_fuel.sw$setShadowType("etched-in")
bmt_labour_fuel.sw$setPolicy("automatic", "automatic")
bmt_labour_fuel.sw$SetUsize(600, 80)  
bmt_labour_fuel_list <<- list()
bmt_labour_fuelIndex <<- 0
# create model
bmt_labour_fuel.create_model()
# create tree view
bmt_labour_fuel.treeview <<- gtkTreeViewNewWithModel(bmt_labour_fuel.model)
bmt_labour_fuel.treeview$setRulesHint(TRUE)
bmt_labour_fuel.treeview$getSelection()$setMode("single")
bmt_labour_fuel.add_columns(bmt_labour_fuel.treeview)
bmt_labour_fuel.sw$add(bmt_labour_fuel.treeview) 

lbl_labour_fuel <<- gtkLabel(" Fuel costs ") 
 
bmt_vboxlabour_fuel_table <- gtkVBox(F, 5)    
bmt_vboxlabour_fuel_table$packStart(lbl_labour_fuel , expand = F, F, 0)
bmt_vboxlabour_fuel_table$packStart(bmt_labour_fuel.sw  , expand = F, F, 0)

bmt_vboxlabour_fuel$packStart(bmt_vboxlabour_fuel_table , expand = F, F, 0)



# ------------------------------------------------------------------------------------ 5)	labour_commercial:	"commercial costs"
 
bmt_vboxlabour_commercial <- gtkVBox(F, 5) 
bmt_hboxlabour_commercial <- gtkHBox(F, 5) 

bmt_labour_commercial.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_labour_commercial.sw$setShadowType("etched-in")
bmt_labour_commercial.sw$setPolicy("automatic", "automatic")
bmt_labour_commercial.sw$SetUsize(600, 80)  
bmt_labour_commercial_list <<- list()
bmt_labour_commercialIndex <<- 0
# create model
bmt_labour_commercial.create_model()
# create tree view
bmt_labour_commercial.treeview <<- gtkTreeViewNewWithModel(bmt_labour_commercial.model)
bmt_labour_commercial.treeview$setRulesHint(TRUE)
bmt_labour_commercial.treeview$getSelection()$setMode("single")
bmt_labour_commercial.add_columns(bmt_labour_commercial.treeview)
bmt_labour_commercial.sw$add(bmt_labour_commercial.treeview) 

lbl_labour_commercial <<- gtkLabel(" Commercial costs ") 
 
bmt_vboxlabour_commercial_table <- gtkVBox(F, 5)    
bmt_vboxlabour_commercial_table$packStart(lbl_labour_commercial , expand = F, F, 0)
bmt_vboxlabour_commercial_table$packStart(bmt_labour_commercial.sw  , expand = F, F, 0)

bmt_vboxlabour_commercial$packStart(bmt_vboxlabour_commercial_table , expand = F, F, 0)


# ------------------------------------------------------------------------------------ 6)	labour_others:	"other costs"
 
bmt_vboxlabour_others <- gtkVBox(F, 5) 
bmt_hboxlabour_others <- gtkHBox(F, 5) 

bmt_labour_others.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_labour_others.sw$setShadowType("etched-in")
bmt_labour_others.sw$setPolicy("automatic", "automatic")
bmt_labour_others.sw$SetUsize(600, 80)  
bmt_labour_others_list <<- list()
bmt_labour_othersIndex <<- 0
# create model
bmt_labour_others.create_model()
# create tree view
bmt_labour_others.treeview <<- gtkTreeViewNewWithModel(bmt_labour_others.model)
bmt_labour_others.treeview$setRulesHint(TRUE)
bmt_labour_others.treeview$getSelection()$setMode("single")
bmt_labour_others.add_columns(bmt_labour_others.treeview)
bmt_labour_others.sw$add(bmt_labour_others.treeview) 

 lbl_labour_others <<- gtkLabel(" Other variable costs ") 
 
bmt_vboxlabour_others_table <- gtkVBox(F, 5)    
bmt_vboxlabour_others_table$packStart(lbl_labour_others , expand = F, F, 0)
bmt_vboxlabour_others_table$packStart(bmt_labour_others.sw  , expand = F, F, 0)

bmt_vboxlabour_others$packStart(bmt_vboxlabour_others_table , expand = F, F, 0)





# ---------------------------------------------------------------------------------------------------------


#   frame_economic_params_labourcosts <- gtkFrame(" LABOUR costs ") 
hbox_economic_cost_crew_minwage_container <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_cost_crew_minwage_container <- gtkVBox(FALSE, 5)        
hbox_economic_cost_crew_minwage_container$packStart(bmt_vboxcost_crew_minwage, expand = T, fill = T, padding = 10)                 
vbox_economic_cost_crew_minwage_container$packStart(hbox_economic_cost_crew_minwage_container, expand = T, fill = T, padding = 0) 

# arrangement in GUI

bmt_tbl_fuelcommothers <- gtkTable(3,1,homogeneous = FALSE)
bmt_tbl_fuelcommothers$SetRowSpacings(5)
bmt_tbl_fuelcommothers$SetColSpacings(30)
bmt_tbl_fuelcommothers$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_fuelcommothers$Attach(bmt_vboxlabour_fuel,i, i+1, j, j+1) 
j=j+1 
bmt_tbl_fuelcommothers$Attach(bmt_vboxlabour_commercial,i, i+1, j, j+1) 
j=j+1 
bmt_tbl_fuelcommothers$Attach(bmt_vboxlabour_others,i, i+1, j, j+1)
 

hbox_economic_labour_fuelcommothers_container <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_labour_fuelcommothers_container <- gtkVBox(FALSE, 5)        
hbox_economic_labour_fuelcommothers_container$packStart(bmt_tbl_fuelcommothers, expand = T, fill = T, padding = 10)                 
vbox_economic_labour_fuelcommothers_container$packStart(hbox_economic_labour_fuelcommothers_container, expand = F, fill = F, padding = 0) 




# ------------------------------------------------------------------------------------ 4)	discard cost coefficient:	"Discard cost coefficient"
 
bmt_vboxdiscard_cost <- gtkVBox(F, 5) 
bmt_hboxdiscard_cost  <- gtkHBox(F, 5) 

bmt_discard_cost.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_discard_cost.sw$setShadowType("etched-in")
bmt_discard_cost.sw$setPolicy("automatic", "automatic")
bmt_discard_cost.sw$SetUsize(120, 150)  
bmt_discard_cost_list <<- list()
bmt_discard_costIndex <<- 0
# create model
bmt_discard_cost.create_model()
# create tree view
bmt_discard_cost.treeview <<- gtkTreeViewNewWithModel(bmt_discard_cost.model)
bmt_discard_cost.treeview$setRulesHint(TRUE)
bmt_discard_cost.treeview$getSelection()$setMode("single")
bmt_discard_cost.add_columns(bmt_discard_cost.treeview)
bmt_discard_cost.sw$add(bmt_discard_cost.treeview) 

 lbl_discard_cost <<- gtkLabel(" Discard cost coefficient ") 
 
bmt_vboxdiscard_cost_table <- gtkVBox(F, 5)    
bmt_vboxdiscard_cost_table$packStart(lbl_discard_cost , expand = F, F, 0)
bmt_vboxdiscard_cost_table$packStart(bmt_discard_cost.sw  , expand = F, F, 0)

bmt_hboxdiscard_cost$packStart(bmt_vboxdiscard_cost_table, expand = T, T, 10) 
bmt_vboxdiscard_cost$packStart(bmt_hboxdiscard_cost , expand = F, F, 0)






## ------------------------------------------------------------------------------------ 3)	labour_sorting:	"Sorting coefficient" 
# 
bmt_vboxlabour_sorting <- gtkVBox(F, 5) 
bmt_hboxlabour_sorting  <- gtkHBox(F, 5) 
#
#bmt_labour_sorting.sw <<- gtkScrolledWindowNew(NULL, NULL)
#bmt_labour_sorting.sw$setShadowType("etched-in")
#bmt_labour_sorting.sw$setPolicy("automatic", "automatic")
#bmt_labour_sorting.sw$SetUsize(120, 65)  
#bmt_labour_sorting_list <<- list()
#bmt_labour_sortingIndex <<- 0
## create model
#bmt_labour_sorting.create_model()
## create tree view
#bmt_labour_sorting.treeview <<- gtkTreeViewNewWithModel(bmt_labour_sorting.model)
#bmt_labour_sorting.treeview$setRulesHint(TRUE)
#bmt_labour_sorting.treeview$getSelection()$setMode("single")
#bmt_labour_sorting.add_columns(bmt_labour_sorting.treeview)
#bmt_labour_sorting.sw$add(bmt_labour_sorting.treeview) 
#
## lbl_labour_sorting <<- gtkLabel(" Sorting coefficients ") 
#bmt_vboxlabour_sorting_table <- gtkVBox(F, 5)    
## bmt_vboxlabour_sorting_table$packStart(lbl_labour_sorting , expand = F, T, 10)
#bmt_vboxlabour_sorting_table$packStart(bmt_labour_sorting.sw  , expand = F, F, 5)

# -------------------------------------------------------------------------------------- blocco etichetta + tabella           --> bmt_vboxlabour_sorting_table
 
hbox_economic_params_setsorting <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_params_setsorting <- gtkVBox(homogeneous = FALSE, 5) 
  
bmt_chk_economic_labour_sorting_coeff <- gtkCheckButton("Use sorting coefficient")
gSignalConnect(bmt_chk_economic_labour_sorting_coeff, "toggled", deactivate_economic_labour_sorting)
hbox_economic_params_setsorting$packStart(bmt_chk_economic_labour_sorting_coeff, expand = F, fill = F, padding = 10)    
 vbox_economic_params_setsorting$packStart(hbox_economic_params_setsorting, expand = F, fill = F, padding = 5)    

    
   radio_sorting_equalToDiscardRate <- gtkRadioButton()
   gSignalConnect(radio_sorting_equalToDiscardRate, "toggled", activate_input_sorting)
radio_sorting_equalToDiscardRate$add(gtkLabel("Equal to Discard Rate"))
radio_sorting_fromVector <- gtkRadioButtonNewWithLabelFromWidget(radio_sorting_equalToDiscardRate, " From vector")
  
   gSignalConnect(radio_sorting_fromVector, "toggled", activate_input_sorting)
   


# ------------------------------------------------------------------------------------------------------ blocco check   --> hbox_economic_params_setsorting 

hbox_economic_params_setsorting_vect <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_params_setsorting_vect <- gtkVBox(FALSE, 5) 
 
  hbox_economic_params_setsorting_vect$packStart(radio_sorting_equalToDiscardRate, expand = F, fill = F, padding = 0)  
    hbox_economic_params_setsorting_vect$packStart(radio_sorting_fromVector, expand = F, fill = F, padding = 30)  
    
vbox_economic_params_setsorting_vect$packStart(hbox_economic_params_setsorting_vect, expand = F, fill = F, padding = 5)  

# ------------------------------------------------------------------------------------------------------ blocco options   --> vbox_economic_params_setsorting_vect                        

bmt_vboxlabour_sorting$packStart( vbox_economic_params_setsorting_vect, expand = F, F, 0)
#bmt_vboxlabour_sorting$packStart(bmt_vboxlabour_sorting_table , expand = F, F, 0)  # tabella
bmt_vboxlabour_sorting$packStart(bmt_vboxdiscard_cost , expand = F, F, 0)  

bmt_hboxlabour_sorting$packStart(vbox_economic_params_setsorting , expand = F, F, 10)   
bmt_hboxlabour_sorting$packStart(bmt_vboxlabour_sorting , expand = T, T, 10)  





frame_economic_params_labourcostcomponents <- gtkFrame(" Components in the labour costs dynamic ") 
 hbox_economic_params_labourcostcomponents <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_labourcostcomponents <- gtkVBox(FALSE, 5)        
 hbox_economic_params_labourcostcomponents$packStart(frame_economic_params_labourcostcomponents, expand = T, fill = T, padding = 10)                 
   vbox_economic_params_labourcostcomponents$packStart(hbox_economic_params_labourcostcomponents, expand = F, fill = F, padding = 0)                        
   frame_economic_params_labourcostcomponents$add(vbox_economic_labour_fuelcommothers_container)           


frame_economic_params_labourcrew <- gtkFrame(" Crew remuneration ") 
 hbox_economic_params_labourcrew <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_labourcrew <- gtkVBox(FALSE, 5)        
 hbox_economic_params_labourcrew$packStart(frame_economic_params_labourcrew, expand = T, fill = T, padding = 10)                 
vbox_economic_params_labourcrew$packStart(hbox_economic_params_labourcrew, expand = F, fill = F, padding = 0)                        
frame_economic_params_labourcrew$add(vbox_economic_cost_crew_minwage_container)   

 frame_economic_params_laboursorting <- gtkFrame(" Costs due to Landing Obligations ") 
 hbox_economic_params_laboursorting <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_laboursorting <- gtkVBox(FALSE, 5)        
 hbox_economic_params_laboursorting$packStart(frame_economic_params_laboursorting, expand = T, fill = T, padding = 10)                 
 vbox_economic_params_laboursorting$packStart(hbox_economic_params_laboursorting, expand = F, fill = F, padding = 0)  

 #frame_economic_params_laboursorting$add(vbox_economic_params_setsorting)                     
     frame_economic_params_laboursorting$add(bmt_hboxlabour_sorting)   


    vbox_economic_params_labour_cost$packStart(vbox_economic_params_labourcrew, expand = F, fill = F, padding = 5)    
    vbox_economic_params_labour_cost$packStart(vbox_economic_params_labourcostcomponents, expand = F, fill = F, padding = 5)    
    vbox_economic_params_labour_cost$packStart(vbox_economic_params_laboursorting, expand = F, fill = F, padding = 5) 
     
               gtkToggleButtonSetActive(bmt_chk_economic_labour_sorting_coeff, T)
          gtkToggleButtonSetActive(bmt_chk_economic_labour_sorting_coeff, F)
          gtkToggleButtonSetActive(radio_sorting_equalToDiscardRate, F)
             gtkToggleButtonSetActive(radio_sorting_equalToDiscardRate, T)


            
