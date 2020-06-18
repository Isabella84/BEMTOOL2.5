# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


                                                        
# aggiungere tutto a vbox_hr_change_effort

hbox_scenarios_r7 <- gtkHBox(FALSE, 5)

hbox_choose_options_r7 <- gtkHBox(FALSE, 5)

bmt_button_load_abundance_r7 <- gtkButtonNewWithLabel(" Load... ")
#bmt_button_load_abundance_r7$AddCallback("clicked", bmt_loadAbundanceIndices_r7)

bmt_button_save_abundance_r7 <- gtkButtonNewWithLabel(" Save... ")
#bmt_button_save_abundance_r7$AddCallback("clicked", bmt_saveAbundanceIndices_r7)

bmt_button_load_prevTAC_r7 <- gtkButtonNewWithLabel(" Load... ")
#bmt_button_load_prevTAC_r7$AddCallback("clicked", bmt_loadPreviousTAC_r7)

bmt_button_save_prevTAC_r7 <- gtkButtonNewWithLabel(" Save... ")
#bmt_button_save_prevTAC_r7$AddCallback("clicked", bmt_savePreviousTAC_r7)

bmt_combo_optionTAC_r7 <- gtkComboBoxNewText()
gSignalConnect(bmt_combo_optionTAC_r7, "changed", activate_deactivate_opt3_r7)

#if (!is.null(BMT_FLEETSEGMENTS)) {
for (item in c("Option 1", "Option 2", "Option 3")) {
  bmt_combo_optionTAC_r7$appendText(item)
}
#}



bmt_combo_TAC_species_r7 <- gtkComboBoxNewText()
#gSignalConnect(bmt_combo_fleetsegments_effort_r4, "changed", bmt_reload_fleetsegment_info)

if (!is.null(BMT_SPECIES)) {
    for (item in BMT_SPECIES ) {
      bmt_combo_TAC_species_r7$appendText(item)
    }
}

hbox_choose_options_r7$packStart(gtkLabel("Species"), expand = FALSE, fill = FALSE, padding = 10)
hbox_choose_options_r7$packStart(bmt_combo_TAC_species_r7, expand = FALSE, fill = FALSE, padding = 10)
hbox_choose_options_r7$packStart(gtkLabel("TAC option"), expand = FALSE, fill = FALSE, padding = 10)
hbox_choose_options_r7$packStart(bmt_combo_optionTAC_r7, expand = FALSE, fill = FALSE, padding = 10)


input_table_r7.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r7.sw$setShadowType("etched-in")
input_table_r7.sw$setPolicy("automatic", "automatic")
input_table_r7.sw$SetUsize(400, 80)  
input_table_r7_list <<- list()
input_table_r7Index <<- 0
# create model
if (!is.null(BMT_FLEETSEGMENTS))  {
input_table_r7.create_model()
# create tree view
input_table_r7.treeview <<- gtkTreeViewNewWithModel(input_table_r7.model)
input_table_r7.treeview$setRulesHint(TRUE)
input_table_r7.treeview$getSelection()$setMode("single")
input_table_r7.add_columns(input_table_r7.treeview)
input_table_r7.sw$add(input_table_r7.treeview) 
} 


input_table_r7_opt3_indices.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r7_opt3_indices.sw$setShadowType("etched-in")
input_table_r7_opt3_indices.sw$setPolicy("automatic", "automatic")
input_table_r7_opt3_indices.sw$SetUsize(150, 80)  
input_table_r7_opt3_indices_list <<- list()
input_table_r7_opt3_indicesIndex <<- 0
# create model
if (!is.null(BMT_YEARS_SIMULATION))  {
input_table_r7_opt3_indices.create_model()
# create tree view
input_table_r7_opt3_indices.treeview <<- gtkTreeViewNewWithModel(input_table_r7_opt3_indices.model)
input_table_r7_opt3_indices.treeview$setRulesHint(TRUE)
input_table_r7_opt3_indices.treeview$getSelection()$setMode("single")
input_table_r7_opt3_indices.add_columns(input_table_r7_opt3_indices.treeview)
input_table_r7_opt3_indices.sw$add(input_table_r7_opt3_indices.treeview) 
} 



input_table_r7_opt3_tac.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r7_opt3_tac.sw$setShadowType("etched-in")
input_table_r7_opt3_tac.sw$setPolicy("automatic", "automatic")
input_table_r7_opt3_tac.sw$SetUsize(150, 80)  
input_table_r7_opt3_tac_list <<- list()
input_table_r7_opt3_tacIndex <<- 0
# create model
if (!is.null(BMT_YEARS_SIMULATION))  {
input_table_r7_opt3_tac.create_model()
# create tree view
input_table_r7_opt3_tac.treeview <<- gtkTreeViewNewWithModel(input_table_r7_opt3_tac.model)
input_table_r7_opt3_tac.treeview$setRulesHint(TRUE)
input_table_r7_opt3_tac.treeview$getSelection()$setMode("single")
input_table_r7_opt3_tac.add_columns(input_table_r7_opt3_tac.treeview)
input_table_r7_opt3_tac.sw$add(input_table_r7_opt3_tac.treeview) 
} 



vbox_scenarios_r7_table <- gtkVBox(FALSE, 5)
vbox_scenarios_r7_table$packStart(hbox_choose_options_r7 , expand=F, F, 0) 
vbox_scenarios_r7_table$packStart(input_table_r7.sw , expand=F, F, 0) 

hbox_scenarios_r7_table_option3 <- gtkHBox(FALSE, 5)
vbox_opt3_indices  <- gtkVBox(FALSE, 5)
vbox_opt3_tac <- gtkVBox(FALSE, 5)

hbox_files_Abundance_r7 <- gtkHBox()
hbox_files_Abundance_r7$packStart(gtkLabel("Abundance indices") , expand=F, F, 4)
hbox_files_Abundance_r7$packStart(bmt_button_load_abundance_r7 , expand=F, F, 4)
hbox_files_Abundance_r7$packStart(bmt_button_save_abundance_r7 , expand=F, F, 4)

hbox_files_prevTACs_r7 <- gtkHBox()
hbox_files_prevTACs_r7$packStart(gtkLabel("Previous TAC") , expand=F, F, 4)
hbox_files_prevTACs_r7$packStart(bmt_button_load_prevTAC_r7 , expand=F, F, 4)
hbox_files_prevTACs_r7$packStart(bmt_button_save_prevTAC_r7 , expand=F, F, 4)


vbox_opt3_indices$packStart(hbox_files_Abundance_r7 , expand=F, F, 0)
vbox_opt3_indices$packStart(input_table_r7_opt3_indices.sw , expand=F, F, 4) 
vbox_opt3_tac$packStart(hbox_files_prevTACs_r7 , expand=F, F, 0) 
vbox_opt3_tac$packStart(input_table_r7_opt3_tac.sw , expand=F, F, 4) 


hbox_scenarios_r7_table_option3$packStart(vbox_opt3_indices , expand=F, F, 5) 
hbox_scenarios_r7_table_option3$packStart(vbox_opt3_tac , expand=F, F, 5) 

  
#vbox_hr_change_total_f_table1$packStart(hbox_scenarios_r7_table , expand = T, fill = FALSE, padding = 5)
  
  hbox_scenarios_r7$packStart( vbox_scenarios_r7_table, expand = F, fill = FALSE, padding = 5) 
   hbox_scenarios_r7$packStart(hbox_scenarios_r7_table_option3, expand = T, fill = T, padding = 5)     
#                                     
#         vbox_hr_set_TAC$packStart(hbox_scenarios_r7, expand = FALSE, fill = FALSE, padding = 5)   
#        vbox_hr_set_TAC$packStart(hbox_scenarios_r4, expand = FALSE, fill = FALSE, padding = 5)   
#
                vbox_hr_set_tac$packStart(hbox_scenarios_r7, expand = FALSE, fill = FALSE, padding = 5) 
                
                 gtkComboBoxSetActive(bmt_combo_optionTAC_r7, 0)               
                        
