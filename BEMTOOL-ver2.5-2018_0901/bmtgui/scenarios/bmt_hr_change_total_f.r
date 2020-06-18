# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 
# aggiungere tutto a vbox_hr_change_total_f

bmt_entry_timespan_r6 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_timespan_r6, NUMERICAL_ENTRY_LENGTH)

bmt_entry_reduction_r6 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_reduction_r6, NUMERICAL_ENTRY_LENGTH)

vbox_hr_change_total_f_table1 <- gtkVBox(FALSE, 5)

 bmt_tbl_scenarios_r6_text_input <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_scenarios_r6_text_input$SetRowSpacings(5)
bmt_tbl_scenarios_r6_text_input$SetColSpacings(10)
bmt_tbl_scenarios_r6_text_input$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_scenarios_r6_text_input$Attach(gtkLabel("Time span"),i, i+1, j, j+1) 
j=j+1
bmt_tbl_scenarios_r6_text_input$Attach(gtkLabel("% of reduction"),i, i+1, j, j+1) 

i=i+1 # column 0 
j=0 
bmt_tbl_scenarios_r6_text_input$Attach(bmt_entry_timespan_r6,i, i+1, j, j+1) 
j=j+1
bmt_tbl_scenarios_r6_text_input$Attach(bmt_entry_reduction_r6,i, i+1, j, j+1) 




#hbox_scenarios_r6_text_input_1 <- gtkHBox(FALSE, 5)
#hbox_scenarios_r6_text_input_2 <- gtkHBox(FALSE, 5)
#hbox_scenarios_r6_text_input_1$packStart(gtkLabel("Time span") , expand=F, F, 5) 
#hbox_scenarios_r6_text_input_1$packStart(bmt_entry_timespan_r6 , expand=F, F, 15) 
#hbox_scenarios_r6_text_input_2$packStart(gtkLabel("% of reduction") , expand=F, F, 15) 
#hbox_scenarios_r6_text_input_2$packStart(bmt_entry_reduction_r6 , expand=F, F,15) 
#vbox_scenarios_r6_text_input <- gtkVBox(FALSE, 5)
#vbox_scenarios_r6_text_input$packStart(hbox_scenarios_r6_text_input_1 , expand=F, F, 0)
#vbox_scenarios_r6_text_input$packStart(hbox_scenarios_r6_text_input_2 , expand=F, F, 0)

#vbox_hr_change_total_f_table1$packStart(hbox_scenarios_r6_text_input , expand=F, F, 0)

# ------------------------------------------------------------------------------------

# --------------------------- DAY table
input_table_r6.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r6.sw$setShadowType("etched-in")
input_table_r6.sw$setPolicy("automatic", "automatic")
input_table_r6.sw$SetUsize(350, 50)  
input_table_r6_list <<- list()
input_table_r6Index <<- 0
# create model
if (!is.null(BMT_FLEETSEGMENTS))  {
input_table_r6.create_model()
# create tree view
input_table_r6.treeview <<- gtkTreeViewNewWithModel(input_table_r6.model)
input_table_r6.treeview$setRulesHint(TRUE)
input_table_r6.treeview$getSelection()$setMode("single")
input_table_r6.add_columns(input_table_r6.treeview)
input_table_r6.sw$add(input_table_r6.treeview) 
} 

hbox_scenarios_r6_table <- gtkHBox(FALSE, 5)
hbox_scenarios_r6_table$packStart(bmt_tbl_scenarios_r6_text_input , expand=F, F, 5) 

hbox_scenarios_r6_table$packStart(input_table_r6.sw , expand=F, F, 5) 

  
vbox_hr_change_total_f_table1$packStart(hbox_scenarios_r6_table , expand = T, fill = T, padding = 0)



vbox_hr_change_total_f_table2 <- gtkVBox(FALSE, 5)
vbox_scenarios_r6_table_by_species <- gtkVBox(FALSE, 5)


input_table_r6_species_settings.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r6_species_settings.sw$setShadowType("etched-in")
input_table_r6_species_settings.sw$setPolicy("automatic", "automatic")
input_table_r6_species_settings.sw$SetUsize(350, 70)  
input_table_r6_species_settings_list <<- list()
input_table_r6_species_settingsIndex <<- 0
# create model
if (!is.null(BMT_SPECIES))  {
input_table_r6_species_settings.create_model()
# create tree view
input_table_r6_species_settings.treeview <<- gtkTreeViewNewWithModel(input_table_r6_species_settings.model)
input_table_r6_species_settings.treeview$setRulesHint(TRUE)
input_table_r6_species_settings.treeview$getSelection()$setMode("single")
input_table_r6_species_settings.add_columns(input_table_r6_species_settings.treeview)
input_table_r6_species_settings.sw$add(input_table_r6_species_settings.treeview) 
} 


vbox_scenarios_r6_table_by_species$packStart( gtkLabel("* SRR model can be 1: Richer, 2: Shepherd, 3: Bevholt, 4: Segreg, 5: Geomean"), padding = 0  )
vbox_scenarios_r6_table_by_species$packStart( gtkLabel("** F settings can be 1: Last, 2: Rescaled") , padding = 0  )


vbox_hr_change_total_f_table2$packStart(input_table_r6_species_settings.sw , expand = F, fill = FALSE, padding = 0)
vbox_hr_change_total_f_table2$packStart(vbox_scenarios_r6_table_by_species , expand = F, fill = FALSE, padding = 0)



hbox_hr_change_total_f_twotables <-gtkHBox()
hbox_hr_change_total_f_twotables$packStart(vbox_hr_change_total_f_table1 , expand = T, fill = T, padding = 0)
hbox_hr_change_total_f_twotables$packStart(vbox_hr_change_total_f_table2 , expand = T, fill = T, padding = 5) 

vbox_hr_change_total_f$packStart(hbox_hr_change_total_f_twotables , expand = T, fill = FALSE, padding = 5) 

