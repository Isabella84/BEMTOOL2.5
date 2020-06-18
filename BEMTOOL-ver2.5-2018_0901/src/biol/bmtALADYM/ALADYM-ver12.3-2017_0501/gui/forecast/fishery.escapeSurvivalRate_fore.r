# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hbox_box_EscapeSurvivalRate_fore <- gtkHBox(homogeneous = FALSE)
vbox_box_EscapeSurvivalRate_fore <- gtkVBox(homogeneous = FALSE)

hbox_box_EscapeSurvivalRate_fore$packStart(vbox_box_EscapeSurvivalRate_fore, expand = T, fill = T, padding = 5)

hbox_EscapeSurvivability_fore <- gtkHBox(homogeneous = FALSE)

# loca_discardsM_sim_temp <-  as.numeric(Discardvector(D_vectorM_fore[D_vectorM_fore$Year==years[(as.integer((loca_irun-1)/12)+1)],2+g],BAS$MLength,INP$tr)  )
title_escape_surv_fore <- gtkLabel("Escape survivability")
gtkLabelSetWidthChars(title_escape_surv_fore, 25)    
hbox_EscapeSurvivability_fore$packStart(title_escape_surv_fore, expand = FALSE, fill = FALSE, padding = 5) 

combo_escape_survival_rate_fore <- gtkComboBoxNewText()
gSignalConnect(combo_escape_survival_rate_fore, "changed", deactivate_escape_Survivability_unused_params_fore)
for (choice in c("YES", "NO")) {
combo_escape_survival_rate_fore$appendText(choice)
}

hbox_EscapeSurvivability_fore$packStart(combo_escape_survival_rate_fore, expand = FALSE, fill = FALSE, padding = 5)

lbl_option_escape_survivability_fore <- gtkLabel(" Options for Escape Survivability ")
gtkLabelSetWidthChars(lbl_option_escape_survivability_fore, 40)    
lbl_option_escape_survivability_males_fore <- gtkLabel("MALES") 
lbl_option_escape_survivability_females_fore <- gtkLabel("FEMALES") 
gtkLabelSetWidthChars(lbl_option_escape_survivability_males_fore, LABEL_LENGTH)  
gtkLabelSetWidthChars(lbl_option_escape_survivability_females_fore, LABEL_LENGTH)  

radio_escape_survivability_size_fore <- gtkRadioButton()
radio_escape_survivability_size_fore$add(gtkLabel("Depending on size"))
radio_escape_survivability_constant_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_escape_survivability_size_fore, "Constant")

entry_escape_survivability_males_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_escape_survivability_males_fore, 6)
gtkEntrySetText(entry_escape_survivability_males_fore, 0)

entry_escape_survivability_females_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_escape_survivability_females_fore, 6)
gtkEntrySetText(entry_escape_survivability_females_fore, 0)  

radio_escape_survivability_size_O_fore <- gtkRadioButton()
radio_escape_survivability_size_O_fore$add(gtkLabel("Ogive"))
radio_escape_survivability_size_EV_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_escape_survivability_size_O_fore, "External vector")

hbox_EscapeSurvivability_fore$packStart(lbl_option_escape_survivability_fore, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability_fore$packStart(radio_escape_survivability_constant_fore, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability_fore$packStart(radio_escape_survivability_size_fore, expand = F, fill = F, padding = 15)

button_load_survivability_vect_age_fore <- gtkButtonNewWithLabel("Load survivability by age...")
button_load_survivability_vect_age_fore$AddCallback("clicked", loadEscapeSurvivabilityByAgefromFile_fore)

button_export_survivability_vect_age_fore <- gtkButtonNewWithLabel("Export survivability by age...")
button_export_survivability_vect_age_fore$AddCallback("clicked", saveEscapeSurvivabilityByAgetoFile_fore)

hbox_EscapeSurvivability_fore$packStart(button_load_survivability_vect_age_fore, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability_fore$packStart(button_export_survivability_vect_age_fore, expand = F, fill = F, padding = 15)

escape_survival_extvector_M_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
escape_survival_extvector_M_fore.sw$setShadowType("etched-in")
escape_survival_extvector_M_fore.sw$setPolicy("automatic", "automatic")
escape_survival_extvector_M_fore.sw$SetUsize(350, 80)  

escape_survival_extvector_M_list_fore <<- list()
escape_survival_extvector_MIndex_fore <<- 0
# ------------------------------
# create model
escape_survival_extvector_M_fore.create_model()
# create tree view
escape_survival_extvector_M_fore.treeview <<- gtkTreeViewNewWithModel(escape_survival_extvector_M_fore.model)
escape_survival_extvector_M_fore.treeview$setRulesHint(TRUE)
escape_survival_extvector_M_fore.treeview$getSelection()$setMode("single")
escape_survival_extvector_M_fore.add_columns(escape_survival_extvector_M_fore.treeview)
escape_survival_extvector_M_fore.sw$add(escape_survival_extvector_M_fore.treeview)  


escape_survival_extvector_F_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
escape_survival_extvector_F_fore.sw$setShadowType("etched-in")
escape_survival_extvector_F_fore.sw$setPolicy("automatic", "automatic")
escape_survival_extvector_F_fore.sw$SetUsize(350, 80)  

escape_survival_extvector_F_list_fore <<- list()
escape_survival_extvector_FIndex_fore <<- 0
# ------------------------------
# create model
escape_survival_extvector_F_fore.create_model()
# create tree view
escape_survival_extvector_F_fore.treeview <<- gtkTreeViewNewWithModel(escape_survival_extvector_F_fore.model)
escape_survival_extvector_F_fore.treeview$setRulesHint(TRUE)
escape_survival_extvector_F_fore.treeview$getSelection()$setMode("single")
escape_survival_extvector_F_fore.add_columns(escape_survival_extvector_F_fore.treeview)
escape_survival_extvector_F_fore.sw$add(escape_survival_extvector_F_fore.treeview)  



#gSignalConnect(radio_escape_survivability_size_O, "toggled", deactive_survivability_C_S)
#gSignalConnect(radio_escape_survivability_size_O, "toggled", deactive_survivability_C_S)

frame_escape_surv_fore <- gtkFrame(" ESCAPE SURVIVABILITY ")   
hbox_escape_surv_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_escape_surv_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_escape_surv_fore$packStart(hbox_EscapeSurvivability_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_escape_surv_fore$packStart(hbox_escape_surv_fore, expand = TRUE, fill = TRUE, padding = 10)
frame_escape_surv_fore$add(vbox_escape_surv_fore) 

h_frame_escape_surv_fore <- gtkHBox(homogeneous = FALSE, 5)
h_frame_escape_surv_fore$packStart(frame_escape_surv_fore, expand = TRUE, fill = TRUE, padding = 5)		

v_frame_escape_surv_fore <- gtkVBox(homogeneous = FALSE, 5)
v_frame_escape_surv_fore$packStart(h_frame_escape_surv_fore, expand = TRUE, fill = T, padding = 5)

vbox_box_EscapeSurvivalRate_fore$packStart(v_frame_escape_surv_fore, expand = F, fill = FALSE, padding = 5)





tbl_escape_surviv_fore <- gtkTable(4,2,homogeneous = F)
tbl_escape_surviv_fore$SetRowSpacings(7)
tbl_escape_surviv_fore$SetColSpacings(30)
tbl_escape_surviv_fore$SetBorderWidth(5) 

i=0  # column 1
j=0   
lbl_escape_surv_C_fore <-  gtkLabel(" CONSTANT ")
tbl_escape_surviv_fore$Attach(lbl_escape_surv_C_fore,i, i+1, j, j+1) 
j=j+1
h_escape_surv_cost_M_fore <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_cost_M_fore$packStart(lbl_option_escape_survivability_males_fore, expand = F, fill = F, padding = 5)
h_escape_surv_cost_M_fore$packStart(entry_escape_survivability_males_fore, expand = F, fill = F, padding = 5)
tbl_escape_surviv_fore$Attach(h_escape_surv_cost_M_fore,i, i+1, j, j+1)  
j=j+1
h_escape_surv_cost_F_fore <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_cost_F_fore$packStart(lbl_option_escape_survivability_females_fore, expand = F, fill = F, padding = 5)
h_escape_surv_cost_F_fore$packStart(entry_escape_survivability_females_fore, expand = F, fill = F, padding = 5)
tbl_escape_surviv_fore$Attach(h_escape_surv_cost_F_fore,i, i+1, j, j+1)  


i=i+1   # column 2
j=0
lbl_DOS_fore <- gtkLabel(" DEPENDING ON SIZE ") 
tbl_escape_surviv_fore$Attach(lbl_DOS_fore,i, i+1, j, j+1)

j=j+1   
#h_escape_surv_size_option_O$packStart(radio_escape_survivability_size_O, expand = F, fill = F, padding = 5)
tbl_escape_surviv_fore$Attach(radio_escape_survivability_size_O_fore,i, i+1, j, j+1)  
j=j+1
tbl_escape_surviv_fore$Attach(radio_escape_survivability_size_EV_fore,i, i+1, j, j+1)  

i=i+1   # column 3
j=0
j=j+1
lbl_option_survivability_ogive_param1_fore <- gtkLabel("L50% [mm]") 
lbl_option_survivability_ogive_param2_fore <- gtkLabel("L75%L25% [mm]") 
#gtkLabelSetWidthChars(lbl_option_survivability_ogive_param1, LABEL_LENGTH)  
#gtkLabelSetWidthChars(lbl_option_survivability_ogive_param2, LABEL_LENGTH)  

entry_survivability_ogive_param1_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_ogive_param1_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_ogive_param1_fore, 0)

entry_survivability_ogive_param2_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_ogive_param2_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_ogive_param2_fore, 0) 
 
h_escape_surv_size_option_O_fore <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_size_option_O_fore$packStart(lbl_option_survivability_ogive_param1_fore, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O_fore$packStart(entry_survivability_ogive_param1_fore, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O_fore$packStart(lbl_option_survivability_ogive_param2_fore, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O_fore$packStart(entry_survivability_ogive_param2_fore, expand = F, fill = F, padding = 5)

tbl_escape_surviv_fore$Attach(h_escape_surv_size_option_O_fore,i, i+1, j, j+1)  

j=j+1     
h_escape_surv_size_option_EV_fore <- gtkHBox(homogeneous = FALSE, 5)
tbl_escape_surviv_fore$Attach(h_escape_surv_size_option_EV_fore,i, i+1, j, j+1)  

lbl_esc_surv_DOS_EV_M_fore <- gtkLabel("MALES")
lbl_esc_surv_DOS_EV_F_fore <- gtkLabel("FEMALES")
h_escape_surv_size_option_EV_fore$packStart(lbl_esc_surv_DOS_EV_M_fore, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV_fore$packStart(escape_survival_extvector_M_fore.sw, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV_fore$packStart(lbl_esc_surv_DOS_EV_F_fore, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV_fore$packStart(escape_survival_extvector_F_fore.sw, expand = F, fill = F, padding = 5)


h_escape_surv_size_table_fore <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_size_table_fore$packStart(tbl_escape_surviv_fore, expand = F, fill = T, padding = 10)
vbox_escape_surv_fore$packStart(h_escape_surv_size_table_fore, expand = F, fill = F, padding = 10)

gSignalConnect(radio_escape_survivability_constant_fore, "toggled", deactive_survivability_C_S_fore)
gSignalConnect(radio_escape_survivability_size_fore, "toggled", deactive_survivability_C_S_fore)

gSignalConnect(radio_escape_survivability_size_O_fore, "toggled", deactive_survivability_O_EV_fore)
gSignalConnect(radio_escape_survivability_size_EV_fore, "toggled", deactive_survivability_O_EV_fore)

gSignalConnect(entry_escape_survivability_males_fore, "changed", set_survivability_C_males_fore)
gSignalConnect(entry_escape_survivability_females_fore, "changed", set_survivability_C_females_fore)

gSignalConnect(entry_survivability_ogive_param1_fore, "changed", set_survivability_OGIVE_param1_fore)
gSignalConnect(entry_survivability_ogive_param2_fore, "changed", set_survivability_OGIVE_param2_fore)

gtkComboBoxSetActive(combo_escape_survival_rate_fore, 0)
gtkToggleButtonSetActive(radio_escape_survivability_constant_fore, TRUE) 
 deactive_survivability_C_S_fore()