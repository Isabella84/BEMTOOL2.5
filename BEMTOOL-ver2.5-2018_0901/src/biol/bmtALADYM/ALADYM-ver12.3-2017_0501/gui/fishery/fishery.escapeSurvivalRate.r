# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hbox_box_EscapeSurvivalRate <- gtkHBox(homogeneous = FALSE)
vbox_box_EscapeSurvivalRate <- gtkVBox(homogeneous = FALSE)

hbox_box_EscapeSurvivalRate$packStart(vbox_box_EscapeSurvivalRate, expand = T, fill = T, padding = 5)

hbox_EscapeSurvivability <- gtkHBox(homogeneous = FALSE)

# loca_discardsM_sim_temp <-  as.numeric(Discardvector(D_vectorM_fore[D_vectorM_fore$Year==years[(as.integer((loca_irun-1)/12)+1)],2+g],BAS$MLength,INP$tr)  )
title_escape_surv <- gtkLabel("Escape survivability")
gtkLabelSetWidthChars(title_escape_surv, 25)    
hbox_EscapeSurvivability$packStart(title_escape_surv, expand = FALSE, fill = FALSE, padding = 5) 

combo_escape_survival_rate <- gtkComboBoxNewText()
gSignalConnect(combo_escape_survival_rate, "changed", deactivate_escape_Survivability_unused_params)
for (choice in c("YES", "NO")) {
combo_escape_survival_rate$appendText(choice)
}

hbox_EscapeSurvivability$packStart(combo_escape_survival_rate, expand = FALSE, fill = FALSE, padding = 5)

lbl_option_escape_survivability <- gtkLabel(" Options for Escape Survivability ")
gtkLabelSetWidthChars(lbl_option_escape_survivability, 40)    
lbl_option_escape_survivability_males <- gtkLabel("MALES") 
lbl_option_escape_survivability_females <- gtkLabel("FEMALES") 
gtkLabelSetWidthChars(lbl_option_escape_survivability_males, LABEL_LENGTH)  
gtkLabelSetWidthChars(lbl_option_escape_survivability_females, LABEL_LENGTH)  

radio_escape_survivability_size <- gtkRadioButton()
radio_escape_survivability_size$add(gtkLabel("Depending on size"))
radio_escape_survivability_constant <- gtkRadioButtonNewWithLabelFromWidget(radio_escape_survivability_size, "Constant")

entry_escape_survivability_males <- gtkEntry() 
gtkEntrySetWidthChars(entry_escape_survivability_males, 6)
gtkEntrySetText(entry_escape_survivability_males, 0)

entry_escape_survivability_females <- gtkEntry() 
gtkEntrySetWidthChars(entry_escape_survivability_females, 6)
gtkEntrySetText(entry_escape_survivability_females, 0)  

radio_escape_survivability_size_O <- gtkRadioButton()
radio_escape_survivability_size_O$add(gtkLabel("Ogive"))
radio_escape_survivability_size_EV <- gtkRadioButtonNewWithLabelFromWidget(radio_escape_survivability_size_O, "External vector")

hbox_EscapeSurvivability$packStart(lbl_option_escape_survivability, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability$packStart(radio_escape_survivability_constant, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability$packStart(radio_escape_survivability_size, expand = F, fill = F, padding = 15)

button_load_survivability_vect_age <- gtkButtonNewWithLabel("Load survivability by age...")
button_load_survivability_vect_age$AddCallback("clicked", loadEscapeSurvivabilityByAgefromFile)

button_export_survivability_vect_age <- gtkButtonNewWithLabel("Export survivability by age...")
button_export_survivability_vect_age$AddCallback("clicked", saveEscapeSurvivabilityByAgetoFile)

hbox_EscapeSurvivability$packStart(button_load_survivability_vect_age, expand = F, fill = F, padding = 15)
hbox_EscapeSurvivability$packStart(button_export_survivability_vect_age, expand = F, fill = F, padding = 15)


escape_survival_extvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
escape_survival_extvector_M.sw$setShadowType("etched-in")
escape_survival_extvector_M.sw$setPolicy("automatic", "automatic")
escape_survival_extvector_M.sw$SetUsize(350, 80)  

escape_survival_extvector_M_list <<- list()
escape_survival_extvector_MIndex <<- 0
# ------------------------------
# create model
escape_survival_extvector_M.create_model()
# create tree view
escape_survival_extvector_M.treeview <<- gtkTreeViewNewWithModel(escape_survival_extvector_M.model)
escape_survival_extvector_M.treeview$setRulesHint(TRUE)
escape_survival_extvector_M.treeview$getSelection()$setMode("single")
escape_survival_extvector_M.add_columns(escape_survival_extvector_M.treeview)
escape_survival_extvector_M.sw$add(escape_survival_extvector_M.treeview)  


escape_survival_extvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
escape_survival_extvector_F.sw$setShadowType("etched-in")
escape_survival_extvector_F.sw$setPolicy("automatic", "automatic")
escape_survival_extvector_F.sw$SetUsize(350, 80)  

escape_survival_extvector_F_list <<- list()
escape_survival_extvector_FIndex <<- 0
# ------------------------------
# create model
escape_survival_extvector_F.create_model()
# create tree view
escape_survival_extvector_F.treeview <<- gtkTreeViewNewWithModel(escape_survival_extvector_F.model)
escape_survival_extvector_F.treeview$setRulesHint(TRUE)
escape_survival_extvector_F.treeview$getSelection()$setMode("single")
escape_survival_extvector_F.add_columns(escape_survival_extvector_F.treeview)
escape_survival_extvector_F.sw$add(escape_survival_extvector_F.treeview)  



#gSignalConnect(radio_escape_survivability_size_O, "toggled", deactive_survivability_C_S)
#gSignalConnect(radio_escape_survivability_size_O, "toggled", deactive_survivability_C_S)

frame_escape_surv <- gtkFrame(" ESCAPE SURVIVABILITY ")   
hbox_escape_surv <- gtkHBox(homogeneous = FALSE, 5)
vbox_escape_surv <- gtkVBox(homogeneous = FALSE, 5)             

hbox_escape_surv$packStart(hbox_EscapeSurvivability, expand = TRUE, fill = TRUE, padding = 10)
vbox_escape_surv$packStart(hbox_escape_surv, expand = TRUE, fill = TRUE, padding = 10)
frame_escape_surv$add(vbox_escape_surv) 

h_frame_escape_surv <- gtkHBox(homogeneous = FALSE, 5)
h_frame_escape_surv$packStart(frame_escape_surv, expand = TRUE, fill = TRUE, padding = 5)		

v_frame_escape_surv <- gtkVBox(homogeneous = FALSE, 5)
v_frame_escape_surv$packStart(h_frame_escape_surv, expand = TRUE, fill = T, padding = 5)

vbox_box_EscapeSurvivalRate$packStart(v_frame_escape_surv, expand = F, fill = FALSE, padding = 5)





tbl_escape_surviv <- gtkTable(4,2,homogeneous = F)
tbl_escape_surviv$SetRowSpacings(7)
tbl_escape_surviv$SetColSpacings(30)
tbl_escape_surviv$SetBorderWidth(5) 

i=0  # column 1
j=0   
lbl_escape_surv_C <-  gtkLabel(" CONSTANT ")
tbl_escape_surviv$Attach(lbl_escape_surv_C,i, i+1, j, j+1) 
j=j+1
h_escape_surv_cost_M <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_cost_M$packStart(lbl_option_escape_survivability_males, expand = F, fill = F, padding = 5)
h_escape_surv_cost_M$packStart(entry_escape_survivability_males, expand = F, fill = F, padding = 5)
tbl_escape_surviv$Attach(h_escape_surv_cost_M,i, i+1, j, j+1)  
j=j+1
h_escape_surv_cost_F <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_cost_F$packStart(lbl_option_escape_survivability_females, expand = F, fill = F, padding = 5)
h_escape_surv_cost_F$packStart(entry_escape_survivability_females, expand = F, fill = F, padding = 5)
tbl_escape_surviv$Attach(h_escape_surv_cost_F,i, i+1, j, j+1)  


i=i+1   # column 2
j=0
lbl_DOS <- gtkLabel(" DEPENDING ON SIZE ") 
tbl_escape_surviv$Attach(lbl_DOS,i, i+1, j, j+1)

j=j+1   
#h_escape_surv_size_option_O$packStart(radio_escape_survivability_size_O, expand = F, fill = F, padding = 5)
tbl_escape_surviv$Attach(radio_escape_survivability_size_O,i, i+1, j, j+1)  
j=j+1
tbl_escape_surviv$Attach(radio_escape_survivability_size_EV,i, i+1, j, j+1)  


i=i+1   # column 3
j=0
j=j+1
lbl_option_survivability_ogive_param1 <- gtkLabel("L50% [mm]") 
lbl_option_survivability_ogive_param2 <- gtkLabel("L75%L25% [mm]") 
#gtkLabelSetWidthChars(lbl_option_survivability_ogive_param1, LABEL_LENGTH)  
#gtkLabelSetWidthChars(lbl_option_survivability_ogive_param2, LABEL_LENGTH)  

entry_survivability_ogive_param1 <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_ogive_param1, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_ogive_param1, 0)

entry_survivability_ogive_param2 <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_ogive_param2, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_ogive_param2, 0) 
 
h_escape_surv_size_option_O <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_size_option_O$packStart(lbl_option_survivability_ogive_param1, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O$packStart(entry_survivability_ogive_param1, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O$packStart(lbl_option_survivability_ogive_param2, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_O$packStart(entry_survivability_ogive_param2, expand = F, fill = F, padding = 5)

tbl_escape_surviv$Attach(h_escape_surv_size_option_O,i, i+1, j, j+1)  

j=j+1     
h_escape_surv_size_option_EV <- gtkHBox(homogeneous = FALSE, 5)
tbl_escape_surviv$Attach(h_escape_surv_size_option_EV,i, i+1, j, j+1)  

lbl_esc_surv_DOS_EV_M <- gtkLabel("MALES")
lbl_esc_surv_DOS_EV_F <- gtkLabel("FEMALES")
h_escape_surv_size_option_EV$packStart(lbl_esc_surv_DOS_EV_M, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV$packStart(escape_survival_extvector_M.sw, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV$packStart(lbl_esc_surv_DOS_EV_F, expand = F, fill = F, padding = 5)
h_escape_surv_size_option_EV$packStart(escape_survival_extvector_F.sw, expand = F, fill = F, padding = 5)


h_escape_surv_size_table <- gtkHBox(homogeneous = FALSE, 5)
h_escape_surv_size_table$packStart(tbl_escape_surviv, expand = F, fill = T, padding = 10)
vbox_escape_surv$packStart(h_escape_surv_size_table, expand = F, fill = F, padding = 10)

gSignalConnect(radio_escape_survivability_constant, "toggled", deactive_survivability_C_S)
gSignalConnect(radio_escape_survivability_size, "toggled", deactive_survivability_C_S)

gSignalConnect(radio_escape_survivability_size_O, "toggled", deactive_survivability_O_EV)
gSignalConnect(radio_escape_survivability_size_EV, "toggled", deactive_survivability_O_EV)

gSignalConnect(entry_escape_survivability_males, "changed", set_survivability_C_males)
gSignalConnect(entry_escape_survivability_females, "changed", set_survivability_C_females)

gSignalConnect(entry_survivability_ogive_param1, "changed", set_survivability_OGIVE_param1)
gSignalConnect(entry_survivability_ogive_param2, "changed", set_survivability_OGIVE_param2)


gtkComboBoxSetActive(combo_escape_survival_rate, 0)
gtkToggleButtonSetActive(radio_escape_survivability_constant, TRUE) 
 deactive_survivability_C_S()