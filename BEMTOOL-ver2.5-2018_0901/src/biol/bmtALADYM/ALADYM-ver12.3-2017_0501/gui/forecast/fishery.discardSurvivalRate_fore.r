# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hbox_box_DiscardSurvivalRate_fore <- gtkHBox(homogeneous = FALSE)
vbox_box_DiscardSurvivalRate_fore <- gtkVBox(homogeneous = FALSE)

hbox_box_DiscardSurvivalRate_fore$packStart(vbox_box_DiscardSurvivalRate_fore, expand = FALSE, fill = FALSE, padding = 5)

hbox_DiscardSurvivability_fore <- gtkHBox(homogeneous = FALSE)

title_disc_surv_fore <- gtkLabel("Discard survivability")
gtkLabelSetWidthChars(title_disc_surv_fore, 25)  
hbox_DiscardSurvivability_fore$packStart(title_disc_surv_fore, expand = FALSE, fill = FALSE, padding = 5) 

combo_discard_survival_rate_fore <- gtkComboBoxNewText()
gSignalConnect(combo_discard_survival_rate_fore, "changed", deactivate_Survivability_unused_params_fore)
for (choice in c("YES", "NO")) {
combo_discard_survival_rate_fore$appendText(choice)
}

hbox_DiscardSurvivability_fore$packStart(combo_discard_survival_rate_fore, expand = FALSE, fill = FALSE, padding = 5)

lbl_option_survivability_fore <- gtkLabel(" Options for Discard Survavibility ")
gtkLabelSetWidthChars(lbl_option_survivability_fore, 40)   
lbl_option_survivability_fore_param1_males <- gtkLabel("L50% [mm]") 
lbl_option_survivability_fore_param2_females <- gtkLabel("L75%L25% [mm]") 
gtkLabelSetWidthChars(lbl_option_survivability_fore_param1_males, LABEL_LENGTH)  
gtkLabelSetWidthChars(lbl_option_survivability_fore_param2_females, LABEL_LENGTH)  



radio_survivability_ogive_fore <- gtkRadioButton()
radio_survivability_ogive_fore$add(gtkLabel("Ogive depending on size"))
radio_survivability_constant_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_survivability_ogive_fore, "Constant")

entry_survivability_param1_males_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_param1_males_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_param1_males_fore, 0)

gSignalConnect(entry_survivability_param1_males_fore, "changed", change_survivability_DISCARD_param1_males_fore)

entry_survivability_param2_females_fore <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_param2_females_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_param2_females_fore, 0)  

gSignalConnect(entry_survivability_param2_females_fore, "changed", change_survivability_DISCARD_param2_females_fore)

hbox_DiscardSurvivability_fore$packStart(lbl_option_survivability_fore, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability_fore$packStart(radio_survivability_ogive_fore, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability_fore$packStart(radio_survivability_constant_fore, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability_fore$packStart(lbl_option_survivability_fore_param1_males, expand = F, fill = F, padding = 25)
hbox_DiscardSurvivability_fore$packStart(entry_survivability_param1_males_fore, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability_fore$packStart(lbl_option_survivability_fore_param2_females, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability_fore$packStart(entry_survivability_param2_females_fore, expand = F, fill = F, padding = 15)


gtkComboBoxSetActive(combo_discard_survival_rate_fore, 0)
gSignalConnect(radio_survivability_ogive_fore, "toggled", deactive_survivability_O_C_fore)
gSignalConnect(radio_survivability_constant_fore, "toggled", deactive_survivability_O_C_fore)


frame_discard_surv_fore <- gtkFrame(" DISCARD SURVIVABILITY ")   
hbox_discard_surv_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_discard_surv_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_discard_surv_fore$packStart(hbox_DiscardSurvivability_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_discard_surv_fore$packStart(hbox_discard_surv_fore, expand = TRUE, fill = TRUE, padding = 10)
frame_discard_surv_fore$add(vbox_discard_surv_fore) 

h_frame_discard_surv_fore <- gtkHBox(homogeneous = FALSE, 5)
h_frame_discard_surv_fore$packStart(frame_discard_surv_fore, expand = TRUE, fill = TRUE, padding = 5)		

v_frame_discard_surv_fore <- gtkVBox(homogeneous = FALSE, 5)
v_frame_discard_surv_fore$packStart(h_frame_discard_surv_fore, expand = TRUE, fill = T, padding = 5)

vbox_box_DiscardSurvivalRate_fore$packStart(v_frame_discard_surv_fore, expand = F, fill = FALSE, padding = 5)