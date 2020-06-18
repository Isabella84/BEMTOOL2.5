# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hbox_box_DiscardSurvivalRate <- gtkHBox(homogeneous = FALSE)
vbox_box_DiscardSurvivalRate <- gtkVBox(homogeneous = FALSE)

hbox_box_DiscardSurvivalRate$packStart(vbox_box_DiscardSurvivalRate, expand = FALSE, fill = FALSE, padding = 5)

hbox_DiscardSurvivability <- gtkHBox(homogeneous = FALSE)

title_disc_surv <- gtkLabel("Discard survivability")
gtkLabelSetWidthChars(title_disc_surv, 25)  
hbox_DiscardSurvivability$packStart(title_disc_surv, expand = FALSE, fill = FALSE, padding = 5) 

combo_discard_survival_rate <- gtkComboBoxNewText()
gSignalConnect(combo_discard_survival_rate, "changed", deactivate_Survivability_unused_params)
for (choice in c("YES", "NO")) {
combo_discard_survival_rate$appendText(choice)
}

hbox_DiscardSurvivability$packStart(combo_discard_survival_rate, expand = FALSE, fill = FALSE, padding = 5)

lbl_option_survivability <- gtkLabel(" Options for Discard Survavibility ")
gtkLabelSetWidthChars(lbl_option_survivability, 40)   
lbl_option_survivability_param1_males <- gtkLabel("L50% [mm]") 
lbl_option_survivability_param2_females <- gtkLabel("L75%L25% [mm]") 
gtkLabelSetWidthChars(lbl_option_survivability_param1_males, LABEL_LENGTH)  
gtkLabelSetWidthChars(lbl_option_survivability_param2_females, LABEL_LENGTH)  



radio_survivability_ogive <- gtkRadioButton()
radio_survivability_ogive$add(gtkLabel("Ogive depending on size"))
radio_survivability_constant <- gtkRadioButtonNewWithLabelFromWidget(radio_survivability_ogive, "Constant")

entry_survivability_param1_males <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_param1_males, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_param1_males, 0)
gSignalConnect(entry_survivability_param1_males, "changed", change_survivability_DISCARD_param1_males)

entry_survivability_param2_females <- gtkEntry() 
gtkEntrySetWidthChars(entry_survivability_param2_females, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_survivability_param2_females, 0)  
gSignalConnect(entry_survivability_param2_females, "changed", change_survivability_DISCARD_param2_females)


hbox_DiscardSurvivability$packStart(lbl_option_survivability, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability$packStart(radio_survivability_ogive, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability$packStart(radio_survivability_constant, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability$packStart(lbl_option_survivability_param1_males, expand = F, fill = F, padding = 25)
hbox_DiscardSurvivability$packStart(entry_survivability_param1_males, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability$packStart(lbl_option_survivability_param2_females, expand = F, fill = F, padding = 15)
hbox_DiscardSurvivability$packStart(entry_survivability_param2_females, expand = F, fill = F, padding = 15)


gtkComboBoxSetActive(combo_discard_survival_rate, 0)
gSignalConnect(radio_survivability_ogive, "toggled", deactive_survivability_O_C)
gSignalConnect(radio_survivability_constant, "toggled", deactive_survivability_O_C)


frame_discard_surv <- gtkFrame(" DISCARD SURVIVABILITY ")   
hbox_discard_surv <- gtkHBox(homogeneous = FALSE, 5)
vbox_discard_surv <- gtkVBox(homogeneous = FALSE, 5)             

hbox_discard_surv$packStart(hbox_DiscardSurvivability, expand = TRUE, fill = TRUE, padding = 10)
vbox_discard_surv$packStart(hbox_discard_surv, expand = TRUE, fill = TRUE, padding = 10)
frame_discard_surv$add(vbox_discard_surv) 

h_frame_discard_surv <- gtkHBox(homogeneous = FALSE, 5)
h_frame_discard_surv$packStart(frame_discard_surv, expand = TRUE, fill = TRUE, padding = 5)		

v_frame_discard_surv <- gtkVBox(homogeneous = FALSE, 5)
v_frame_discard_surv$packStart(h_frame_discard_surv, expand = TRUE, fill = T, padding = 5)

vbox_box_DiscardSurvivalRate$packStart(v_frame_discard_surv, expand = F, fill = FALSE, padding = 5)