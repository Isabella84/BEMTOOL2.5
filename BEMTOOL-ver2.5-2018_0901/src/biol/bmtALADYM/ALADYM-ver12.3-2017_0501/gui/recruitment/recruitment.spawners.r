# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vboxSpawners <- gtkVBox(FALSE, 5)

hboxSS <- gtkHBox(FALSE, 5)
hboxSS$packStart(gtkLabel("Select SS"), expand = FALSE, fill = FALSE, padding = 5)

combo_SS <- gtkComboBoxNewText()
#gSignalConnect(combo_L50dis_M, "changed", deactivate_unused_params)
for (choice in SS_TYPE) {  combo_SS$appendText(choice) }
gtkComboBoxSetActive(combo_SS, 0 )

hboxSS$packStart(combo_SS, expand = FALSE, fill = FALSE, padding = 5)

# vboxSpawners$packStart(hboxSS, expand = FALSE, fill = FALSE, padding = 5)

#hboxdelaySS <- gtkHBox(FALSE, 5)
hboxSS$packStart(gtkLabel("Delay for SS calculation"), expand = FALSE, fill = FALSE, padding =10)
entry_delaySS <- gtkEntry()
gtkEntrySetWidthChars(entry_delaySS, NUMERICAL_ENTRY_LENGTH)
hboxSS$packStart(entry_delaySS, expand = FALSE, fill = FALSE, padding = 5)

vboxSpawners$packStart(hboxSS, expand = FALSE, fill = FALSE, padding = 5)
