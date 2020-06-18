# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vboxSEXRATIO <- gtkVBox(FALSE, 5)

hboxSEXRATIO <- gtkHBox(FALSE, 5)
hboxSEXRATIO$packStart(gtkLabel("Sex ratio F/F+M"), expand = FALSE, fill = FALSE, padding = 5) 

entry_SR_value <- gtkEntry() 
gtkEntrySetWidthChars(entry_SR_value, NUMERICAL_ENTRY_LENGTH)  

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
if (IN_BEMTOOL) {    
gtkEntrySetText(entry_SR_value, as.numeric(as.character(Populations[[ALADYM_spe]]@sexratio)))
gtkEntrySetEditable(entry_SR_value, FALSE)
} else {
gtkEntrySetText(entry_SR_value, 0.5 )
}
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
 
hboxSEXRATIO$packStart(entry_SR_value, expand = FALSE, fill = FALSE, padding = 5) 

vboxSEXRATIO$packStart(hboxSEXRATIO, expand = FALSE, fill = FALSE, padding = 5)
