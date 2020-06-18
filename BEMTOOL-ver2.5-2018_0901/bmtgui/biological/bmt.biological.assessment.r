# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




combo_assessment_tool <- gtkComboBoxNewText()
gSignalConnect(combo_assessment_tool, "changed", set_assessment_tool)
for (choice in ASSESSMENT_TOOLS) { combo_assessment_tool$appendText(choice) }

BMTnotebook_assessment <<- gtkNotebook()
BMTnotebook_assessment$setTabPos("top")
                            
suppressWarnings(source(paste(getwd(), "/bmtgui/biological/assessment/tab_assessment_VIT.r", sep="")))						
BMTnotebook_assessment$appendPage(vbox_assessment_VIT_outer, gtkLabel(str=" VIT "))

suppressWarnings(source(paste(getwd(), "/bmtgui/biological/assessment/tab_assessment_XSA.r", sep="")))						
BMTnotebook_assessment$appendPage(vbox_assessment_XSA_outer, gtkLabel(str=" XSA "))

suppressWarnings(source(paste(getwd(), "/bmtgui/biological/assessment/tab_assessment_SURBA.r", sep="")))						
BMTnotebook_assessment$appendPage(vbox_assessment_SURBA_outer, gtkLabel(str=" SURBA "))

suppressWarnings(source(paste(getwd(), "/bmtgui/biological/assessment/tab_assessment_externalSource.r", sep="")))						
BMTnotebook_assessment$appendPage(vbox_assessment_Report_outer, gtkLabel(str=" from Report "))

vbox_assessment_combo_1 <- gtkVBox(homogeneous = FALSE, 5)    
vbox_assessment_combo_1$packStart(combo_assessment_tool, expand = F, fill = F, padding = 0)         #true

hbox_assessment_combo <- gtkHBox(homogeneous = FALSE, 5)    
hbox_assessment_combo$packStart(vbox_assessment_combo_1, expand = T, fill = T, padding = 0)         #true
hbox_assessment_combo$packStart(BMTnotebook_assessment, expand = T, fill = T, padding = 0)         #true

vbox_bio_assessment$packStart(hbox_assessment_combo, expand = F, fill = T, padding = 5)         #true
#vbox_bio_assessment$packStart(BMTnotebook_assessment, expand = T, fill = T, padding = 0)         #true


