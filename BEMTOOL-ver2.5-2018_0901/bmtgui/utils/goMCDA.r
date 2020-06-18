# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
#
#
#
#
#
#
#
#
#
goMCDA <- function(w) {

    gtkLabelSetText(lbl_current_step, " Step 9 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 8)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), T)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 

gtkWidgetSetSensitive(btn_next, F)  
       
}

