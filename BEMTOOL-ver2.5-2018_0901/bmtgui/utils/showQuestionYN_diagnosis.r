# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




showQuestionYN_diagnosis<-function() {

error_window <<- gtkWindow(show=FALSE)       
error_window["title"] <- "BEMTOOL 2.0"  
gtkWindowSetModal(error_window, TRUE)    
gtkWindowSetResizable(error_window, FALSE)
gtkWindowSetDeletable(error_window, T)                
gtkWindowSetDefaultSize(error_window, 300, 20)
gtkWindowSetPosition(error_window, 3)   
  
no_button <- gtkButton("         No         ")  
no_button$AddCallback("clicked", QuestionYN_diagnosis_NO)

yes_button <- gtkButton("         Yes         ") 
yes_button$AddCallback("clicked", QuestionYN_diagnosis_YES)

vbox <- gtkVBox()
hbox1 <- gtkHBox()
hbox1$PackStart(gtkLabel("Reload the DIAGNOSIS found for this case study?\n\nPress YES to load existing Diagnosis, NO to run a new Diagnosis."), expand = TRUE, fill = T, padding = 40)
hbox2 <- gtkHBox(homogeneous = F)
hbox2$PackStart(yes_button, expand = F, fill = T, padding = 10)  
hbox2$PackStart(no_button, expand = F, fill = T, padding = 10) 
hbox_buttons <- gtkHBox(homogeneous = F)
hbox_buttons$PackStart(hbox2, expand =T, fill = F, padding = 10) 
vbox$PackStart(hbox1, expand = F, fill = FALSE, padding = 20)
vbox$PackStart(hbox_buttons, expand = F, fill = FALSE, padding = 20)
error_window$add(vbox)
error_window$show()
#     print("dentro showQuestionYN_diagnosis")
#print(BMTnotebook)
#return(w)

}


QuestionYN_diagnosis_NO<-function(w) {
     error_window$destroy()
}



QuestionYN_diagnosis_YES<-function(w) {
     error_window$destroy()
#print("dentro QuestionYN_diagnosis_YES")
#print(BMTnotebook)
EX_DIAGNOSIS <<- TRUE
  goNextTab()
    # suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/reload_diagnosis.r", sep="")))	     	
#print(BMTnotebook)
#gtkLabelSetText(lbl_current_step, " Step 5 of 9 ")  
#gtkNotebookSetCurrentPage(BMTnotebook, 4)   
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), T)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
#gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
#gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
#gtkWidgetSetSensitive(btn_load_LOADSCENARIO, T) 
     
}


