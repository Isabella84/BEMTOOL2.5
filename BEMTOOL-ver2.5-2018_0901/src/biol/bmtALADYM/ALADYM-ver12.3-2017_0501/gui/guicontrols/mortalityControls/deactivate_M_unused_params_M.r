# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





deactivate_M_unused_params_M <-function(w){
 select_index = -1
  gtkWidgetSetSensitive(lbl_Mconstant_M, TRUE)
 gtkWidgetSetSensitive(entryMconstant_M, TRUE)
  gtkWidgetSetSensitive(lbl_Mvectorfile_M, TRUE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, TRUE)
  gtkWidgetSetSensitive(Mvector_M.treeview, TRUE)
   gtkWidgetSetSensitive(  btn_browse_Msave_M, TRUE)
              gtkLabelSetText(lbl_Mconstant_M, "Constant")
              
selected <- gtkComboBoxGetActiveText(combo_Mtype_M)
select_index <- which(MORTALITY_TYPE == selected )
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (select_index == 1 ) {     # "M constant"
   gtkWidgetSetSensitive(lbl_Mvectorfile_M, FALSE)
   gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, FALSE)
   gtkWidgetSetSensitive(Mvector_M.treeview, FALSE)
   gtkWidgetSetSensitive(  btn_browse_Msave_M, FALSE)   
     gtkLabelSetText(lbl_Mconstant_M, "Constant")
} else if (select_index == 2) {   #  "Chen&Watanabe" 
    gtkWidgetSetSensitive(lbl_Mvectorfile_M, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, FALSE)
   gtkWidgetSetSensitive(lbl_Mconstant_M, FALSE)
 gtkWidgetSetSensitive(entryMconstant_M, FALSE) 
   gtkWidgetSetSensitive(Mvector_M.treeview, FALSE)
   gtkWidgetSetSensitive(  btn_browse_Msave_M, FALSE)
   } else if (select_index == 3) {   #  "From vector" 
  gtkWidgetSetSensitive(lbl_Mconstant_M, FALSE)
 gtkWidgetSetSensitive(entryMconstant_M, FALSE)
} else if (select_index == 4) {    #  "ProdbiomUniqueSolution" 
  gtkWidgetSetSensitive(lbl_Mvectorfile_M, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, FALSE)
   gtkWidgetSetSensitive(Mvector_M.treeview, FALSE)
       gtkWidgetSetSensitive(  btn_browse_Msave_M, FALSE)
       gtkLabelSetText(lbl_Mconstant_M, "Mtmax")
} else if (select_index == 5) {   # "Gislason"
    gtkWidgetSetSensitive(lbl_Mvectorfile_M, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, FALSE)
   gtkWidgetSetSensitive(lbl_Mconstant_M, FALSE)
 gtkWidgetSetSensitive(entryMconstant_M, FALSE) 
    gtkWidgetSetSensitive(Mvector_M.treeview, FALSE)
    gtkWidgetSetSensitive(  btn_browse_Msave_M, FALSE)
}                     

}