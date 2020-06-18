# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_M_unused_params_F <-function(w){
 select_index = -1
  gtkWidgetSetSensitive(lbl_Mconstant_F, TRUE)
 gtkWidgetSetSensitive(entryMconstant_F, TRUE)
  gtkWidgetSetSensitive(lbl_Mvectorfile_F, TRUE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, TRUE)
   gtkWidgetSetSensitive(Mvector_F.treeview, TRUE)
      gtkWidgetSetSensitive(  btn_browse_Msave_F, TRUE)
                 gtkLabelSetText(lbl_Mconstant_F, "Constant")
                 
selected <- gtkComboBoxGetActiveText(combo_Mtype_F)
select_index <- which(MORTALITY_TYPE == selected )
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (select_index == 1 ) {      # "M constant"
  gtkWidgetSetSensitive(lbl_Mvectorfile_F, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, FALSE)
   gtkWidgetSetSensitive(Mvector_F.treeview, FALSE)
       gtkWidgetSetSensitive(  btn_browse_Msave_F, FALSE)
              gtkLabelSetText(lbl_Mconstant_F, "Constant")
} else if (select_index == 2) {   #  "Chen&Watanabe" 
    gtkWidgetSetSensitive(lbl_Mvectorfile_F, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, FALSE)
   gtkWidgetSetSensitive(lbl_Mconstant_F, FALSE)
 gtkWidgetSetSensitive(entryMconstant_F, FALSE) 
    gtkWidgetSetSensitive(Mvector_F.treeview, FALSE)
    gtkWidgetSetSensitive(  btn_browse_Msave_F, FALSE)
} else if (select_index == 3) {    #  "From vector" 
  gtkWidgetSetSensitive(lbl_Mconstant_F, FALSE)
 gtkWidgetSetSensitive(entryMconstant_F, FALSE)
} else if (select_index == 4) {    #  "ProdbiomUniqueSolution" 
  gtkWidgetSetSensitive(lbl_Mvectorfile_F, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, FALSE)
   gtkWidgetSetSensitive(Mvector_F.treeview, FALSE)
       gtkWidgetSetSensitive(  btn_browse_Msave_F, FALSE)
       gtkLabelSetText(lbl_Mconstant_F, "Mtmax")
} else if (select_index == 5) {   # "Gislason"
    gtkWidgetSetSensitive(lbl_Mvectorfile_F, FALSE)
 gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, FALSE)
   gtkWidgetSetSensitive(lbl_Mconstant_F, FALSE)
 gtkWidgetSetSensitive(entryMconstant_F, FALSE) 
    gtkWidgetSetSensitive(Mvector_F.treeview, FALSE)
    gtkWidgetSetSensitive(  btn_browse_Msave_F, FALSE)
}                     


}
