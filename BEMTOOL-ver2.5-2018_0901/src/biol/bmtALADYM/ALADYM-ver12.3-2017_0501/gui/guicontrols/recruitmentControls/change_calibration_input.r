# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




change_calibration_input <-function(w){
          gtkWidgetSetSensitive(entry_maxrec, TRUE)
          gtkWidgetSetSensitive(entry_minrec, TRUE)
          gtkWidgetSetSensitive(lbl_min_rec, TRUE)
          gtkWidgetSetSensitive(lbl_max_rec, TRUE)

      if (gtkToggleButtonGetActive(chkCalibration)) {
          gtkWidgetSetSensitive(entry_maxrec, TRUE)
          gtkWidgetSetSensitive(entry_minrec, TRUE)
          gtkWidgetSetSensitive(lbl_min_rec, TRUE)
          gtkWidgetSetSensitive(lbl_max_rec, TRUE)
      } else {
          gtkWidgetSetSensitive(entry_maxrec, FALSE)
          gtkWidgetSetSensitive(entry_minrec, FALSE)
          gtkWidgetSetSensitive(lbl_min_rec, FALSE)
          gtkWidgetSetSensitive(lbl_max_rec, FALSE)
      } 
}
