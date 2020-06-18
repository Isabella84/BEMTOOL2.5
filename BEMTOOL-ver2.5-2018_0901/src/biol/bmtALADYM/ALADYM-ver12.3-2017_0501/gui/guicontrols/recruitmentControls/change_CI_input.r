# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 change_CI_input <- function(w) {     
                                      
 gtkWidgetSetSensitive(lbl_errorsource_ci, T)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution, T)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file, T)
     gtkWidgetSetSensitive(hboxNumRUNSCI, T)
      gtkWidgetSetSensitive(hboxErrorNoise_dis, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr, T)
        gtkWidgetSetSensitive(lbl_errortype_ci, T)
         gtkWidgetSetSensitive(hboxErrorType, T)

    
     if (gtkToggleButtonGetActive(chkConfidenceIntervals)) {
 gtkWidgetSetSensitive(lbl_errorsource_ci, T)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution, T)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file, T)
     gtkWidgetSetSensitive(hboxNumRUNSCI, T)
      gtkWidgetSetSensitive(hboxErrorNoise_dis, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr, T)
        gtkWidgetSetSensitive(lbl_errortype_ci, T)
         gtkWidgetSetSensitive(hboxErrorType, T)
         
         gtkToggleButtonSetActive(radio_recruitment_error_distribution, T)
         
          } else {
 gtkWidgetSetSensitive(lbl_errorsource_ci, F)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution, F)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file, F)
     gtkWidgetSetSensitive(hboxNumRUNSCI, F)
      gtkWidgetSetSensitive(hboxErrorNoise_dis, F)
       gtkWidgetSetSensitive(hboxExternalRecrErr, F)
        gtkWidgetSetSensitive(lbl_errortype_ci, F)
         gtkWidgetSetSensitive(hboxErrorType, F)             
            }                      
    }
    
    
 change_CI_source_err <- function(w) {     
                                      
      gtkWidgetSetSensitive(hboxErrorNoise_dis, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr, T)

     if (gtkToggleButtonGetActive(radio_recruitment_error_ext_file)) {
      gtkWidgetSetSensitive(hboxErrorNoise_dis, F)
       gtkWidgetSetSensitive(hboxExternalRecrErr, T)
          } else {
      gtkWidgetSetSensitive(hboxErrorNoise_dis, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr, F)           
            }                      
    }  