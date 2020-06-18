# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 change_CI_input_fore <- function(w) {     
                                      
 gtkWidgetSetSensitive(lbl_errorsource_ci_fore, T)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution_fore, T)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file_fore, T)
     gtkWidgetSetSensitive(hboxNumRUNSCI_fore, T)
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)
        gtkWidgetSetSensitive(lbl_errortype_ci_fore, T)
         gtkWidgetSetSensitive(hboxErrorType_fore, T)

    
     if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore)) {
 gtkWidgetSetSensitive(lbl_errorsource_ci_fore, T)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution_fore, T)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file_fore, T)
     gtkWidgetSetSensitive(hboxNumRUNSCI_fore, T)
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)
        gtkWidgetSetSensitive(lbl_errortype_ci_fore, T)
         gtkWidgetSetSensitive(hboxErrorType_fore, T)
         
         gtkToggleButtonSetActive(radio_recruitment_error_distribution_fore, T)
         
          } else {
 gtkWidgetSetSensitive(lbl_errorsource_ci_fore, F)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution_fore, F)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file_fore, F)
     gtkWidgetSetSensitive(hboxNumRUNSCI_fore, F)
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, F)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, F)
        gtkWidgetSetSensitive(lbl_errortype_ci_fore, F)
         gtkWidgetSetSensitive(hboxErrorType_fore, F)             
            }                      
    
    change_CI_source_err_fore()
    
    }
    
    
 change_CI_source_err_fore <- function(w) {     
                                      
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)

     if (gtkToggleButtonGetActive(radio_recruitment_error_ext_file_fore)) {
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, F)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)
          } else {
      gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, T)
       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, F)           
            }                      
    }  