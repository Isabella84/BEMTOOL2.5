# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 change_CI_input_fore <- function(w) {     
                                      
 gtkWidgetSetSensitive(vbox_recruits_fore_UN, F)  
 gtkWidgetSetSensitive(vbox_recruits_fore, T)   
    
     if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore)) {
 gtkWidgetSetSensitive(vbox_recruits_fore_UN, T)
  gtkWidgetSetSensitive(vbox_recruits_fore, F)

 
          } else {
 gtkWidgetSetSensitive(vbox_recruits_fore_UN, F)  
 gtkWidgetSetSensitive(vbox_recruits_fore, T)      
            }                      
    
    # change_CI_source_err_fore()
    
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
    
    
    
    
    
 change_CI_source_SRR_err <- function(w) {     
                               
      gtkWidgetSetSensitive(vbox_costant_radio_plus_load, T)
       gtkWidgetSetSensitive(vbox_SRR_radio_plus_combo, T)

     if (gtkToggleButtonGetActive(radio_distribution_SRR_uncert)) {
      gtkWidgetSetSensitive(vbox_costant_radio_plus_load, F)
       gtkWidgetSetSensitive(vbox_SRR_radio_plus_combo, T)
          } else {
      gtkWidgetSetSensitive(vbox_costant_radio_plus_load, T)
       gtkWidgetSetSensitive(vbox_SRR_radio_plus_combo, F)           
            }                      
    }  
    
    
    
    
    
 change_CI_Type_Error_Costant_SRR <- function(w) {     
                               
      gtkWidgetSetSensitive(frame_uncert_cost_error, T)
       gtkWidgetSetSensitive(frame_uncert_SRR_error, T)

     if (gtkToggleButtonGetActive(radio_forecast_recruits_relationship_UN)) {
      gtkWidgetSetSensitive(frame_uncert_cost_error, F)
       gtkWidgetSetSensitive(frame_uncert_SRR_error, T)
          } else {
      gtkWidgetSetSensitive(frame_uncert_cost_error, T)
       gtkWidgetSetSensitive(frame_uncert_SRR_error, F)           
            }                      
    }
    
    

        
 change_CI_Type_Error_Costant_betweenDistrOrExtFile <- function(w) {     
                               
   gtkWidgetSetSensitive(btn_load_external_recruitment_err_fore, T)
       gtkWidgetSetSensitive(combo_RecrNoise_dis_fore, T)
        gtkWidgetSetSensitive(tbl_SRR_cost_mean_devSt, T)
         gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)  
                       
     if (gtkToggleButtonGetActive(radio_recruitment_error_distribution_fore)) {
      gtkWidgetSetSensitive(btn_load_external_recruitment_err_fore, F)
       gtkWidgetSetSensitive(combo_RecrNoise_dis_fore, T)
              gtkWidgetSetSensitive(tbl_SRR_cost_mean_devSt, T)
                       gtkWidgetSetSensitive(hboxExternalRecrErr_fore, F)   
          } else {
      gtkWidgetSetSensitive(btn_load_external_recruitment_err_fore, T)
       gtkWidgetSetSensitive(combo_RecrNoise_dis_fore, F) 
          gtkWidgetSetSensitive(tbl_SRR_cost_mean_devSt, F)
          gtkWidgetSetSensitive(hboxExternalRecrErr_fore, T)   
              
            }                      
    }
    
    
    
    
     change_CI_Type_Error_SRR_betweenDistrOrExtFile <- function(w) {     
                               
   gtkWidgetSetSensitive(btn_load_external_SRR_err, T)
       gtkWidgetSetSensitive(combo_hboxSR_Distribution_Uncert_UN, T)
        gtkWidgetSetSensitive(tbl_SRR_a_b_c_params, T)
         gtkWidgetSetSensitive(hboxExternalSRRErr_fore, T)  
                       
     if (gtkToggleButtonGetActive(radio_distribution_SRR_uncert)) {
      gtkWidgetSetSensitive(btn_load_external_SRR_err, F)
       gtkWidgetSetSensitive(combo_hboxSR_Distribution_Uncert_UN, T)
              gtkWidgetSetSensitive(tbl_SRR_a_b_c_params, T)
                       gtkWidgetSetSensitive(hboxExternalSRRErr_fore, F)   
          } else {
      gtkWidgetSetSensitive(btn_load_external_SRR_err, T)
       gtkWidgetSetSensitive(combo_hboxSR_Distribution_Uncert_UN, F) 
          gtkWidgetSetSensitive(tbl_SRR_a_b_c_params, F)
          gtkWidgetSetSensitive(hboxExternalSRRErr_fore, T)   
              
            }                      
    }    
    
    
    
    
    
    
    
change_label_SRR_error_distribution <- function(w) {       
    
  # select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_hboxSR_Distribution_Uncert_UN)
# select_index <- which(DISTRIBUTION_UNCERT == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (selected == "Lognormal") {
          lbl_A_noise_txt <- "Mean ln(x)"
          lbl_B_noise_txt <- "Ds ln(x)"          

      } else if (selected == "Normal") {
          lbl_A_noise_txt <- "Mean (x)"
          lbl_B_noise_txt <- "Ds (x)"

      } else if (selected == "Uniform") {

          lbl_A_noise_txt <- "Min"
          lbl_B_noise_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_noise_fore_UN, lbl_A_noise_txt)
gtkLabelSetText(lbl_B_noise_fore_UN, lbl_B_noise_txt)
     }