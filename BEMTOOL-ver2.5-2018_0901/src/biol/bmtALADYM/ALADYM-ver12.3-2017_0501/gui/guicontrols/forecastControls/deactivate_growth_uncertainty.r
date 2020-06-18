# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_growth_uncertainty <-function(w) {

gtkWidgetSetSensitive(vbox_global_growth_uncert, T)
gtkToggleButtonSetActive(chkConfidenceIntervals_fore_M, T)
 
if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore_crescita)) {
gtkWidgetSetSensitive(vbox_global_growth_uncert, T)
gtkToggleButtonSetActive(chkConfidenceIntervals_fore_M, F)
}  else { 
gtkWidgetSetSensitive(vbox_global_growth_uncert, F) 
gtkToggleButtonSetActive(chkConfidenceIntervals_fore_M, F)
}

}





deactivate_growth_uncertainty_Linf_k <-function(w) {

gtkWidgetSetSensitive(h_frame_growth_uncert_Linf, T)
gtkWidgetSetSensitive(h_frame_growth_uncert_k, T)
 
if (gtkToggleButtonGetActive(radio_growth_uncert_Linf)) {
gtkWidgetSetSensitive(h_frame_growth_uncert_Linf, T)
gtkWidgetSetSensitive(h_frame_growth_uncert_k, F)
}  else { 
gtkWidgetSetSensitive(h_frame_growth_uncert_Linf, F)
gtkWidgetSetSensitive(h_frame_growth_uncert_k, T)
}

}


 deactivate_growth_uncertainty_Linf_distr_extfile <-function(w) {

gtkWidgetSetSensitive(combo_distr_growth_uncert_Linf, T)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_Linf, T)
gtkWidgetSetSensitive(btn_load_growth_uncert_Linf_from_file, T)
gtkWidgetSetSensitive(growth_uncert_Linf_from_file_sw, T)
 
if (gtkToggleButtonGetActive(radio_growth_uncert_Linf_from_distribution)) {
gtkWidgetSetSensitive(combo_distr_growth_uncert_Linf, T)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_Linf, T)
gtkWidgetSetSensitive(btn_load_growth_uncert_Linf_from_file, F)
gtkWidgetSetSensitive(growth_uncert_Linf_from_file_sw, F)
}  else { 
gtkWidgetSetSensitive(combo_distr_growth_uncert_Linf, F)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_Linf, F)
gtkWidgetSetSensitive(btn_load_growth_uncert_Linf_from_file, T)
gtkWidgetSetSensitive(growth_uncert_Linf_from_file_sw, T)
}

}



 deactivate_growth_uncertainty_k_distr_extfile <-function(w) {

gtkWidgetSetSensitive(combo_distr_growth_uncert_k, T)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_k, T)
gtkWidgetSetSensitive(btn_load_growth_uncert_k_from_file, T)
gtkWidgetSetSensitive(growth_uncert_k_from_file_sw, T)
 
if (gtkToggleButtonGetActive(radio_growth_uncert_k_from_distribution)) {
gtkWidgetSetSensitive(combo_distr_growth_uncert_k, T)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_k, T)
gtkWidgetSetSensitive(btn_load_growth_uncert_k_from_file, F)
gtkWidgetSetSensitive(growth_uncert_k_from_file_sw, F)
}  else { 
gtkWidgetSetSensitive(combo_distr_growth_uncert_k, F)
gtkWidgetSetSensitive(vbox_growth_mean_devSt_k, F)
gtkWidgetSetSensitive(btn_load_growth_uncert_k_from_file, T)
gtkWidgetSetSensitive(growth_uncert_k_from_file_sw, T)
}

}




change_label_growthUncert_Linf_distribution <- function(w) {       
    
  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_distr_growth_uncert_Linf)
select_index <- which(DISTRIBUTION_UNCERT == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_distr_growth_uncert_Linf_txt <- "Mean ln(x)"
          lbl_B_distr_growth_uncert_Linf_txt <- "Ds ln(x)"          

      } else if (select_index == 2) {
          lbl_A_distr_growth_uncert_Linf_txt <- "Mean (x)"
          lbl_B_distr_growth_uncert_Linf_txt <- "Ds (x)"

      } else {

          lbl_A_distr_growth_uncert_Linf_txt <- "Min"
          lbl_B_distr_growth_uncert_Linf_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_distr_growth_uncert_Linf, lbl_A_distr_growth_uncert_Linf_txt)
gtkLabelSetText(lbl_B_distr_growth_uncert_Linf, lbl_B_distr_growth_uncert_Linf_txt)
     }
     
     
     
change_label_growthUncert_k_distribution <- function(w) {       
    
  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_distr_growth_uncert_k)
select_index <- which(DISTRIBUTION_UNCERT == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_distr_growth_uncert_k_txt <- "Mean ln(x)"
          lbl_B_distr_growth_uncert_k_txt <- "Ds ln(x)"          

      } else if (select_index == 2) {
          lbl_A_distr_growth_uncert_k_txt <- "Mean (x)"
          lbl_B_distr_growth_uncert_k_txt <- "Ds (x)"

      } else {

          lbl_A_distr_growth_uncert_k_txt <- "Min"
          lbl_B_distr_growth_uncert_k_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_distr_growth_uncert_k, lbl_A_distr_growth_uncert_k_txt)
gtkLabelSetText(lbl_B_distr_growth_uncert_k, lbl_B_distr_growth_uncert_k_txt)
     }

