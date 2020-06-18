# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_maturity_uncertainty <-function(w) {

gtkWidgetSetSensitive(vbox_global_maturity_uncert, T)

if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore_Maturity)) {
gtkWidgetSetSensitive(vbox_global_maturity_uncert, T)
}  else { 
gtkWidgetSetSensitive(vbox_global_maturity_uncert, F) 
}

}



 deactivate_maturity_uncertainty_males_distr_extfile <-function(w) {

gtkWidgetSetSensitive(combo_distr_maturity_uncert_males, T)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_males, T)
gtkWidgetSetSensitive(btn_load_maturity_uncert_males_from_file, T)
gtkWidgetSetSensitive(maturity_uncert_males_from_file_sw, T)
 
if (gtkToggleButtonGetActive(radio_maturity_uncert_males_from_distribution)) {
gtkWidgetSetSensitive(combo_distr_maturity_uncert_males, T)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_males, T)
gtkWidgetSetSensitive(btn_load_maturity_uncert_males_from_file, F)
gtkWidgetSetSensitive(maturity_uncert_males_from_file_sw, F)
}  else { 
gtkWidgetSetSensitive(combo_distr_maturity_uncert_males, F)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_males, F)
gtkWidgetSetSensitive(btn_load_maturity_uncert_males_from_file, T)
gtkWidgetSetSensitive(maturity_uncert_males_from_file_sw, T)
}

}



deactivate_maturity_uncertainty_females_distr_extfile <-function(w) {

gtkWidgetSetSensitive(combo_distr_maturity_uncert_females, T)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_females, T)
gtkWidgetSetSensitive(btn_load_maturity_uncert_females_from_file, T)
gtkWidgetSetSensitive(maturity_uncert_females_from_file_sw, T)
 
if (gtkToggleButtonGetActive(radio_maturity_uncert_females_from_distribution)) {
gtkWidgetSetSensitive(combo_distr_maturity_uncert_females, T)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_females, T)
gtkWidgetSetSensitive(btn_load_maturity_uncert_females_from_file, F)
gtkWidgetSetSensitive(maturity_uncert_females_from_file_sw, F)
}  else { 
gtkWidgetSetSensitive(combo_distr_maturity_uncert_females, F)
gtkWidgetSetSensitive(vbox_maturity_mean_devSt_females, F)
gtkWidgetSetSensitive(btn_load_maturity_uncert_females_from_file, T)
gtkWidgetSetSensitive(maturity_uncert_females_from_file_sw, T)
}

}



change_label_maturity_uncert_distribution_males <- function(w) {       
    
  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_distr_maturity_uncert_males)
select_index <- which(DISTRIBUTION_UNCERT == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_distr_maturity_uncert_males_txt <- "Mean ln(x)"
          lbl_B_distr_maturity_uncert_males_txt <- "Ds ln(x)"          

      } else if (select_index == 2) {
          lbl_A_distr_maturity_uncert_males_txt <- "Mean (x)"
          lbl_B_distr_maturity_uncert_males_txt <- "Ds (x)"

      } else {

          lbl_A_distr_maturity_uncert_males_txt <- "Min"
          lbl_B_distr_maturity_uncert_males_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_distr_maturity_uncert_males, lbl_A_distr_maturity_uncert_males_txt)
gtkLabelSetText(lbl_B_distr_maturity_uncert_males, lbl_B_distr_maturity_uncert_males_txt)
     }
     
     
     
change_label_maturity_uncert_distribution_females <- function(w) {       
    
  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_distr_maturity_uncert_females)
select_index <- which(DISTRIBUTION_UNCERT == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_distr_maturity_uncert_females_txt <- "Mean ln(x)"
          lbl_B_distr_maturity_uncert_females_txt <- "Ds ln(x)"          

      } else if (select_index == 2) {
          lbl_A_distr_maturity_uncert_females_txt <- "Mean (x)"
          lbl_B_distr_maturity_uncert_females_txt <- "Ds (x)"

      } else {

          lbl_A_distr_maturity_uncert_females_txt <- "Min"
          lbl_B_distr_maturity_uncert_females_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_distr_maturity_uncert_females, lbl_A_distr_maturity_uncert_females_txt)
gtkLabelSetText(lbl_B_distr_maturity_uncert_females, lbl_B_distr_maturity_uncert_females_txt)
     }

