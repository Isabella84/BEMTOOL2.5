# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



change_noise_AB_fore <-function(w){
          gtkWidgetSetSensitive(lbl_A_noise_fore, TRUE)
          gtkWidgetSetSensitive(entryNoise_a_fore, TRUE)
          gtkWidgetSetSensitive(lbl_B_noise_fore, TRUE)
          gtkWidgetSetSensitive(entryNoise_b_fore, TRUE)
          
#          gtkWidgetSetSensitive(lbl_min_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entry_noise_recr_min_fore, TRUE)
#          gtkWidgetSetSensitive(lbl_max_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entry_noise_recr_max_fore, TRUE)

  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore)
select_index <- which(DISTRIBUTION == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_noise_txt <- "Mean ln(x)"
          lbl_B_noise_txt <- "Ds ln(x)"          
#          gtkWidgetSetSensitive(lbl_min_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_min_fore, FALSE)
#          gtkWidgetSetSensitive(lbl_max_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_max_fore, FALSE)
#                    gtkWidgetSetSensitive(lbl_A_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_a_fore, TRUE)
#          gtkWidgetSetSensitive(lbl_B_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_b_fore, TRUE)
      } else if (select_index == 2) {
          lbl_A_noise_txt <- "Shape"
          lbl_B_noise_txt <- "Scale"
#          gtkWidgetSetSensitive(lbl_min_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_min_fore, FALSE)
#          gtkWidgetSetSensitive(lbl_max_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_max_fore, FALSE)
#                    gtkWidgetSetSensitive(lbl_A_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_a_fore, TRUE)
#          gtkWidgetSetSensitive(lbl_B_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_b_fore, TRUE)
      } else if (select_index == 3) {
          lbl_A_noise_txt <- "Mean (x)"
          lbl_B_noise_txt <- "Ds (x)"
#          gtkWidgetSetSensitive(lbl_min_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_min_fore, FALSE)
#          gtkWidgetSetSensitive(lbl_max_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entry_noise_recr_max_fore, FALSE)
#                    gtkWidgetSetSensitive(lbl_A_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_a_fore, TRUE)
#          gtkWidgetSetSensitive(lbl_B_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entryNoise_b_fore, TRUE)
      } else {
#          gtkWidgetSetSensitive(lbl_min_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entry_noise_recr_min_fore, TRUE)
#          gtkWidgetSetSensitive(lbl_max_noise_fore, TRUE)
#          gtkWidgetSetSensitive(entry_noise_recr_max_fore, TRUE)
#          
#          gtkWidgetSetSensitive(lbl_A_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entryNoise_a_fore, FALSE)
#          gtkWidgetSetSensitive(lbl_B_noise_fore, FALSE)
#          gtkWidgetSetSensitive(entryNoise_b_fore, FALSE)
          lbl_A_noise_txt <- "Min"
          lbl_B_noise_txt <- "Max"
      }  
gtkLabelSetText(lbl_A_noise_fore, lbl_A_noise_txt)
gtkLabelSetText(lbl_B_noise_fore, lbl_B_noise_txt)
}