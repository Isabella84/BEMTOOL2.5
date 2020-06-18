# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
change_maturityogive_L75L25_F <-function(w){
          gtkWidgetSetSensitive(lbl_A_maturityogive_L75L25_F, TRUE)
          gtkWidgetSetSensitive(entryOGIVEL75L25_F_a, TRUE)
          gtkWidgetSetSensitive(lbl_B_maturityogive_L75L25_F, TRUE)
          gtkWidgetSetSensitive(entryOGIVEL75L25_F_b, TRUE)

  select_index = -1
  selected <- gtkComboBoxGetActiveText(combo_L75L25dis_F)
select_index <- which(DISTRIBUTION == selected )          #DISTRIBUTION <- c("Lognormal","Gamma","Normal","Uniform")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

      if (select_index == 1) {
          lbl_A_maturityogive_txt <- "Mean ln(x)"
          lbl_B_maturityogive_txt <- "Ds ln(x)"
      } else if (select_index == 2) {
          lbl_A_maturityogive_txt <- "Shape"
          lbl_B_maturityogive_txt <- "Scale"
      } else if (select_index == 3) {
          lbl_A_maturityogive_txt <- "Mean (x)"
          lbl_B_maturityogive_txt <- "Ds (x)"
      } else {
          gtkWidgetSetSensitive(lbl_A_maturityogive_L75L25_F, FALSE)
          gtkWidgetSetSensitive(entryOGIVEL75L25_F_a, FALSE)
          gtkWidgetSetSensitive(lbl_B_maturityogive_L75L25_F, FALSE)
          gtkWidgetSetSensitive(entryOGIVEL75L25_F_b, FALSE)
          lbl_A_maturityogive_txt <- "A"
          lbl_B_maturityogive_txt <- "B"
      }  
gtkLabelSetText(lbl_A_maturityogive_L75L25_F, lbl_A_maturityogive_txt)
gtkLabelSetText(lbl_B_maturityogive_L75L25_F, lbl_B_maturityogive_txt)
}