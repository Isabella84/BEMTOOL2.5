# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



split_F_button <- function(w) {

wnd <- showMessage("        Splitting F overall...        ")   #  --------------------------------------
      gtkWidgetSetSensitive(main_window, F)
 

       
split_F(new_aldSimulation@Fsplittingtype)      

wnd$destroy()
gtkWidgetSetSensitive(main_window, TRUE)

}    




