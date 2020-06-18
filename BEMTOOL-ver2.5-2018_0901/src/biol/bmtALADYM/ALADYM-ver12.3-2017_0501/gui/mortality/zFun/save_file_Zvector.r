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
#
#
#
# ------------------------------------------------------------------------------
# Function for the saving of the vector
# ------------------------------------------------------------------------------
#
save_file_Zvector <- function(w) {
       dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
# create pproduction table 

Z_save_path <- dialog$getFilename()
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving total mortality...        ")


Zvector_F_table <- get_table("TOTAL_MORTALITY_F")
Zvector_F_table$sex="F"

Zvector_M_table <- get_table("TOTAL_MORTALITY_M")
Zvector_M_table$sex="M"

Zvector_table <- rbind(Zvector_F_table, Zvector_M_table)

write.table(Zvector_table, Z_save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Total mortality saved!        ")

} 
}