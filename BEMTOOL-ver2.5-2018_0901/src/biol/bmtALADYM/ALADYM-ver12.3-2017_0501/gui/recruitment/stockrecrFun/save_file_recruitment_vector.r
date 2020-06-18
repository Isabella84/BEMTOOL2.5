# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
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
save_file_recruitment_vector <- function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

save_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving recruitment...        ")

# create pproduction table 
recruitment_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(recruitment_table) <- c("year","seed", MONTHS)
recruitment_table$year <- years
recruitment_table$seed[1] <- gtkEntryGetText(entry_OFFSPRING_seedvalue)
     
for (i in 1:length(recruitments)) {
  for (m in 3:14) {
      recruitment_table[as.character(recruitment_table$year) == as.character(recruitments[[i]]$year),m] <-  recruitments[[i]][m-1]
  }
} 

write.table(recruitment_table, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment saved!        ")


} 


}

