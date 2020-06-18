# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveF_overalltoFile <- function(w) {
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
wnd <- showMessage("        Saving overall Fishing mortality...        ")

n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))

first_age_mal <- 0
first_age_fem <- 0

    n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 



all_Fmortalities_O <- data.frame(matrix(nrow=(n_ages_M + n_ages_F), ncol=2+length(years) ))
colnames(all_Fmortalities_O) <- c("Age", years, "Sex") 
        
        all_Fmortalities_O$Age <-  matrix(c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1))), ncol=1)
         all_Fmortalities_O$Sex  <- c(rep("M", n_ages_M), rep("F", n_ages_F)) 

  all_Fmortalities_O[all_Fmortalities_O$Sex == "M", 2:(length(years)+1) ] <- t(new_aldSimulation@fishingmortality.overall.M[,2:ncol(new_aldSimulation@fishingmortality.overall.M)] )
    all_Fmortalities_O[all_Fmortalities_O$Sex == "F", 2:(length(years)+1) ] <- t(new_aldSimulation@fishingmortality.overall.F[,2:ncol(new_aldSimulation@fishingmortality.overall.F)] )
      
 
write.table(all_Fmortalities_O, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Overall Fishing mortality saved!        ")


} 
}