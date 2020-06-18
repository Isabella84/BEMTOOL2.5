# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveFtoFile_fore <- function(w) {
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
wnd <- showMessage("        Saving fishing mortality for forecast...        ")

n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))

first_age_mal <- 0
first_age_fem <- 0

    n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
    n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 

all_Fmortalities <- data.frame(matrix(nrow=(n_ages_M*length(years) + n_ages_F*length(years.forecast)), ncol=3+length(FLEETSEGMENTS_names) ))
colnames(all_Fmortalities) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex") 

    vect_year <- data.frame(matrix(nrow=0, ncol=1))
        vect_sex <- data.frame(matrix(nrow=0, ncol=1))
            vect_ages <- data.frame(matrix(nrow=0, ncol=1))
#    if (exists("vect_year") ) { rm(vect_year) }
#    if (exists("vect_sex") ) { rm(vect_sex) }
#    if (exists("vect_ages") ) { rm(vect_ages) }

 for (yy in 1:length(years.forecast) ) { 
vect_year <- rbind(vect_year, matrix(rep(years.forecast[yy], (n_ages_M+n_ages_F)), ncol=1) )
vect_sex <- rbind(vect_sex,   matrix(c(rep("M", n_ages_M) , rep("F", n_ages_F)), ncol=1) )
vect_ages <- rbind(vect_ages,  matrix(c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1))), ncol=1) )
 }
    
 all_Fmortalities$Age <- vect_ages[,1]
  all_Fmortalities$Year <- vect_year[,1]  
    all_Fmortalities$Sex <- vect_sex[,1]     
 for (yy in 1:length(years.forecast) ) { 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
fs_object <- FleetList_simulation[[fs]]        
dataframe_F_M <- fs_object@fishingmortality.M.vector
dataframe_F_F <- fs_object@fishingmortality.F.vector
                                                                                        
all_Fmortalities[all_Fmortalities$Year == years.forecast[yy] & all_Fmortalities$Sex == "M", (2+fs) ] <- as.numeric(as.character(dataframe_F_M[yy,2:(n_ages_M+1)]))
all_Fmortalities[all_Fmortalities$Year == years.forecast[yy] & all_Fmortalities$Sex == "F", (2+fs) ] <- as.numeric(as.character(dataframe_F_F[yy,2:(n_ages_F+1)]))
 }
}
#write.table(all_Fmortalities, "C:\\test.csv",  sep=";", na = "",row.names = FALSE)
write.table(all_Fmortalities, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Fishing mortality for forecast saved!        ")


} 
}