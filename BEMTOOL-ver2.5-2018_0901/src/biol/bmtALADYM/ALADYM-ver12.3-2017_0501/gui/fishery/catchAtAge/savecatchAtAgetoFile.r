# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




savecatchAtAgetoFile <- function(w) {
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
wnd <- showMessage("        Saving catch at age...        ")

n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))

first_age_mal <- 0
first_age_fem <- 0

   n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 

all_catchAtAge <- data.frame(matrix(nrow=(n_ages_M*length(years) + n_ages_F*length(years)), ncol=3+length(FLEETSEGMENTS_names) ))
colnames(all_catchAtAge) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex") 

    vect_year <- data.frame(matrix(nrow=0, ncol=1))
        vect_sex <- data.frame(matrix(nrow=0, ncol=1))
            vect_ages <- data.frame(matrix(nrow=0, ncol=1))
#    if (exists("vect_year") ) { rm(vect_year) }
#    if (exists("vect_sex") ) { rm(vect_sex) }
#    if (exists("vect_ages") ) { rm(vect_ages) }

 for (yy in 1:length(years) ) { 
vect_year <- rbind(vect_year, matrix(rep(years[yy], (n_ages_M+n_ages_F)), ncol=1) )
vect_sex <- rbind(vect_sex,   matrix(c(rep("M", n_ages_M) , rep("F", n_ages_F)), ncol=1) )
vect_ages <- rbind(vect_ages,  matrix(c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1))), ncol=1) )
 }
    
 all_catchAtAge$Age <- vect_ages[,1]
  all_catchAtAge$Year <- vect_year[,1]  
    all_catchAtAge$Sex <- vect_sex[,1]     
 for (yy in 1:length(years) ) { 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
fs_object <- FleetList_simulation[[fs]]        
dataframe_F_M <- fs_object@catchAtAge.M.vector
dataframe_F_F <- fs_object@catchAtAge.F.vector
                                                                                        
all_catchAtAge[all_catchAtAge$Year == years[yy] & all_catchAtAge$Sex == "M", (2+fs) ] <- as.numeric(as.character(dataframe_F_M[yy,2:(n_ages_M+1)]))
all_catchAtAge[all_catchAtAge$Year == years[yy] & all_catchAtAge$Sex == "F", (2+fs) ] <- as.numeric(as.character(dataframe_F_F[yy,2:(n_ages_F+1)]))
 }
}
#write.table(all_Fmortalities, "C:\\test.csv",  sep=";", na = "",row.names = FALSE)
write.table(all_catchAtAge, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Catch at age saved!        ")


} 
}