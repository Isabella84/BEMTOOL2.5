# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


loadF_overallfromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_F_O <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading overall Fishing mortality...        ")

# loaded_F_O <- read.csv( "C:\\ALADYM-ver10.1.4-2015\\caso_DPS_GSA18\\Overall F.csv", sep=";", na.strings = "")  

n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))

first_age_mal <- 0
first_age_fem <- 0

    n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
    n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 

FF_overall_mat <- data.frame(matrix(NA, nrow=length(years), ncol =(length(c(first_age_fem:(n_ages_F+first_age_fem-1)))+1) ))
FM_overall_mat <- data.frame(matrix(NA, nrow=length(years), ncol=(length(c(first_age_mal:(n_ages_M+first_age_mal-1)))+1) ))

colnames(FM_overall_mat) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
FM_overall_mat$year <- years
colnames(FF_overall_mat) <- c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
FF_overall_mat$year <- years

for (ye in 1:length(years) ) {
     FM_overall_mat[FM_overall_mat$year == years[ye], 2:ncol(FM_overall_mat)] <-  loaded_F_O[loaded_F_O$Sex == "M",ye+1]
          FF_overall_mat[FF_overall_mat$year == years[ye], 2:ncol(FF_overall_mat)] <-  loaded_F_O[loaded_F_O$Sex == "F",ye+1]
}

 new_aldSimulation@fishingmortality.overall.F  <<- FF_overall_mat 
new_aldSimulation@fishingmortality.overall.M    <<- FM_overall_mat 

FF_overall_matrix <<- FF_overall_mat
FM_overall_matrix <<- FM_overall_mat

# loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])
reload_fishingmortalityF_overall()
reload_fishingmortalityM_overall()

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Fishing mortality loaded!        ")

} 
}
