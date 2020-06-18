# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



loadEscapeSurvivabilityByAgefromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_SurvivabilityAge <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading Survivability by age for FORECAST...        ")

# loaded_SurvivabilityAge <- read.csv( "C:\\BEMTOOL-ver2.0.6-2015_21042015\\src\\biol\\bmtALADYM\\ALADYM-ver10.1.4-2015_aperto\\caso_HKE_GSA16_discatch\\Escape survivability vector by age.csv", sep=";", na.strings = "")  
       
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages_F <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan)) 
n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))    
} else {
  n_ages_F <- as.numeric(new_aldPopulation@lifespan[2,1])     
n_ages_M <- as.numeric(new_aldPopulation@lifespan[1,1])    
}  

first_age_mal <- 0
first_age_fem <- 0

    n_ages_M <- n_ages_M - trunc(INP$tr/12)
    first_age_mal <- trunc(INP$tr/12)
    n_ages_F <- n_ages_F - trunc(INP$tr/12)
    first_age_fem <- trunc(INP$tr/12)

for (fs in 1:length(FLEETSEGMENTS_names) ) {
   SurvivabilityAge_fs <- loaded_SurvivabilityAge[,colnames(loaded_SurvivabilityAge) == "Age" | colnames(loaded_SurvivabilityAge) == "Sex" | colnames(loaded_SurvivabilityAge) == paste("fs", fs, sep="")]  # c(1, (fs+1), (length(FLEETSEGMENTS_names)+2))

   dataframe_SelAge_M <- data.frame(matrix(nrow=1, ncol=(n_ages_M)))
   colnames(dataframe_SelAge_M) <-  c(paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
   dataframe_SelAge_F <- data.frame(matrix(nrow=1, ncol=(n_ages_F)))
   colnames(dataframe_SelAge_F) <-  c(paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
     
       dataframe_SelAge_M[1, ] <-  c(SurvivabilityAge_fs[SurvivabilityAge_fs$Sex == "M", 2] )
         dataframe_SelAge_F[1, ] <- c( SurvivabilityAge_fs[SurvivabilityAge_fs$Sex == "F", 2] ) 

     colnames(dataframe_SelAge_M) <-  c(paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
         colnames(dataframe_SelAge_F) <-  c(paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
      # dataframe_SelAge_M[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="M",2] 
#        dataframe_SelAge_F[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="F",2] 
         
 FleetList_forecast[[fs]]@escape.survivability.DOS.ext_vect.M <<- dataframe_SelAge_M
 FleetList_forecast[[fs]]@escape.survivability.DOS.ext_vect.F <<- dataframe_SelAge_F
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected )  

escape_surv_extvector_Mtable_fore <<-  FleetList_forecast[[index_to_load]]@escape.survivability.DOS.ext_vect.M 
reload_escape_survival_extvector_M_fore()
escape_surv_extvector_Ftable_fore <<-  FleetList_forecast[[index_to_load]]@escape.survivability.DOS.ext_vect.F
reload_escape_survival_extvector_F_fore()

# loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Survivability by Age loaded!        ")

} 
}
