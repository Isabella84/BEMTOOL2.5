# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#loadFfromFile<-function(w) {
#dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
#if (dialog$run() == GtkResponseType["accept"]) {
#
#loaded_F <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
loadSelectivityAgefromFile_fore<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_SelectivityAge_fore <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading Selectivity by age for forecast...        ")

# loaded_F <- read.csv( "C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\input\\ALADYM fishing mortality - MERLMER.csv", sep=";", na.strings = "")  
 
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages_F <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan)) 
n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))    
} else {
  n_ages_F <- as.numeric(new_aldPopulation@lifespan[2,1])     
n_ages_M <- as.numeric(new_aldPopulation@lifespan[1,1])    
} 


 
#n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
#n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))  

first_age_mal <- 0
first_age_fem <- 0

#if (modulo(Tr, 12) == 0 & Tr!=0) {
#    n_ages_M <- n_ages_M - trunc(Tr/12) + 1
#    first_age_mal <- trunc(Tr/12) - 1
#    n_ages_F <- n_ages_F - trunc(Tr/12) + 1
#    first_age_fem <- trunc(Tr/12) - 1
#} else if (trunc(Tr/12) > 0 & Tr!=0) {
    n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
    n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)
#} 

for (fs in 1:length(FLEETSEGMENTS_names) ) {
SelectivityAge_fs_fore <- loaded_SelectivityAge_fore[,colnames(loaded_SelectivityAge_fore) == "Age" | colnames(loaded_SelectivityAge_fore) == "Year" | colnames(loaded_SelectivityAge_fore) == "Sex" | colnames(loaded_SelectivityAge_fore) == paste("fs", fs, sep="")]  # c(1, (fs+1), (length(FLEETSEGMENTS_names)+2))

   dataframe_SelAge_M_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(n_ages_M+1)))
   colnames(dataframe_SelAge_M_fore) <-  c("year",paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
   dataframe_SelAge_F_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(n_ages_F+1)))
   colnames(dataframe_SelAge_F_fore) <-  c("year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
     
       for (ye in 1:length(years_forecast)) { 
       dataframe_SelAge_M_fore[ye, ] <-  c(years_forecast[ye], SelectivityAge_fs_fore[SelectivityAge_fs_fore$Sex == "M" & SelectivityAge_fs_fore$Year == years_forecast[ye], 3] )
         dataframe_SelAge_F_fore[ye, ] <- c(years_forecast[ye],  SelectivityAge_fs_fore[SelectivityAge_fs_fore$Sex == "F" & SelectivityAge_fs_fore$Year == years_forecast[ye], 3] ) 
  }
     
          colnames(dataframe_SelAge_M_fore) <-  c("Year",paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
         colnames(dataframe_SelAge_F_fore) <-  c("Year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )

      # dataframe_SelAge_M[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="M",2] 
#        dataframe_SelAge_F[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="F",2] 
         
 FleetList_forecast[[fs]]@SelectivityAge.M.vector <<- dataframe_SelAge_M_fore
 FleetList_forecast[[fs]]@SelectivityAge.F.vector <<- dataframe_SelAge_F_fore
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Selectivity by Age for forecast loaded!        ")

} 
}
