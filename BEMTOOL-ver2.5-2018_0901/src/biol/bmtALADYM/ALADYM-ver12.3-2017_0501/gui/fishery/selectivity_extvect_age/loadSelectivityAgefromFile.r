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
loadSelectivityAgefromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_SelectivityAge <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading Selectivity by age...        ")

# loaded_F <- read.csv( "C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\input\\ALADYM fishing mortality - MERLMER.csv", sep=";", na.strings = "")  
       
n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))  

first_age_mal <- 0
first_age_fem <- 0

    n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
    n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

for (fs in 1:length(FLEETSEGMENTS_names) ) {
   SelectivityAge_fs <- loaded_SelectivityAge[,colnames(loaded_SelectivityAge) == "Age" | colnames(loaded_SelectivityAge) == "Year" | colnames(loaded_SelectivityAge) == "Sex" | colnames(loaded_SelectivityAge) == paste("fs", fs, sep="")]  # c(1, (fs+1), (length(FLEETSEGMENTS_names)+2))

   dataframe_SelAge_M <- data.frame(matrix(nrow=length(years), ncol=(n_ages_M+1)))
   colnames(dataframe_SelAge_M) <-  c("Year",paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
   dataframe_SelAge_F <- data.frame(matrix(nrow=length(years), ncol=(n_ages_F+1)))
   colnames(dataframe_SelAge_F) <-  c("Year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
   
   dataframe_SelAge_M$Year <- years
      dataframe_SelAge_F$Year <- years
     
       for (ye in 1:length(years)) {
       dataframe_SelAge_M[ye, ] <-  c(years[ye], SelectivityAge_fs[SelectivityAge_fs$Sex == "M" & SelectivityAge_fs$Year == years[ye], 3] )
         dataframe_SelAge_F[ye, ] <- c(years[ye],  SelectivityAge_fs[SelectivityAge_fs$Sex == "F" & SelectivityAge_fs$Year == years[ye], 3] ) 
  }
     colnames(dataframe_SelAge_M) <-  c("Year",paste( "age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
         colnames(dataframe_SelAge_F) <-  c("Year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
      # dataframe_SelAge_M[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="M",2] 
#        dataframe_SelAge_F[1,] <- SelectivityAge_fs[ SelectivityAge_fs[ncol(SelectivityAge_fs)]=="F",2] 
         
 FleetList_simulation[[fs]]@SelectivityAge.M.vector <<- dataframe_SelAge_M
 FleetList_simulation[[fs]]@SelectivityAge.F.vector <<- dataframe_SelAge_F
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Selectivity by Age loaded!        ")

} 
}
