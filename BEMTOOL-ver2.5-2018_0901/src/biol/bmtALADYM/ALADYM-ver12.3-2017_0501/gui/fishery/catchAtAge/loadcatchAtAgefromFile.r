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
loadcatchAtAgefromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_catchAtAge <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading catch at age...        ")

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
   loaded_catchAtAge_fs <- loaded_catchAtAge[,c(1:2, (fs+2), (length(FLEETSEGMENTS_names)+3))]

   dataframe_F_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
   colnames(dataframe_F_M) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
   dataframe_F_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
   colnames(dataframe_F_F) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
      
   for (yy in 1:length(years) ) {
       loaded_catchAtAge_fs_y <- loaded_catchAtAge_fs[loaded_catchAtAge_fs$Year == years[yy], ]
       to_add <- data.frame(matrix(  loaded_catchAtAge_fs_y[loaded_catchAtAge_fs_y[ncol(loaded_catchAtAge_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_F_M <- rbind(dataframe_F_M, to_add)
       to_add <- data.frame(matrix(  loaded_catchAtAge_fs_y[loaded_catchAtAge_fs_y[ncol(loaded_catchAtAge_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_F_F <- rbind(dataframe_F_F, to_add) 
   }     # .GlobalEnv$
 FleetList_simulation[[fs]]@catchAtAge.M.vector <<- dataframe_F_M
 FleetList_simulation[[fs]]@catchAtAge.F.vector <<- dataframe_F_F
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )

catchAtAge.vector.males <<- FleetList_simulation[[index_to_load]]@catchAtAge.M.vector 
catchAtAge.vector.females <<- FleetList_simulation[[index_to_load]]@catchAtAge.F.vector   
# loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])
reload_catchAtAgeF()
reload_catchAtAgeM()

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Catch at age loaded!        ")

} 
}
