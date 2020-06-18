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
loadSelectivityLengthFromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_SelectivityLength <- data.frame(read.csv(dialog$getFilename(), sep=";", na.strings = ""), stringsAsFactors =F)  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading Selectivity by length...        ")

# loaded_SelectivityLength <- read.csv( "C:\\ALADYM-ver10.1.2-2015\\input examples\\toload Selectivity vector by length.csv", sep=";", na.strings = "")  

 l_inf_F <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max)) 
  l_inf_M <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))   
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))
l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))

for (fs in 1:length(FLEETSEGMENTS_names) ) {

    dataframe_sel_len_M <- data.frame(matrix(nrow=length(l_inf_lens_M) , ncol=(length(years)+1)))
 colnames(dataframe_sel_len_M) <-  c( "Length", years )
  dataframe_sel_len_F <- data.frame(matrix(nrow=length(l_inf_lens_F) , ncol=(length(years)+1)))
  colnames(dataframe_sel_len_F) <-  c( "Length", years )

  dataframe_sel_len_M$Length <- l_inf_lens_M
    dataframe_sel_len_F$Length <- l_inf_lens_F
  
  for (ye in 1:length(years)) {
       dataframe_sel_len_M[,ye+1 ] <-  loaded_SelectivityLength[loaded_SelectivityLength$Sex == "M" & loaded_SelectivityLength$Year == years[ye], (fs+2)] 
         dataframe_sel_len_F[,ye+1 ] <- loaded_SelectivityLength[loaded_SelectivityLength$Sex == "F" & loaded_SelectivityLength$Year == years[ye], (fs+2)]  
  }

   FleetList_simulation[[fs]]@SelectivityLength.M.vector   <<- dataframe_sel_len_M
   FleetList_simulation[[fs]]@SelectivityLength.F.vector   <<- dataframe_sel_len_F
   
}

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )  
loadFleetsegmentintoGUI(FleetList_simulation[[index_to_load]])

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Selectivity by Length loaded!        ")

} 
}
