# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveSelectivityLengthtoFile <- function(w) {
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
wnd <- showMessage("        Saving Selectivity by Length...        ")

 l_inf_F <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max)) 
  l_inf_M <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))   
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))
l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))

print(paste("Max length: males", l_inf_lens_M[length(l_inf_lens_M)], "females",  l_inf_lens_F[length(l_inf_lens_F)]) ) 

nrow_males_lenXsel <- length(years)*length(l_inf_lens_M )       #
nrow_females_lenXsel <- length(years)*length(l_inf_lens_F)       #

all_SelectivityLength <- data.frame(matrix(nrow=(nrow_males_lenXsel + nrow_females_lenXsel), ncol=(3+length(FLEETSEGMENTS_names))))
colnames(all_SelectivityLength) <- c("Length", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")  

    vect_year <- data.frame(matrix(nrow=0, ncol=1))
        vect_sex <- data.frame(matrix(nrow=0, ncol=1))
            vect_lengths <- data.frame(matrix(nrow=0, ncol=1))
         

             for (yy in 1:length(years) ) { 
vect_year <- rbind(vect_year, matrix(rep(years[yy], (length(l_inf_lens_M )+length(l_inf_lens_F ))), ncol=1) )
vect_sex <- rbind(vect_sex,   matrix(c(rep("M", length(l_inf_lens_M )) , rep("F", length(l_inf_lens_F ))), ncol=1) )
vect_lengths <- rbind(vect_lengths,  matrix(c(l_inf_lens_M , l_inf_lens_F), ncol=1) )
 }


 all_SelectivityLength$Length <- vect_lengths[,1] 
   all_SelectivityLength$Year <- vect_year[,1]   
    all_SelectivityLength$Sex <- vect_sex[,1]     

     for (yy in 1:length(years) ) { 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
fs_object <- FleetList_simulation[[fs]]        
dataframe_F_M <- fs_object@SelectivityLength.M.vector
dataframe_F_F <- fs_object@SelectivityLength.F.vector
                                                                                        
all_SelectivityLength[all_SelectivityLength$Year == years[yy] & all_SelectivityLength$Sex == "M", (2+fs) ] <- as.numeric(as.character(dataframe_F_M[, yy+1]))
all_SelectivityLength[all_SelectivityLength$Year == years[yy] & all_SelectivityLength$Sex == "F", (2+fs) ] <- as.numeric(as.character(dataframe_F_F[, yy+1]))
 }
}

write.table(all_SelectivityLength, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Selectivity by Length saved!        ")


} 
}