# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


save_landing_file<-function(w) {

dialog <- gtkFileChooserDialog("Enter a name for the Landing .csv file", BMTmain_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
save_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {

     LANDING_final <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     LANDING_final[,1] <- c("Units: kg", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep=""))     
     
      index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_eco_landing_species)
index_to_update <- which(BMT_SPECIES == selected) 

      
         for (fl in 1:length(BMT_FLEETSEGMENTS)) {
      LANDING_matrix   <-    LANDING_list_all[[fl]][[index_to_update]]
     
           LANDING_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         LANDING_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         LANDING_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
         
        mat <- as.data.frame(t(LANDING_matrix ) , stringsAsFactors =F) 
        LANDING_final[5:16,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:13,]
        
      }
                     
  write.table(LANDING_final, file=save_path, sep=";", row.names=F, col.names=F)

    temp_landcfg <-  as.data.frame(mat_cfg_LandingData , stringsAsFactors =F)
    
   levels(temp_landcfg[,2]) <- factor(c(levels(temp_landcfg[,2]), save_path))     
 temp_landcfg[2,2] <- save_path
   
    mat_cfg_LandingData <<- temp_landcfg

    wnd <- showMessageOK(paste("        ", selected, "landing saved and all fleet segment landings exported!        "))


}

}