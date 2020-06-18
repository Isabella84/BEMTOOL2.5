# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


      
  
  go_quest_refpoints <- T 
   
for (sp in 1:length(BMT_SPECIES) ) {
   if (go_quest_refpoints) {                     
      if ( !as.logical(mat_cfg_REF_points[sp, 2]) &  as.logical(mat_cfg_REF_points[sp, 3] )  ) {
tablesDIR <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[sp], sep="") 
suffix_outfiles <<- ""
prefix_outfiles <<- paste(casestudy_name, " - ", sep="")
REFERENCEPOINTS_table <<- paste(tablesDIR, "/", prefix_outfiles, "Reference points", suffix_outfiles," - ",BMT_SPECIES[sp], ".csv",sep="")
referencepoints_tbl <- try(read.csv(REFERENCEPOINTS_table, sep=";"), silent=TRUE)
      
    if (class(referencepoints_tbl) ==  "try-error") {
     go_quest_refpoints <- F
    showError(paste("ALADYM reference points for ",BMT_SPECIES[sp]," not found! Be sure the reference points calculation is checked before running ALADYM simulation.", sep=""))
      }  
    }
  }
} 

go_quest <- go_quest_refpoints   
