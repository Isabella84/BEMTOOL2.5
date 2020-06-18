# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

#runBEMTOOL  <- function()  {
# rm(list=ls(all=TRUE))
BMT_STATE <<- "START"
IN_BEMTOOL <<- TRUE

ALADYM_GUI_fleets_fore <<- vector(mode="list", length=length(BMT_SPECIES))
ALADYM_GUI_populations <<- vector(mode="list", length=length(BMT_SPECIES))
ALADYM_GUI_simulations <<- vector(mode="list", length=length(BMT_SPECIES))
ALADYM_GUI_fleets <<- vector(mode="list", length=length(BMT_SPECIES))

source(paste(getwd(), "/src/runBEMTOOLdiagnosis.r", sep="")) 

# runBEMTOOLdiagnosis()
#}
















if (FALSE) {
 for (spe in 1:length(BMT_SPECIES)) {
     dput(Populations[[spe]], file=paste(getwd(), "/logs/Populations_",BMT_SPECIES[spe],".csv", sep=""))
 }

 for (spe in 1:length(BMT_SPECIES)) {
  for (ye in 1:length(years)) {
 #   for (flee in 1:length(Interactionsyear[[ye]][[spe]]@interactions)) {      # [[flee]]                 "_", Interactionsyear[[ye]][[spe]]@interactions[[flee]]$fleetsegment,
         dput(Interactionsyear[[ye]][[spe]]@interactions, file=paste(getwd(), "/logs/interactions_",BMT_SPECIES[spe],"_", years[ye],".csv", sep="")) 
 # }
  }
  }

   for (ye in 1:length(years)) {
   for (flee in 1:length(BMT_FLEETSEGMENTS)) {      # [[flee]]                 "_", Interactionsyear[[ye]][[spe]]@interactions[[flee]]$fleetsegment,
         dput(Fleetyear[[ye]]@fleetsegments[[flee]], file=paste(getwd(), "/logs/", years[ye],"_",BMT_FLEETSEGMENTS[flee] ,".csv", sep="")) 
   }
  }
  
}

                                                  