# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




change_fishingM_F<- function(w) {

 n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))  
 # print(paste("changed life span for females!", n_ages_F) )
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
mortality_dataframe_M_F <- get_table("FISHING_MORTALITY_F")

mortality.Fvector.females <<- mortality_dataframe_M_F 

ages_for_FFemales <-  colnames(mortality_dataframe_M_F)

  if (length(FleetList_simulation) != 0 ) {
for (fl in 1:length(FLEETSEGMENTS_names)) {
 mortality_dataframe_M_F <-  FleetList_simulation[[fl]]@fishingmortality.F.vector
 
   mortality_dataframe_M_F <- mortality_dataframe_M_F[,which(colnames(mortality_dataframe_M_F) %in% ages_for_FFemales)]

 
FleetList_simulation[[fl]]@fishingmortality.F.vector <<- mortality_dataframe_M_F 
}
 }
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#------------------------------------------ load the file
source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableF.r", sep=""))


}
