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
#mortality_dataframe_M_F <- get_table("FISHING_MORTALITY_F")

n_ages_fem <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))
first_age_fem <- 0

    n_ages_fem <- n_ages_fem - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

FishingMvector_F_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_fem+1) ))
colnames(FishingMvector_F_table) <-  c("year",paste("age", c(first_age_fem:(n_ages_fem+first_age_fem-1)), sep=""))
FishingMvector_F_table$year <- years

if (new_aldSimulation@Ftype == "F") {

if (length(FishingMvector_F) != 0) {
 names_ages <-  colnames(FishingMvector_F_table[ which(colnames(FishingMvector_F_table) %in% names(FishingMvector_F[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]

for (i in 1:length(FishingMvector_F)) {
  for (m in names_ages) {
FishingMvector_F_table[as.character(FishingMvector_F_table$year) == as.character(FishingMvector_F[[i]]$year),colnames(FishingMvector_F_table) == m] <-  FishingMvector_F[[i]][m] 
  }
}
}

mortality.Fvector.females <<- FishingMvector_F_table

} else {
mortality.Fvector.females <<- NULL
}

ages_for_FFemales <-  colnames(mortality_dataframe_M_F)

  if (length(FleetList_simulation) != 0 ) {
for (fl in 1:length(FLEETSEGMENTS_names)) {
 mortality_dataframe_M_F <-  FleetList_simulation[[fl]]@fishingmortality.F.vector

mortality_dataframe_M_F <- mortality_dataframe_M_F[,c(1,which(colnames(mortality_dataframe_M_F) %in% ages_for_FFemales))]

FleetList_simulation[[fl]]@fishingmortality.F.vector <<- mortality_dataframe_M_F 
}
 }
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#------------------------------------------ load the file
reload_fishingmortalityF()


}
