# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




change_fishingM_M<- function(w) {

 n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
 # print(paste("changed life span for males!", n_ages_M) )
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration

n_ages_male <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))
first_age_male <- 0

    n_ages_male <- n_ages_male - trunc(Tr/12)
    first_age_male <- trunc(Tr/12)

FishingMvector_M_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_male+1) ))
colnames(FishingMvector_M_table) <-  c("year",paste("age", c(first_age_male:(n_ages_male+first_age_male-1)), sep=""))
FishingMvector_M_table$year <- years

if (new_aldSimulation@Ftype == "F") {
if (length(FishingMvector_M) != 0) {
names_ages <-  colnames(FishingMvector_M_table[ which(colnames(FishingMvector_M_table) %in% names(FishingMvector_M[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]
for (i in 1:length(FishingMvector_M)) {
 for (m in names_ages) {
FishingMvector_M_table[as.character(FishingMvector_M_table$year) == as.character(FishingMvector_M[[i]]$year),colnames(FishingMvector_M_table) == m] <-  FishingMvector_M[[i]][m] 
 }
}
}
mortality.Fvector.males <<- FishingMvector_M_table
 
} else {
mortality.Fvector.males <<- NULL

}
 
 ages_for_FMales <-  colnames(mortality_dataframe_M_M)

 
mortality_dataframe_M_M <- get_table("FISHING_MORTALITY_M")

mortality.Fvector.males <<- mortality_dataframe_M_M 

ages_for_FMales <-  colnames(mortality_dataframe_M_M)

  if (length(FleetList_simulation) != 0 ) {

for (fl in 1:length(FLEETSEGMENTS_names)) {
 mortality_dataframe_M_M <-  FleetList_simulation[[fl]]@fishingmortality.F.vector



mortality_dataframe_M_M <- mortality_dataframe_M_M[,c(1,which(colnames(mortality_dataframe_M_M) %in% ages_for_FMales))]


FleetList_simulation[[fl]]@fishingmortality.F.vector <<- mortality_dataframe_M_M 
}
 }

#mortality.FishingMvector.males <<- mortality.FishingMvector.males[,c(1:n_ages_M)]

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#------------------------------------------ load the file

reload_fishingmortalityM()

}