# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
FishingMvector_F_fore.cell_edited <- function(cell, path.string, new.text, data) {
 if (is.na(as.double(new.text) )) {
      showError("Value for fishing mortality must be a number!")
 } else { 
  #checkPtrType(data, "GtkListStore")
  FishingMvector_F_fore.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("Fishing mortality (FEMALES) Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("Fishing mortality (FEMALES) Edited column:", column))
  iter <- FishingMvector_F_fore.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  	#print(Zvector_M[[i]])
  	FishingMvector_F_fore[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(Zvector_M[[i]][column+1])
  	FishingMvector_F_fore.model$set(iter, column, FishingMvector_F_fore[[i]][column+1])
  
  # set the new values # !!!!!!

n_ages_mal <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  

n_ages_mal <- n_ages_mal - trunc(Tr/12)
first_age_mal <- trunc(Tr/12)

#print(paste("years for fishing mortality table (MALES):", n_ages_mal))
FishingMvector_M_table <- data.frame(matrix(nrow=length(years.forecast), ncol=(n_ages_mal+1) ))
colnames(FishingMvector_M_table) <- c("year",paste("age", c(first_age_mal:(n_ages_mal+first_age_mal-1)), sep=""))
FishingMvector_M_table$year <- years.forecast
 
if (length(FishingMvector_M_fore) != 0) {
names_ages <-  colnames(FishingMvector_M_table[ which(colnames(FishingMvector_M_table) %in% names(FishingMvector_M_fore[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]
for (i in 1:length(FishingMvector_M_fore)) {
 for (m in names_ages) {
FishingMvector_M_table[as.character(FishingMvector_M_table$year) == as.character(FishingMvector_M_fore[[i]]$year),colnames(FishingMvector_M_table) == m] <-  FishingMvector_M_fore[[i]][m] 
 }
}
}
	
FleetList_forecast[[gtkComboBoxGetActive(combo_fleetsegments)+1]]@fishingmortality.F.vector <<- FishingMvector_M_table
  }
}
