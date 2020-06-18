# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#
# ------------------------------------------------------------------------------
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
bmt_fishingmortality.create_model <- function() {

bmt_fishingmortality_list <<- list()
bmt_fishingmortalityIndex <<- 0

 index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_diagnosis_species_pressure)
  index_to_update <- which(BMT_SPECIES == selected) 
  
    
names_of_fleets <- mat_cfg_SF[index_to_update,2:ncol(mat_cfg_SF)]

#print("Creating model...")   
  # create list store
  bmt_fishingmortality.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])), "gboolean") 
  add.bmt_fishingmortality()
  # add items 
  if (!is.null(bmt_fishingmortality)) {
  for (i in 1:length(bmt_fishingmortality_list)) {
    iter <- bmt_fishingmortality.model$append()$iter
    bmt_fishingmortality.model$set(iter,0, as.character(bmt_fishingmortality_list[[i]][1]))
     for (nfle in 1:length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])) {
             bmt_fishingmortality.model$set(iter,nfle, as.numeric(bmt_fishingmortality_list[[i]][nfle+1]))
     }
       bmt_fishingmortality.model$set(iter, (length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])+1),FALSE)
  } 
  }

}
