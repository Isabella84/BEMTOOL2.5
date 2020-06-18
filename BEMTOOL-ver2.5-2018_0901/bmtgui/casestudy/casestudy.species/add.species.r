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
#
# ------------------------------------------------------------------------------
# add elements to the list of the selectivity values
# ------------------------------------------------------------------------------
#
add.species <- function() {
#print("Adding elements to the list...")                               
  if (!is.null(speciess) ) {
  if ( nrow(speciess) != 0) {
  
  for (r in 1:nrow(speciess)) {
  dis_temp <- as.list(speciess[r,]) 

     heading <- c("GSA")
 
  names(dis_temp) <- heading
  species_list <<- c(species_list, list(dis_temp)) 
  }
  }
   }
# print("DISCARD (simulation) list successfully updated!", quote=F)
    #print(selectivities[1])
}