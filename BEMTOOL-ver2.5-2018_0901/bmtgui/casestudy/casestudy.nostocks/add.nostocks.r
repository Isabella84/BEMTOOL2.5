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
add.nostocks <- function() {
#print("Adding elements to the list...")                               
  if (!is.null(nostockss) ) {
  if ( nrow(nostockss) != 0) {
  
  for (r in 1:nrow(nostockss)) {
  dis_temp <- as.list(nostockss[r,]) 

     heading <- c("Stock")
 
  names(dis_temp) <- heading
  nostocks_list <<- c(nostocks_list, list(dis_temp)) 
  }
  }
   }
# print("DISCARD (simulation) list successfully updated!", quote=F)
    #print(selectivities[1])
}