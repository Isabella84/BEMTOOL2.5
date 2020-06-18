# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
add.discards <- function() {
#print("Adding elements to the list...")                               
  if (!is.null(fleet.discard) ) {
  if ( nrow(fleet.discard) != 0) {
  
  for (r in 1:nrow(fleet.discard)) {
  dis_temp <- as.list(fleet.discard[r,]) 

     heading <- c("year","month",  "L50%",  "L75%-L25%")
 
  names(dis_temp) <- heading
  discards_list <<- c(discards_list, list(dis_temp)) 
  }
  
  } else {
      #selectivity_params <<- get_selectivity_param_name()
   dis_matrix <- data.frame(matrix(0, nrow=((length(years)*12) +1), ncol=4))
   
   heading <- c("year","month",   "L50%",  "L75%-L25%")
   
 colnames(dis_matrix) <- heading
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   dis_matrix$year <- years_rep
   dis_matrix$month <- months_rep
   
   for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_list <<- c(discards_list, list(dis_temp)) 
  }
  
  }
   } else {
  dis_matrix <- data.frame(matrix(0, nrow=((length(years)*12) +1), ncol=4))
   
   heading <- c("year","month",   "L50%",  "L75%-L25%")
 
   colnames(dis_matrix) <- heading
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   dis_matrix$year <- years_rep
   dis_matrix$month <- months_rep
   
    for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_list <<- c(discards_list, list(dis_temp)) 
  }
 }

# print("DISCARD (simulation) list successfully updated!", quote=F)
    #print(selectivities[1])
}