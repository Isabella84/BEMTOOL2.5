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
#
# ------------------------------------------------------------------------------
# add elements to the list of the selectivity_fore values
# ------------------------------------------------------------------------------
#
add.selectivities_fore <- function() {
#print("Adding elements to the list...")                    &  length(selectivities_fore) != 0           
  if (!is.null(fleet.selectivity_fore)  ) {
  if ( nrow(fleet.selectivity_fore) != 0) {
  
  for (r in 1:nrow(fleet.selectivity_fore)) {
  sel_temp <- as.list(fleet.selectivity_fore[r,]) 

 #if (selectivity_params$n_par == 2) {
#   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
# } else if (selectivity_params$n_par == 3) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
# } else if (selectivity_params$n_par == 5) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
# } 
 
#  names(sel_temp) <- heading
  selectivities_fore <<- c(selectivities_fore, list(sel_temp)) 
  }
  
  } else {
#      selectivity_params <<- get_selectivity_param_name()
   sel_matrix <- data.frame(matrix(0, nrow=(length(years_forecast)*12), ncol=8))
   
#    if (selectivity_params$n_par == 2) {
#   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
# } else if (selectivity_params$n_par == 3) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
# } else if (selectivity_params$n_par == 5) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
# } 
 
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
      sel_matrix$sel_type <- 1 
      
   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities_fore <<- c(selectivities_fore, list(sel_temp)) 
  }
  
  }
   } else {
  #      selectivity_params <<- get_selectivity_param_name()
   sel_matrix <- data.frame(matrix(0, nrow=(length(years_forecast)*12), ncol=8))
   
#    if (selectivity_params$n_par == 2) {
#   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
# } else if (selectivity_params$n_par == 3) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
# } else if (selectivity_params$n_par == 5) {
#   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
# } 
 
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
      sel_matrix$sel_type <- 1 
      
   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities_fore <<- c(selectivities_fore, list(sel_temp)) 
  }
 }

# print("SELECTIVITY (simulation) list successfully updated!", quote=F)
    #print(selectivities_fore[1])
}
