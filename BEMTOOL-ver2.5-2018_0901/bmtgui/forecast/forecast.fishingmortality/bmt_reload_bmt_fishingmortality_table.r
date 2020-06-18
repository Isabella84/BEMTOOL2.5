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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#


  bmt_fishingmortality_list <<- list()
  bmt_fishingmortalityIndex <<- 0

  if (is.null(bmt_fishingmortality)) { 
  reduction_matrix <- data.frame(matrix(NA, nrow=length(BMT_YEARS_SIMULATION), ncol=(length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])+1) ))
   colnames(reduction_matrix) <- c("Year", paste("% F to", names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ]))
    reduction_matrix$Year <- BMT_YEARS_SIMULATION
  bmt_fishingmortality <<- reduction_matrix   
 } else {
    reduction_matrix <- bmt_fishingmortality
  }

#  print("Reduction matrix")
#  print(reduction_matrix)

   for (r in 1:nrow(reduction_matrix)) { 
  reduction_temp <- as.list(reduction_matrix[r,]) 
  bmt_fishingmortality_list <<- c(bmt_fishingmortality_list, list(reduction_temp)) 
  }
  
  bmt_fishingmortality.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])), "gboolean")  
 
  for (i in 1:length(bmt_fishingmortality_list)) {
    iter <- bmt_fishingmortality.model$append()$iter
    bmt_fishingmortality.model$set(iter,0, as.character(bmt_fishingmortality_list[[i]][1]))
     for (nfle in 1:length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])) {
             bmt_fishingmortality.model$set(iter,nfle, as.numeric(bmt_fishingmortality_list[[i]][nfle+1]))
     }
       bmt_fishingmortality.model$set(iter, (length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])+1),FALSE)
  } 
  
      if (exists(" bmt_fishingmortality.treeview")) {
        bmt_fishingmortality.treeview$destroy()
}

  bmt_fishingmortality.treeview <<- gtkTreeViewNewWithModel( bmt_fishingmortality.model)
 bmt_fishingmortality.treeview$setRulesHint(TRUE)
 bmt_fishingmortality.treeview$getSelection()$setMode("single")
bmt_fishingmortality.add_columns( bmt_fishingmortality.treeview)
bmt_fishingmortality.sw$add(bmt_fishingmortality.treeview) 

