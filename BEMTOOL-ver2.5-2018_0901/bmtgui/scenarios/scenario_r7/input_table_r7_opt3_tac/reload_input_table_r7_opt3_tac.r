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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_input_table_r7_opt3_tac <- function(w) {

  input_table_r7_opt3_tac_list <<- list()
  input_table_r7_opt3_tacIndex <<- 0

    if (is.null( input_table_r7_opt3_tac )) { 
   input_table_r7_opt3_tac_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_SIMULATION), ncol=2))
   colnames(input_table_r7_opt3_tac_matrix) <- c("year", "tac")
     input_table_r7_opt3_tac_matrix$year <- BMT_YEARS_SIMULATION
 input_table_r7_opt3_tac <<- input_table_r7_opt3_tac_matrix
 } else {
     input_table_r7_opt3_tac_matrix <<- input_table_r7_opt3_tac 
 }
 
   for (r in 1:nrow(input_table_r7_opt3_tac_matrix)) { 
  input_table_r7_opt3_tac_temp <- as.list(input_table_r7_opt3_tac_matrix[r,]) 
  input_table_r7_opt3_tac_list <<- c(input_table_r7_opt3_tac_list, list(input_table_r7_opt3_tac_temp)) 
  }

input_table_r7_opt3_tac.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  # add items 
  for (i in 1:length(input_table_r7_opt3_tac_list)) {
    iter <- input_table_r7_opt3_tac.model$append()$iter
    input_table_r7_opt3_tac.model$set(iter,0, input_table_r7_opt3_tac_list[[i]]$year)
	    input_table_r7_opt3_tac.model$set(iter,1, input_table_r7_opt3_tac_list[[i]]$tac)
       input_table_r7_opt3_tac.model$set(iter, 2, TRUE)
  } 

if (exists("input_table_r7_opt3_tac.treeview")) { 
input_table_r7_opt3_tac.treeview$destroy()
}
  
  input_table_r7_opt3_tac.treeview <<- gtkTreeViewNewWithModel( input_table_r7_opt3_tac.model)
 input_table_r7_opt3_tac.treeview$setRulesHint(TRUE)
 input_table_r7_opt3_tac.treeview$getSelection()$setMode("single")
input_table_r7_opt3_tac.add_columns( input_table_r7_opt3_tac.treeview)
input_table_r7_opt3_tac.sw$add(input_table_r7_opt3_tac.treeview)
    
}
