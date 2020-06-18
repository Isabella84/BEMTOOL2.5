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
reload_input_table_r7_opt3_indices <- function(w) {

  input_table_r7_opt3_indices_list <<- list()
  input_table_r7_opt3_indicesIndex <<- 0

    if (is.null( input_table_r7_opt3_indices )) { 
   input_table_r7_opt3_indices_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_SIMULATION), ncol=2))
   colnames(input_table_r7_opt3_indices_matrix) <- c("year", "SSB index")
     input_table_r7_opt3_indices_matrix$year <- BMT_YEARS_SIMULATION
 input_table_r7_opt3_indices <<- input_table_r7_opt3_indices_matrix
 } else {
     input_table_r7_opt3_indices_matrix <<- input_table_r7_opt3_indices 
 }
 
   for (r in 1:nrow(input_table_r7_opt3_indices_matrix)) { 
  input_table_r7_opt3_indices_temp <- as.list(input_table_r7_opt3_indices_matrix[r,]) 
  input_table_r7_opt3_indices_list <<- c(input_table_r7_opt3_indices_list, list(input_table_r7_opt3_indices_temp)) 
  }

input_table_r7_opt3_indices.model <<- gtkListStoreNew("gchararray",  "gdouble", "gboolean")  
  # add items 
  for (i in 1:length(input_table_r7_opt3_indices_list)) {
    iter <- input_table_r7_opt3_indices.model$append()$iter
    input_table_r7_opt3_indices.model$set(iter,0, input_table_r7_opt3_indices_list[[i]]$year)
         input_table_r7_opt3_indices.model$set(iter, 1, as.numeric(input_table_r7_opt3_indices_list[[i]][2]))
       input_table_r7_opt3_indices.model$set(iter, 2,TRUE)
  } 

if (exists("input_table_r7_opt3_indices.treeview")) { 
input_table_r7_opt3_indices.treeview$destroy()
}
  
  input_table_r7_opt3_indices.treeview <<- gtkTreeViewNewWithModel( input_table_r7_opt3_indices.model)
 input_table_r7_opt3_indices.treeview$setRulesHint(TRUE)
 input_table_r7_opt3_indices.treeview$getSelection()$setMode("single")
input_table_r7_opt3_indices.add_columns( input_table_r7_opt3_indices.treeview)
input_table_r7_opt3_indices.sw$add(input_table_r7_opt3_indices.treeview)
    
}
