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
reload_input_table_r5 <- function(w) {

  input_table_r5_list <<- list()
  input_table_r5Index <<- 0

    if (is.null( input_table_r5 )) { 
   input_table_r5_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=4))
   colnames(input_table_r5_matrix) <- c("fleet_segment", "Reduction scenario", "% vessel red.", "% days reduction")
     input_table_r5_matrix$fleet_segment <- BMT_FLEETSEGMENTS
	 	 input_table_r5_matrix[,2] <- 1
 input_table_r5 <<- input_table_r5_matrix
 } else {
     input_table_r5_matrix <<- input_table_r5 
 }
 
   for (r in 1:nrow(input_table_r5_matrix)) { 
  input_table_r5_temp <- as.list(input_table_r5_matrix[r,]) 
  input_table_r5_list <<- c(input_table_r5_list, list(input_table_r5_temp)) 
  }

input_table_r5.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 3), "gboolean")  
  # add items 
  for (i in 1:length(input_table_r5_list)) {
    iter <- input_table_r5.model$append()$iter
    input_table_r5.model$set(iter,0, input_table_r5_list[[i]]$fleet_segment)
    for (e in 1:length(c("Reduction scenario", "% vessel red.", "% days reduction"))) {
         input_table_r5.model$set(iter, e, as.numeric(as.character(input_table_r5_list[[i]][e+1])))
    }
       input_table_r5.model$set(iter, 4,TRUE)
  } 

if (exists("input_table_r5.treeview")) { 
input_table_r5.treeview$destroy()
}
  
  input_table_r5.treeview <<- gtkTreeViewNewWithModel( input_table_r5.model)
 input_table_r5.treeview$setRulesHint(TRUE)
 input_table_r5.treeview$getSelection()$setMode("single")
input_table_r5.add_columns( input_table_r5.treeview)
input_table_r5.sw$add(input_table_r5.treeview)
    
}
