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
reload_input_table_r6 <- function(w) {

  input_table_r6_list <<- list()
  input_table_r6Index <<- 0

    if (is.null( input_table_r6 )) { 
   input_table_r6_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=4))
   colnames(input_table_r6_matrix) <- c("fleet_segment", "to be reduced", "% vessel red.", "% days reduction")
   input_table_r6_matrix[,2] <- "N"
     input_table_r6_matrix$fleet_segment <- BMT_FLEETSEGMENTS
 input_table_r6 <<- input_table_r6_matrix
 } else {
     input_table_r6_matrix <<- input_table_r6 
 }
 
   for (r in 1:nrow(input_table_r6_matrix)) { 
  input_table_r6_temp <- as.list(input_table_r6_matrix[r,]) 
  input_table_r6_list <<- c(input_table_r6_list, list(input_table_r6_temp)) 
  }

input_table_r6.model <<- gtkListStoreNew("gchararray",  "gchararray", rep("gdouble", 2), "gboolean")  
  # add items 
  for (i in 1:length(input_table_r6_list)) {
    iter <- input_table_r6.model$append()$iter
    input_table_r6.model$set(iter,0, input_table_r6_list[[i]]$fleet_segment)
    for (e in 1:length(c("to be reduced", "% vessel red.", "% days reduction"))) {
      if (e==1) {
         input_table_r6.model$set(iter, e, input_table_r6_list[[i]][e+1])
      } else {
         input_table_r6.model$set(iter, e, as.numeric(input_table_r6_list[[i]][e+1]))
         }
    }
       input_table_r6.model$set(iter, 4,TRUE)
  } 

if (exists("input_table_r6.treeview")) { 
input_table_r6.treeview$destroy()
}
  
  input_table_r6.treeview <<- gtkTreeViewNewWithModel( input_table_r6.model)
 input_table_r6.treeview$setRulesHint(TRUE)
 input_table_r6.treeview$getSelection()$setMode("single")
input_table_r6.add_columns( input_table_r6.treeview)
input_table_r6.sw$add(input_table_r6.treeview)
    
}
