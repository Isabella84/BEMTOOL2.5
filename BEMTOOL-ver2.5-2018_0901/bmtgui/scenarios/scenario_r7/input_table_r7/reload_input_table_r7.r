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
#
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_input_table_r7<- function(w) {

  input_table_r7_list <<- list()
  input_table_r7Index <<- 0

    if (is.null( input_table_r7 )) { 
   input_table_r7_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=2))
   colnames(input_table_r7_matrix) <- c("fleet_segment", "% for the quota splitting")
     input_table_r7_matrix$fleet_segment <- BMT_FLEETSEGMENTS
 input_table_r7 <<- input_table_r7_matrix
 } else {
     input_table_r7_matrix <<- input_table_r7 
 }
 
   for (r in 1:nrow(input_table_r7_matrix)) { 
  input_table_r7_temp <- as.list(input_table_r7_matrix[r,]) 
  input_table_r7_list <<- c(input_table_r7_list, list(input_table_r7_temp)) 
  }

input_table_r7.model <<- gtkListStoreNew("gchararray",  "gdouble", "gboolean")  
  # add items 
  for (i in 1:length(input_table_r7_list)) {
    iter <- input_table_r7.model$append()$iter
    input_table_r7.model$set(iter,0, input_table_r7_list[[i]]$fleet_segment)
    for (e in 1:length(c("% for the quota splitting"))) {
         input_table_r7.model$set(iter, e, as.numeric(input_table_r7_list[[i]][e+1]))
    }
       input_table_r7.model$set(iter, 2,TRUE)
  } 

if (exists("input_table_r7.treeview")) { 
input_table_r7.treeview$destroy()
}
  
  input_table_r7.treeview <<- gtkTreeViewNewWithModel( input_table_r7.model)
 input_table_r7.treeview$setRulesHint(TRUE)
 input_table_r7.treeview$getSelection()$setMode("single")
input_table_r7.add_columns( input_table_r7.treeview)
input_table_r7.sw$add(input_table_r7.treeview)
    
}
