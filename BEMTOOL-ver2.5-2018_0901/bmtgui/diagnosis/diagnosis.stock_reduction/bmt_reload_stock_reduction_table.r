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
bmt_reload_stock_reduction_table<- function(w) {

  stock_reduction_list <<- list()
  stock_reductionIndex <<- 0
  
  if (is.null( bmt_stock_reduction)) { 
  reduction_matrix <- data.frame(matrix("", nrow=length(BMT_SPECIES), ncol=7))
   colnames(reduction_matrix) <- c("Stock",	"% needed reduction (last year)",	"Fcurrent",	"Reference point",	"SSBcurrent",	"SSBref",	"Comments")
    reduction_matrix$Stock <- BMT_SPECIES
  bmt_stock_reduction <<- reduction_matrix   
  } else {
    reduction_matrix <<- bmt_stock_reduction
  }

   for (r in 1:nrow(reduction_matrix)) { 
  reduction_temp <- as.list(reduction_matrix[r,]) 
  stock_reduction_list <<- c(stock_reduction_list, list(reduction_temp)) 
  }
  
  stock_reduction.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 5), "gchararray", "gboolean")  
 
  for (i in 1:length(stock_reduction_list)) {
    iter <- stock_reduction.model$append()$iter
    stock_reduction.model$set(iter,0, as.character(stock_reduction_list[[i]][1]))
     stock_reduction.model$set(iter,1, as.numeric(stock_reduction_list[[i]][2]))
          stock_reduction.model$set(iter,2, as.numeric(stock_reduction_list[[i]][3]))
               stock_reduction.model$set(iter,3, as.numeric(stock_reduction_list[[i]][4]))
                    stock_reduction.model$set(iter,4, as.numeric(stock_reduction_list[[i]][5]))
                         stock_reduction.model$set(iter,5, as.numeric(stock_reduction_list[[i]][6]))
                             stock_reduction.model$set(iter,6, as.character(stock_reduction_list[[i]][7]))
       stock_reduction.model$set(iter, 7,FALSE)
  } 
 
stock_reduction.treeview$destroy()
  
  stock_reduction.treeview <<- gtkTreeViewNewWithModel( stock_reduction.model)
 stock_reduction.treeview$setRulesHint(TRUE)
 stock_reduction.treeview$getSelection()$setMode("single")
stock_reduction.add_columns( stock_reduction.treeview)
stock_reduction.sw$add(stock_reduction.treeview)
    
}
