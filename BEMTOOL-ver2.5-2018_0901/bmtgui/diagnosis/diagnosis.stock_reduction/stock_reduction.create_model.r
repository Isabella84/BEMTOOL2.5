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
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
stock_reduction.create_model <- function() {
#print("Creating model...")   
  # create list store
  stock_reduction.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 5), "gchararray", "gboolean")  
  add.stock_reduction()
  # add items 
  for (i in 1:length(stock_reduction_list)) {
    iter <- stock_reduction.model$append()$iter
    stock_reduction.model$set(iter,0, as.character(stock_reduction_list[[i]][1]))
     stock_reduction.model$set(iter,1, as.numeric(stock_reduction_list[[i]][2]))
          stock_reduction.model$set(iter,2, as.numeric(stock_reduction_list[[i]][3]))
               stock_reduction.model$set(iter,3, as.numeric(stock_reduction_list[[i]][4]))
                    stock_reduction.model$set(iter,4, as.numeric(stock_reduction_list[[i]][5]))
                         stock_reduction.model$set(iter,5, as.numeric(stock_reduction_list[[i]][6]))
                             stock_reduction.model$set(iter,6, as.character(stock_reduction_list[[i]][7]))
       stock_reduction.model$set(iter, 7, FALSE)
  } 
}
