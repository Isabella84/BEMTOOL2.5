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
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
input_table_r7_opt3_tac.create_model <- function() {
#print("Creating model...")   
  # create list store
  input_table_r7_opt3_tac.model <<- gtkListStoreNew("gchararray","gdouble", "gboolean")  
  add.input_table_r7_opt3_tac()
  # add items 
  for (i in 1:length(input_table_r7_opt3_tac_list)) {
    iter <- input_table_r7_opt3_tac.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    input_table_r7_opt3_tac.model$set(iter,0, input_table_r7_opt3_tac_list[[i]]$year)
        input_table_r7_opt3_tac.model$set(iter,0, input_table_r7_opt3_tac_list[[i]]$tac)
       input_table_r7_opt3_tac.model$set(iter, 2,TRUE)
  } 
  #print("input_table_r7_opt3_tac Model successfully created!")  
}
