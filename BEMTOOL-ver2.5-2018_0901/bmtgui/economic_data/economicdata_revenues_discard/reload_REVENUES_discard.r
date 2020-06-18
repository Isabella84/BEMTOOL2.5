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
bmt_reload_REVENUES_discard_table<- function(w) {

  bmt_REVENUES_discard_list <<- list()
  bmt_REVENUES_discardIndex <<- 0

  if (is.null( bmt_economic.REVENUES_discard )) { 
  REVENUES_disc_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(BMT_SPECIES)+1)))
   colnames(REVENUES_disc_matrix) <- c("year",paste("casestudy.revenues.discard.S", 1:length(BMT_SPECIES), sep=""))
     REVENUES_disc_matrix$year <- years   
  bmt_economic.REVENUES_discard <<- REVENUES_disc_matrix   
  } else {
    REVENUES_disc_matrix <<- bmt_economic.REVENUES_discard 
  }

   for (r in 1:nrow(REVENUES_disc_matrix)) { 
  REVENUES_disc_temp <- as.list(REVENUES_disc_matrix[r,]) 
  bmt_REVENUES_discard_list <<- c(bmt_REVENUES_discard_list, list(REVENUES_disc_temp)) 
  }
  
bmt_REVENUES_discard.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_SPECIES)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_REVENUES_discard_list)) {
    iter <- bmt_REVENUES_discard.model$append()$iter
    bmt_REVENUES_discard.model$set(iter,0, bmt_REVENUES_discard_list[[i]]$year)
    for (e in 1:length(BMT_SPECIES)) {
         bmt_REVENUES_discard.model$set(iter, e, as.numeric(bmt_REVENUES_discard_list[[i]][e+1]))
    }
       bmt_REVENUES_discard.model$set(iter, (length(BMT_SPECIES)+1),TRUE)
  } 

if (exists("bmt_REVENUES_discard.treeview")) {  
bmt_REVENUES_discard.treeview$destroy()
}
  
  bmt_REVENUES_discard.treeview <<- gtkTreeViewNewWithModel( bmt_REVENUES_discard.model)
 bmt_REVENUES_discard.treeview$setRulesHint(TRUE)
 bmt_REVENUES_discard.treeview$getSelection()$setMode("single")
bmt_REVENUES_discard.add_columns( bmt_REVENUES_discard.treeview)
bmt_REVENUES_discard.sw$add(bmt_REVENUES_discard.treeview)
    
}
