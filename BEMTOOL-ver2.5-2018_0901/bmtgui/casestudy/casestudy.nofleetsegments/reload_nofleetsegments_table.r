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
# Function to reload the values for the selectivity according to the 
# selection of the selectivity function
# ------------------------------------------------------------------------------
#
reload_nofleetsegments_table <- function(w) {

for (flfl in 1:length(BMT_FLEETSEGMENTS) ) {
      to_add <- data.frame( BMT_FLEETSEGMENTS[flfl])
     colnames(to_add)  <-  "Fleet segment"
    nofleetsegments <<- rbind(nofleetsegments, to_add)
#    print(nofleetsegments)
}

    nofleetsegments_casestudy  <<- NULL
   nofleetsegments_list <<- list()
nofleetsegmentsIndex <<- 0
add.nofleetsegments()
 nofleetsegments.model <<- gtkListStoreNew("gchararray", "gboolean")  
  
 for (i in 1:length(nofleetsegments_list)) {
    iter <-  nofleetsegments.model$append()$iter
   nofleetsegments.model$set(iter,0, nofleetsegments_list[[i]])
   # discards.model$set(iter, 1, discards_list[[i]]$month)           
       #  nofleetsegments.model$set(iter, 2, nofleetsegments_list[[i]][3])
     nofleetsegments.model$set(iter,1,TRUE)
  } 

nofleetsegments.treeview$destroy()
nofleetsegments.treeview <<- gtkTreeViewNewWithModel(nofleetsegments.model)
nofleetsegments.treeview$setRulesHint(TRUE)
nofleetsegments.treeview$getSelection()$setMode("single")
nofleetsegments.add_columns(nofleetsegments.treeview)
nofleetsegments.sw$add(nofleetsegments.treeview)   
 
}
