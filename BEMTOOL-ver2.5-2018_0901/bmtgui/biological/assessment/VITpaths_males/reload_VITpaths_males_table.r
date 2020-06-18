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
# Function to reload the values for the selectivity according to the 
# selection of the selectivity function
# ------------------------------------------------------------------------------
#
reload_VITpaths_males_table <- function(w) {

    VITpaths_males_casestudy  <<- NULL
   VITpaths_males_list <<- list()
VITpaths_malesIndex <<- 0
add.VITpaths_males()
 VITpaths_males.model <<- gtkListStoreNew("gchararray", "gchararray", "gboolean")  
  
    if (!is.null(VITpaths_maless) ) {  
 for (i in 1:length(VITpaths_males_list)) {
    iter <-  VITpaths_males.model$append()$iter
    VITpaths_males.model$set(iter,0, as.character(VITpaths_males_list[[i]][[1]]))
   VITpaths_males.model$set(iter,1, as.character(VITpaths_males_list[[i]][[2]]))       
     VITpaths_males.model$set(iter,2,FALSE)
  } 
   }   

VITpaths_males.treeview$destroy()
# ------------------------------
# create tree view
VITpaths_males.treeview <<- gtkTreeViewNewWithModel(VITpaths_males.model)
VITpaths_males.treeview$setRulesHint(TRUE)
VITpaths_males.treeview$getSelection()$setMode("single")
VITpaths_males.add_columns(VITpaths_males.treeview)
VITpaths_males.sw$add(VITpaths_males.treeview) 
 
}
