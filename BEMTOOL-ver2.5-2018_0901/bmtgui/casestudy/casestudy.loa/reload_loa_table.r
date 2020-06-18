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
reload_loa_table <- function(w) {

loa_file <<- ""
#gtkLabelSetText(lbl_Selfile,"C:\\")
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

#select_fs <- gtkComboBoxGetActiveText(combo_loas)
#select_index_fs <- which(BMT_loaS[associated_loa_indices] == select_fs )
#
#gtkComboBoxSetActiveText(combo_discard, FleetList_simulation[[select_index_fs]]@discard.calculation)

    loa_casestudy  <<- NULL
   loa_list <<- list()
loaIndex <<- 0
add.loa()
 loa.model <<- gtkListStoreNew("gchararray", "gboolean")  
  
 for (i in 1:length(loa_list)) {
    iter <-  loa.model$append()$iter
   loa.model$set(iter,0, loa_list[[i]])
   # discards.model$set(iter, 1, discards_list[[i]]$month)           
       #  loa.model$set(iter, 2, loa_list[[i]][3])
     loa.model$set(iter,1,TRUE)
  } 

 loa.treeview <<- gtkTreeViewNewWithModel( loa.model)
 loa.treeview$setRulesHint(TRUE)
 loa.treeview$getSelection()$setMode("single")
loa.add_columns( loa.treeview)
loa.sw$destroy()
loa.sw <<- gtkScrolledWindowNew(NULL, NULL)
loa.sw$setShadowType("etched-in")
loa.sw$setPolicy("automatic", "automatic")
loa.sw$SetUsize(100, dim_big_tables)  
loa.sw$add(loa.treeview)
vbox_casestudy$packStart(loa.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vbox_casestudy, loa.sw, 3)
 
}
