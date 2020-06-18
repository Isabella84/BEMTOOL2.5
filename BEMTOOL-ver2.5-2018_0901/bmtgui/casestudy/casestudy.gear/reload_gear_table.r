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
reload_gear_table <- function(w) {

    gear_casestudy  <<- NULL
   gear_list <<- list()
gearIndex <<- 0
add.gear()
 gear.model <<- gtkListStoreNew("gchararray", "gboolean")  
  
 for (i in 1:length(gear_list)) {
    iter <-  gear.model$append()$iter
   gear.model$set(iter,0, gear_list[[i]])
   # discards.model$set(iter, 1, discards_list[[i]]$month)           
       #  gear.model$set(iter, 2, gear_list[[i]][3])
     gear.model$set(iter,1,TRUE)
  } 

 gear.treeview <<- gtkTreeViewNewWithModel( gear.model)
 gear.treeview$setRulesHint(TRUE)
 gear.treeview$getSelection()$setMode("single")
gear.add_columns( gear.treeview)
gear.sw$destroy()
gear.sw <<- gtkScrolledWindowNew(NULL, NULL)
gear.sw$setShadowType("etched-in")
gear.sw$setPolicy("automatic", "automatic")
gear.sw$SetUsize(100, dim_big_tables)  
gear.sw$add(gear.treeview)
vbox_casestudy$packStart(gear.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vbox_casestudy, gear.sw, 3)
 
}
