# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
reload_EMPTY_selectivity_fore <- function(w) {
#selectivity_params <<- get_selectivity_param_name()
#    fleet.selectivity_fore  <<- NULL

   selectivities_fore <<- list()
selectivity_foreIndex <<- 0

sel_matrix <- data.frame(matrix(-1, nrow=(length(years_forecast)*12), ncol=8))
 
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
   sel_matrix$sel_type <- 1 
   
   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities_fore <<- c(selectivities_fore, list(sel_temp)) 
  }
  
  fleet.selectivity_fore <<- sel_matrix


  selectivities_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  # add items 
 for (i in 1:length(selectivities_fore)) {
    iter <-  selectivities_fore.model$append()$iter
    selectivities_fore.model$set(iter,0, selectivities_fore[[i]]$year)
    selectivities_fore.model$set(iter,1, selectivities_fore[[i]]$month) 
        selectivities_fore.model$set(iter,2, ifelse(!is.na(selectivities_fore[[i]]$param1), selectivities_fore[[i]]$param1, -1))  
            selectivities_fore.model$set(iter,3, ifelse(!is.na(selectivities_fore[[i]]$param2), selectivities_fore[[i]]$param2, -1))  
                selectivities_fore.model$set(iter,4, ifelse(!is.na(selectivities_fore[[i]]$param3), selectivities_fore[[i]]$param3, -1))  
                    selectivities_fore.model$set(iter,5, ifelse(!is.na(selectivities_fore[[i]]$param4), selectivities_fore[[i]]$param4, -1))  
                        selectivities_fore.model$set(iter,6, ifelse(!is.na(selectivities_fore[[i]]$param5), selectivities_fore[[i]]$param5, -1)) 
                            selectivities_fore.model$set(iter,7, selectivities_fore[[i]]$sel_type)                             
     selectivities_fore.model$set(iter,8,TRUE)
  } 

  selectivities_fore.treeview$destroy()
 selectivities_fore.treeview <<- gtkTreeViewNewWithModel( selectivities_fore.model)
 selectivities_fore.treeview$setRulesHint(TRUE)
 selectivities_fore.treeview$getSelection()$setMode("single")
selectivities_fore.add_columns( selectivities_fore.treeview)
selectivities_fore.sw$add(selectivities_fore.treeview)
    
}






reload_selectivity_fore <- function(w) {
#selectivity_params <<- get_selectivity_param_name()
#    fleet.selectivity_fore  <<- NULL

   selectivities_fore <<- list()
selectivity_foreIndex <<- 0

sel_matrix <- fleet.selectivity_fore

   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities_fore <<- c(selectivities_fore, list(sel_temp)) 
  }

  selectivities_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  # add items 
 for (i in 1:length(selectivities_fore)) {
    iter <-  selectivities_fore.model$append()$iter
    selectivities_fore.model$set(iter,0, selectivities_fore[[i]]$year)
    selectivities_fore.model$set(iter,1, selectivities_fore[[i]]$month) 
        selectivities_fore.model$set(iter,2, ifelse(!is.na(selectivities_fore[[i]]$param1), selectivities_fore[[i]]$param1, -1))  
            selectivities_fore.model$set(iter,3, ifelse(!is.na(selectivities_fore[[i]]$param2), selectivities_fore[[i]]$param2, -1))  
                selectivities_fore.model$set(iter,4, ifelse(!is.na(selectivities_fore[[i]]$param3), selectivities_fore[[i]]$param3, -1))  
                    selectivities_fore.model$set(iter,5, ifelse(!is.na(selectivities_fore[[i]]$param4), selectivities_fore[[i]]$param4, -1))  
                        selectivities_fore.model$set(iter,6, ifelse(!is.na(selectivities_fore[[i]]$param5), selectivities_fore[[i]]$param5, -1))  
                            selectivities_fore.model$set(iter,7, selectivities_fore[[i]]$sel_type)                             
     selectivities_fore.model$set(iter,8,TRUE)
  } 

  selectivities_fore.treeview$destroy()
 selectivities_fore.treeview <<- gtkTreeViewNewWithModel( selectivities_fore.model)
 selectivities_fore.treeview$setRulesHint(TRUE)
 selectivities_fore.treeview$getSelection()$setMode("single")
selectivities_fore.add_columns( selectivities_fore.treeview)
selectivities_fore.sw$add(selectivities_fore.treeview)
    
}