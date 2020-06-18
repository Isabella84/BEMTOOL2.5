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
reload_EMPTY_selectivity <- function(w) {


#selectivity_params <<- get_selectivity_param_name()
#    fleet.selectivity  <<- NULL
   selectivities <<- list()
selectivityIndex <<- 0
#add.selectivities()


 sel_matrix <- data.frame(matrix(-1, nrow=((length(years)*12) +1), ncol=8))
 
   colnames(sel_matrix) <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities <<- c(selectivities, list(sel_temp)) 
  }
  
  fleet.selectivity <<- sel_matrix


  selectivities.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  # add items 
 for (i in 1:length(selectivities)) {
    iter <-  selectivities.model$append()$iter
    selectivities.model$set(iter,0, selectivities[[i]]$year)
    selectivities.model$set(iter,1, selectivities[[i]]$month) 
        selectivities.model$set(iter,2, selectivities[[i]]$param1)  
            selectivities.model$set(iter,3, selectivities[[i]]$param2)  
                selectivities.model$set(iter,4, selectivities[[i]]$param3)  
                    selectivities.model$set(iter,5, selectivities[[i]]$param4)  
                        selectivities.model$set(iter,6, selectivities[[i]]$param5)  
                            selectivities.model$set(iter,7, selectivities[[i]]$sel_type)                             
     selectivities.model$set(iter,8,TRUE)
  } 

  selectivities.treeview$destroy()
 selectivities.treeview <<- gtkTreeViewNewWithModel( selectivities.model)
 selectivities.treeview$setRulesHint(TRUE)
 selectivities.treeview$getSelection()$setMode("single")
selectivities.add_columns( selectivities.treeview)
selectivity.sw$add(selectivities.treeview)
 
}



reload_selectivity <- function(w) {


#selectivity_params <<- get_selectivity_param_name()
#    fleet.selectivity  <<- NULL
   selectivities <<- list()
selectivityIndex <<- 0
#add.selectivities()


 sel_matrix <-   fleet.selectivity
 
   for (r in 1:nrow(sel_matrix)) { 
  sel_temp <- as.list(sel_matrix[r,]) 
  selectivities <<- c(selectivities, list(sel_temp)) 
  }

  selectivities.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  # add items 
 for (i in 1:length(selectivities)) {
    iter <-  selectivities.model$append()$iter
    selectivities.model$set(iter,0, selectivities[[i]]$year)
    selectivities.model$set(iter,1, selectivities[[i]]$month) 
        selectivities.model$set(iter,2, ifelse(!is.na(selectivities[[i]]$param1), selectivities[[i]]$param1, -1))  
            selectivities.model$set(iter,3, ifelse(!is.na(selectivities[[i]]$param2), selectivities[[i]]$param2, -1))  
                selectivities.model$set(iter,4, ifelse(!is.na(selectivities[[i]]$param3), selectivities[[i]]$param3, -1))  
                    selectivities.model$set(iter,5, ifelse(!is.na(selectivities[[i]]$param4), selectivities[[i]]$param4, -1))  
                        selectivities.model$set(iter,6, ifelse(!is.na(selectivities[[i]]$param5), selectivities[[i]]$param5, -1)) 
                            selectivities.model$set(iter,7, selectivities[[i]]$sel_type)                             
     selectivities.model$set(iter,8,TRUE)
  } 

  selectivities.treeview$destroy()
 selectivities.treeview <<- gtkTreeViewNewWithModel( selectivities.model)
 selectivities.treeview$setRulesHint(TRUE)
 selectivities.treeview$getSelection()$setMode("single")
selectivities.add_columns( selectivities.treeview)
selectivity.sw$add(selectivities.treeview)

 
}