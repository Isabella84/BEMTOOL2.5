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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_catchAtAgeM <- function(w) {
                           
catchAtAgeM_list <<- list()
catchAtAgeM_index <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

         FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1)))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  catchAtAgeM_list <<- c(catchAtAgeM_list, list(FF_temp)) 
  }
  
  catchAtAge.vector.males <<- FF_matrix
   
  catchAtAge_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(catchAtAgeM_list)) {
    iter <-  catchAtAge_M.model$append()$iter
     catchAtAge_M.model$set(iter,0, catchAtAgeM_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        catchAtAge_M.model$set(iter, e, as.double(catchAtAgeM_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     catchAtAge_M.model$set(iter,(n_ages+1),TRUE)
  } 

catchAtAge_M.treeview$destroy()
catchAtAge_M.treeview <<- gtkTreeViewNewWithModel( catchAtAge_M.model)
catchAtAge_M.treeview$setRulesHint(TRUE)
catchAtAge_M.treeview$getSelection()$setMode("single")
catchAtAge_M.add_columns( catchAtAge_M.treeview)
catchAtAge_M.sw$add(catchAtAge_M.treeview)

}




reload_catchAtAgeM <- function(w) {
                           
catchAtAgeM_list <<- list()
catchAtAgeM_index <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

         FF_matrix <-  catchAtAge.vector.males 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  catchAtAgeM_list <<- c(catchAtAgeM_list, list(FF_temp)) 
  }

  catchAtAge_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(catchAtAgeM_list)) {
    iter <-  catchAtAge_M.model$append()$iter
     catchAtAge_M.model$set(iter,0, catchAtAgeM_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        catchAtAge_M.model$set(iter, e, as.double(catchAtAgeM_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     catchAtAge_M.model$set(iter,(n_ages+1),TRUE)
  } 

catchAtAge_M.treeview$destroy()
catchAtAge_M.treeview <<- gtkTreeViewNewWithModel( catchAtAge_M.model)
catchAtAge_M.treeview$setRulesHint(TRUE)
catchAtAge_M.treeview$getSelection()$setMode("single")
catchAtAge_M.add_columns( catchAtAge_M.treeview)
catchAtAge_M.sw$add(catchAtAge_M.treeview)

}