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
reload_EMPTY_catchAtAgeF <- function(w) {
                           
catchAtAgeF_list <<- list()
catchAtAgeF_index <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

         FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1)))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_fem:(n_ages+first_age_fem-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  catchAtAgeF_list <<- c(catchAtAgeF_list, list(FF_temp)) 
  }
  
  catchAtAge.vector.females <<- FF_matrix
   
  catchAtAge_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(catchAtAgeF_list)) {
    iter <-  catchAtAge_F.model$append()$iter
     catchAtAge_F.model$set(iter,0, catchAtAgeF_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        catchAtAge_F.model$set(iter, e, as.double(catchAtAgeF_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     catchAtAge_F.model$set(iter,(n_ages+1),TRUE)
  } 

catchAtAge_F.treeview$destroy()
catchAtAge_F.treeview <<- gtkTreeViewNewWithModel( catchAtAge_F.model)
catchAtAge_F.treeview$setRulesHint(TRUE)
catchAtAge_F.treeview$getSelection()$setMode("single")
catchAtAge_F.add_columns( catchAtAge_F.treeview)
catchAtAge_F.sw$add(catchAtAge_F.treeview)

}




reload_catchAtAgeF <- function(w) {
                           
catchAtAgeF_list <<- list()
catchAtAgeF_index <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

         FF_matrix <-  catchAtAge.vector.females 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  catchAtAgeF_list <<- c(catchAtAgeF_list, list(FF_temp)) 
  }

  catchAtAge_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(catchAtAgeF_list)) {
    iter <-  catchAtAge_F.model$append()$iter
     catchAtAge_F.model$set(iter,0, catchAtAgeF_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        catchAtAge_F.model$set(iter, e, as.double(catchAtAgeF_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     catchAtAge_F.model$set(iter,(n_ages+1),TRUE)
  } 

catchAtAge_F.treeview$destroy()
catchAtAge_F.treeview <<- gtkTreeViewNewWithModel( catchAtAge_F.model)
catchAtAge_F.treeview$setRulesHint(TRUE)
catchAtAge_F.treeview$getSelection()$setMode("single")
catchAtAge_F.add_columns( catchAtAge_F.treeview)
catchAtAge_F.sw$add(catchAtAge_F.treeview)

}