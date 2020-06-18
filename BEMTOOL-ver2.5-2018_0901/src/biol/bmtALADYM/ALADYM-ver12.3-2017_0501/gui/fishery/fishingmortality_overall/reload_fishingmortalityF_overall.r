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
reload_EMPTY_fishingmortalityF_overall <- function(w) {
                           
FishingMvector_F_overall_list <<- list()
FishingMvector_F_overallIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

 n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

         FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=n_ages+1))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_fem:(n_ages+first_age_fem-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  FishingMvector_F_overall_list <<- c(FishingMvector_F_overall_list, list(FF_temp)) 
  }
  
FF_overall_matrix <<- FF_matrix
   
  FishingMvector_F_overall.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(FishingMvector_F_overall_list)) {
    iter <-  FishingMvector_F_overall.model$append()$iter
     FishingMvector_F_overall.model$set(iter,0, FishingMvector_F_overall_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_F_overall.model$set(iter, e, as.double(FishingMvector_F_overall_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_F_overall.model$set(iter,(n_ages+1),TRUE)
  } 

FishingMvector_F_overall.treeview$destroy()
FishingMvector_F_overall.treeview <<- gtkTreeViewNewWithModel( FishingMvector_F_overall.model)
FishingMvector_F_overall.treeview$setRulesHint(TRUE)
FishingMvector_F_overall.treeview$getSelection()$setMode("single")
FishingMvector_F_overall.add_columns( FishingMvector_F_overall.treeview)
FishingMvector_F_overall.sw$add(FishingMvector_F_overall.treeview)

}




reload_fishingmortalityF_overall <- function(w) {
                           
FishingMvector_F_overall_list <<- list()
FishingMvector_F_overallIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

         FF_matrix <-  FF_overall_matrix 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  FishingMvector_F_overall_list <<- c(FishingMvector_F_overall_list, list(FF_temp)) 
  }

  FishingMvector_F_overall.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(FishingMvector_F_overall_list)) {
    iter <-  FishingMvector_F_overall.model$append()$iter
     FishingMvector_F_overall.model$set(iter,0, FishingMvector_F_overall_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_F_overall.model$set(iter, e, as.double(FishingMvector_F_overall_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_F_overall.model$set(iter,(n_ages+1),TRUE)
  } 

FishingMvector_F_overall.treeview$destroy()
FishingMvector_F_overall.treeview <<- gtkTreeViewNewWithModel( FishingMvector_F_overall.model)
FishingMvector_F_overall.treeview$setRulesHint(TRUE)
FishingMvector_F_overall.treeview$getSelection()$setMode("single")
FishingMvector_F_overall.add_columns( FishingMvector_F_overall.treeview)
FishingMvector_F_overall.sw$add(FishingMvector_F_overall.treeview)

}