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
reload_EMPTY_fishingmortalityM_overall <- function(w) {
                           
FishingMvector_M_overall_list <<- list()
FishingMvector_M_overallIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

   n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

         FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1) ))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  FishingMvector_M_overall_list <<- c(FishingMvector_M_overall_list, list(FF_temp)) 
  }
  
 FM_overall_matrix <<- FF_matrix
   
  FishingMvector_M_overall.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(FishingMvector_M_overall_list)) {
    iter <-  FishingMvector_M_overall.model$append()$iter
     FishingMvector_M_overall.model$set(iter,0, FishingMvector_M_overall_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_M_overall.model$set(iter, e, as.double(FishingMvector_M_overall_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_M_overall.model$set(iter,(n_ages+1),TRUE)
  } 

FishingMvector_M_overall.treeview$destroy()
FishingMvector_M_overall.treeview <<- gtkTreeViewNewWithModel( FishingMvector_M_overall.model)
FishingMvector_M_overall.treeview$setRulesHint(TRUE)
FishingMvector_M_overall.treeview$getSelection()$setMode("single")
FishingMvector_M_overall.add_columns( FishingMvector_M_overall.treeview)
FishingMvector_M_overall.sw$add(FishingMvector_M_overall.treeview)

}




reload_fishingmortalityM_overall <- function(w) {
                           
FishingMvector_M_overall_list <<- list()
FishingMvector_M_overallIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

   n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

         FF_matrix <-  FM_overall_matrix
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  FishingMvector_M_overall_list <<- c(FishingMvector_M_overall_list, list(FF_temp)) 
  }

  FishingMvector_M_overall.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  
   for (i in 1:length(FishingMvector_M_overall_list)) {
    iter <-  FishingMvector_M_overall.model$append()$iter
     FishingMvector_M_overall.model$set(iter,0, FishingMvector_M_overall_list[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_M_overall.model$set(iter, e, as.double(FishingMvector_M_overall_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_M_overall.model$set(iter,(n_ages+1),TRUE)
  } 

FishingMvector_M_overall.treeview$destroy()
FishingMvector_M_overall.treeview <<- gtkTreeViewNewWithModel( FishingMvector_M_overall.model)
FishingMvector_M_overall.treeview$setRulesHint(TRUE)
FishingMvector_M_overall.treeview$getSelection()$setMode("single")
FishingMvector_M_overall.add_columns( FishingMvector_M_overall.treeview)
FishingMvector_M_overall.sw$add(FishingMvector_M_overall.treeview)

}