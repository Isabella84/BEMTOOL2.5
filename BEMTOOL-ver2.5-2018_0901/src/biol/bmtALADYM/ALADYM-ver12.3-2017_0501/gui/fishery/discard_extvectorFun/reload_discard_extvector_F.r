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
reload_EMPTY_discard_extvector_F <- function(w) {
                           
discards_extvector_F_list <<- list()
discards_extvector_FIndex <<- 0

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
  discards_extvector_F_list <<- c(discards_extvector_F_list, list(FF_temp)) 
  }
  fleet.discard_extvector_F  <<- FF_matrix
   
  discards_extvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
 
  for (i in 1:length(discards_extvector_F_list)) {
    iter <- discards_extvector_F.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    discards_extvector_F.model$set(iter,0, discards_extvector_F_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         discards_extvector_F.model$set(iter, e, as.numeric(discards_extvector_F_list[[i]][e+1]))
    }
       discards_extvector_F.model$set(iter, (n_ages+1),TRUE)
  } 

discards_extvector_F.treeview$destroy()
discards_extvector_F.treeview <<- gtkTreeViewNewWithModel(discards_extvector_F.model)
discards_extvector_F.treeview$setRulesHint(TRUE)
discards_extvector_F.treeview$getSelection()$setMode("single")
discards_extvector_F.add_columns(discards_extvector_F.treeview)
discards_extvector_F.sw$add(discards_extvector_F.treeview)  
  
}


reload_discard_extvector_F <- function(w) {
                           
discards_extvector_F_list <<- list()
discards_extvector_FIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

       FF_matrix <-   fleet.discard_extvector_F
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  discards_extvector_F_list <<- c(discards_extvector_F_list, list(FF_temp)) 
  }
   
  discards_extvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
 
  for (i in 1:length(discards_extvector_F_list)) {
    iter <- discards_extvector_F.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    discards_extvector_F.model$set(iter,0, discards_extvector_F_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         discards_extvector_F.model$set(iter, e, as.numeric(discards_extvector_F_list[[i]][e+1]))
    }
       discards_extvector_F.model$set(iter, (n_ages+1),TRUE)
  } 

discards_extvector_F.treeview$destroy()
discards_extvector_F.treeview <<- gtkTreeViewNewWithModel(discards_extvector_F.model)
discards_extvector_F.treeview$setRulesHint(TRUE)
discards_extvector_F.treeview$getSelection()$setMode("single")
discards_extvector_F.add_columns(discards_extvector_F.treeview)
discards_extvector_F.sw$add(discards_extvector_F.treeview)  
  
}