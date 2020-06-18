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
reload_EMPTY_discard_extvector_M <- function(w) {
                           
discards_extvector_M_list <<- list()
discards_extvector_MIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

       FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1)))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  discards_extvector_M_list <<- c(discards_extvector_M_list, list(FF_temp)) 
  }
  fleet.discard_extvector_M  <<- FF_matrix
   
  discards_extvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
 
  for (i in 1:length(discards_extvector_M_list)) {
    iter <- discards_extvector_M.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    discards_extvector_M.model$set(iter,0, discards_extvector_M_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         discards_extvector_M.model$set(iter, e, as.numeric(discards_extvector_M_list[[i]][e+1]))
    }
       discards_extvector_M.model$set(iter, (n_ages+1),TRUE)
  } 

discards_extvector_M.treeview$destroy()
discards_extvector_M.treeview <<- gtkTreeViewNewWithModel(discards_extvector_M.model)
discards_extvector_M.treeview$setRulesHint(TRUE)
discards_extvector_M.treeview$getSelection()$setMode("single")
discards_extvector_M.add_columns(discards_extvector_M.treeview)
discards_extvector_M.sw$add(discards_extvector_M.treeview)  
  
}


reload_discard_extvector_M <- function(w) {
                           
discards_extvector_M_list <<- list()
discards_extvector_MIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

       FF_matrix <-   fleet.discard_extvector_M
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  discards_extvector_M_list <<- c(discards_extvector_M_list, list(FF_temp)) 
  }
   
  discards_extvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
 
  for (i in 1:length(discards_extvector_M_list)) {
    iter <- discards_extvector_M.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    discards_extvector_M.model$set(iter,0, discards_extvector_M_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         discards_extvector_M.model$set(iter, e, as.numeric(discards_extvector_M_list[[i]][e+1]))
    }
       discards_extvector_M.model$set(iter, (n_ages+1),TRUE)
  } 

discards_extvector_M.treeview$destroy()
discards_extvector_M.treeview <<- gtkTreeViewNewWithModel(discards_extvector_M.model)
discards_extvector_M.treeview$setRulesHint(TRUE)
discards_extvector_M.treeview$getSelection()$setMode("single")
discards_extvector_M.add_columns(discards_extvector_M.treeview)
discards_extvector_M.sw$add(discards_extvector_M.treeview)  
  
}