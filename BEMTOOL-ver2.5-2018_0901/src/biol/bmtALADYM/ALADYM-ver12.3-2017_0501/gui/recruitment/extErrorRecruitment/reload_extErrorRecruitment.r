# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
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
reload_EMPTY_extErrorRecruitment <- function(w) {
                           
extErrorRecruitment_list <<- list()
extErrorRecruitmentIndex <<- 0

       zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS, ncol=1))
     colnames(zero_matrix) <- "error"
     # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
 # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
  for (cl in 1:nrow(zero_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(zero_matrix[,cl]))) 
  names(sr_matrix) <- "error"
  extErrorRecruitment_list <<- c(extErrorRecruitment_list, sr_matrix) 
  } 
  CI_external_matrix <<- zero_matrix
   
  extErrorRecruitment.model <<- gtkListStoreNew(rep("gdouble",CI_NB_RUNS), "gboolean")  

  # add items 
  iter <- extErrorRecruitment.model$append()$iter
  for (i in c(1:nrow(zero_matrix))) {
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    extErrorRecruitment.model$set(iter,(i-1), as.double(extErrorRecruitment_list[[i]]))
    #print(i)
      }   
   extErrorRecruitment.model$set(iter, i,TRUE)

extErrorRecruitment.treeview$destroy()
extErrorRecruitment.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment.model)
extErrorRecruitment.treeview$setRulesHint(TRUE)
extErrorRecruitment.treeview$getSelection()$setMode("single")
extErrorRecruitment.add_columns(extErrorRecruitment.treeview)
extErrorRecruitment.sw$add(extErrorRecruitment.treeview)      
  
}


reload_extErrorRecruitment <- function(w) {
                           
extErrorRecruitment_list <<- list()
extErrorRecruitmentIndex <<- 0


       this_matrix <- CI_external_matrix 
     for (r in 1:nrow(this_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(this_matrix[r,]))) 
  names(sr_matrix) <- "error"
  extErrorRecruitment_list <<- c(extErrorRecruitment_list, sr_matrix) 
  } 
   
  extErrorRecruitment.model <<- gtkListStoreNew(rep("gdouble",CI_NB_RUNS), "gboolean")  

  # add items 
  iter <- extErrorRecruitment.model$append()$iter
  for (i in c(1:nrow(this_matrix))) {
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    extErrorRecruitment.model$set(iter,(i-1), as.double(extErrorRecruitment_list[[i]]))
    #print(i)
      }   
   extErrorRecruitment.model$set(iter, i,TRUE)

extErrorRecruitment.treeview$destroy()
extErrorRecruitment.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment.model)
extErrorRecruitment.treeview$setRulesHint(TRUE)
extErrorRecruitment.treeview$getSelection()$setMode("single")
extErrorRecruitment.add_columns(extErrorRecruitment.treeview)
extErrorRecruitment.sw$add(extErrorRecruitment.treeview)      
  
  
}