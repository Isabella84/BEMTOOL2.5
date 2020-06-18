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
reload_EMPTY_extErrorRecruitment_fore <- function(w) {
                           
extErrorRecruitment_fore_list <<- list()
extErrorRecruitment_foreIndex <<- 0

       zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=(length(years_forecast)+1) ))
     colnames(zero_matrix) <- c("run_N", years_forecast) 
     # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
 # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
  for (cl in 1:nrow(zero_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(zero_matrix[cl,]))) 
  names(sr_matrix) <- c("run_N", years_forecast) 
  extErrorRecruitment_fore_list <<- c(extErrorRecruitment_fore_list, list(sr_matrix)) 
  } 
  CI_external_matrix_fore <<- zero_matrix
   
  extErrorRecruitment_fore.model <<- gtkListStoreNew(rep("gdouble",(length(years_forecast)+1)), "gboolean")  


     for (i in 1:length(extErrorRecruitment_fore_list)) {
    iter <-  extErrorRecruitment_fore.model$append()$iter
    extErrorRecruitment_fore.model$set(iter,0, extErrorRecruitment_fore_list[[i]][1])
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years_forecast)) {
        extErrorRecruitment_fore.model$set(iter, e, as.double(extErrorRecruitment_fore_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     extErrorRecruitment_fore.model$set(iter,(length(years_forecast)+1),TRUE)
  } 


extErrorRecruitment_fore.treeview$destroy()
extErrorRecruitment_fore.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment_fore.model)
extErrorRecruitment_fore.treeview$setRulesHint(TRUE)
extErrorRecruitment_fore.treeview$getSelection()$setMode("single")
extErrorRecruitment_fore.add_columns(extErrorRecruitment_fore.treeview)
extErrorRecruitment_fore.sw$add(extErrorRecruitment_fore.treeview)      
  
}


reload_extErrorRecruitment_fore <- function(w) {
                           
extErrorRecruitment_fore_list <<- list()
extErrorRecruitment_foreIndex <<- 0


       this_matrix <- CI_external_matrix_fore 
     for (r in 1:nrow(this_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(this_matrix[r,]))) 
  names(sr_matrix) <- c("run_N", years_forecast) 
  extErrorRecruitment_fore_list <<- c(extErrorRecruitment_fore_list, list(sr_matrix)) 
  } 
   
  extErrorRecruitment_fore.model <<- gtkListStoreNew(rep("gdouble",(length(years_forecast)+1)), "gboolean")  



     for (i in 1:length(extErrorRecruitment_fore_list)) {
    iter <-  extErrorRecruitment_fore.model$append()$iter
    extErrorRecruitment_fore.model$set(iter,0, extErrorRecruitment_fore_list[[i]][1])
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years_forecast)) {
        extErrorRecruitment_fore.model$set(iter, e, as.double(extErrorRecruitment_fore_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     extErrorRecruitment_fore.model$set(iter,(length(years_forecast)+1),TRUE)
  } 

extErrorRecruitment_fore.treeview$destroy()
extErrorRecruitment_fore.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment_fore.model)
extErrorRecruitment_fore.treeview$setRulesHint(TRUE)
extErrorRecruitment_fore.treeview$getSelection()$setMode("single")
extErrorRecruitment_fore.add_columns(extErrorRecruitment_fore.treeview)
extErrorRecruitment_fore.sw$add(extErrorRecruitment_fore.treeview)      
  
  
}