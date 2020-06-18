# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#
# ------------------------------------------------------------------------------
# add elements to the list of total mortality values (FEMALES)
# ------------------------------------------------------------------------------
#
add.escape_survival_extvector_M_fore <- function() {
 first_age_mal <- 0
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan)) 
          n_ages <- n_ages - trunc( 0 /12)
    first_age_mal <- trunc(0/12) 
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])  
            n_ages <- n_ages - trunc(INP$tr/12)
    first_age_mal <- trunc(INP$tr/12)      
} 

#print("Adding elements to the list...")   
  if (!is.null(escape_surv_extvector_Mtable_fore)) {
  for (r in 1:nrow(escape_surv_extvector_Mtable_fore)) {
  FF_temp <- as.list(escape_surv_extvector_Mtable_fore[r,]) 
  # names(FF_temp) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))
    names(FF_temp) <-  paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") 
  escape_survival_extvector_M_list_fore <<- c(escape_survival_extvector_M_list_fore, list(FF_temp)) 
  } 
   } else {
   FF_matrix <- data.frame(matrix(1, nrow=1 , ncol=n_ages))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c(paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  escape_survival_extvector_M_list_fore <<- c(escape_survival_extvector_M_list_fore, list(FF_temp)) 
  }
 }
 #print("Discard (FEMALES) successfully added to the list!", quote=F)
}

