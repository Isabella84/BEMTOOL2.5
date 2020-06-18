# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
# ------------------------------------------------------------------------------
# add elements to the list of total mortality values (MALES)
# ------------------------------------------------------------------------------
#
add.FishingMvector_M_overall <- function() {
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
   n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 
first_age_mal <- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)


#print("Adding elements to the list...")   
  if (!is.null(FM_overall_matrix)) {
  for (r in 1:nrow(FM_overall_matrix)) {
  FM_temp <- as.list(FM_overall_matrix[r,]) 
  # names(FM_temp) <- c("year", paste("age", c(0:(n_ages-1)), sep="") )
  names(FM_temp) <-  c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep=""))
  FishingMvector_M_overall_list <<- c(FishingMvector_M_overall_list, list(FM_temp)) 
  }
   } else {
   FM_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(length(c(first_age_fem:(n_ages+first_age_fem-1)))+1)))
  #  colnames(FM_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))
  colnames(FM_matrix) <-  c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     FM_matrix$year <- years
   for (r in 1:nrow(FM_matrix)) { 
  FM_temp <- as.list(FM_matrix[r,]) 
  FishingMvector_M_overall_list <<- c(FishingMvector_M_overall_list, list(FM_temp)) 
  }
 }
# print("Fishing mortality (MALES) successfully added to the list!", quote=F)
}

