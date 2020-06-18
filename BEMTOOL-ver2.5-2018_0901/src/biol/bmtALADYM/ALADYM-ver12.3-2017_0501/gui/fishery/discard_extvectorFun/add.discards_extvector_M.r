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
add.discards_extvector_M <- function() {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

first_age_mal <- 0

#if (modulo(Tr, 12) == 0 & Tr!=0) {
#    n_ages <- n_ages - trunc(Tr/12) + 1
#    first_age_mal <- trunc(Tr/12) - 1
#} else if (trunc(Tr/12) > 0 & Tr!=0) {
    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
#} 

#print("Adding elements to the list...")   
  if (!is.null(fleet.discard_extvector_M)) {
  for (r in 1:nrow(fleet.discard_extvector_M)) {
  FF_temp <- as.list(fleet.discard_extvector_M[r,]) 
  # names(FF_temp) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))
    names(FF_temp) <-  c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
  discards_extvector_M_list <<- c(discards_extvector_M_list, list(FF_temp)) 
  }
   } else {
   FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1)))
     # colnames(FF_matrix) <- c("year",paste("age", c(0:(n_ages-1)), sep=""))   
      colnames(FF_matrix) <-   c("year",paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     FF_matrix$year <- years
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  discards_extvector_M_list <<- c(discards_extvector_M_list, list(FF_temp)) 
  }
 }
 #print("Discard (FEMALES) successfully added to the list!", quote=F)
}

