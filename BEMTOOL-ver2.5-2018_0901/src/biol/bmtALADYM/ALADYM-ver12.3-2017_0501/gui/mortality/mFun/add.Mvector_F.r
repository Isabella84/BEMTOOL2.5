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
# ------------------------------------------------------------------------------
# add elements to the list of natural mortality values (FEMALES)
# ------------------------------------------------------------------------------
#
add.Mvector_F <- function() {
#print("Adding elements to the list...")   
  # mortality.Mvector.males <- read.csv("C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\Aladym_ r_9.2.2_GUI-ver\\gui\\TEMPLATE_naturalmortality.csv", sep=";")
    
    months_vec_F <-  months_vec_F[months_vec_F >= (Tr)]
    
    MF_matrix <- data.frame(matrix(0, nrow = length(months_vec_F), ncol=2)) 
   heading <- c("age_month", "M")
   colnames(MF_matrix) <- heading
   MF_matrix$age_month <- months_vec_F
  if (!is.null(mortality.Mvector.females)) {
   MF_matrix$M <- mortality.Mvector.females$M[as.numeric(as.character(mortality.Mvector.females$age_month)) >= Tr]
  } else {
      MF_matrix$M <- 0
  }

   for (r in 1:nrow(MF_matrix)) { 
  MF_temp <- as.list(MF_matrix[r,]) 
  Mvector_F <<- c(Mvector_F, list(MF_temp)) 
  }

   mortality.Mvector.females  <<- MF_matrix

#print("Natural mortality (FEMALES) successfully added to the list!", quote=F)

}
