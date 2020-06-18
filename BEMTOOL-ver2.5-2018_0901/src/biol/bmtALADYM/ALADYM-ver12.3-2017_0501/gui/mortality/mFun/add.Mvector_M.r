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
# add elements to the list of natural mortality values (MALES)
# ------------------------------------------------------------------------------
#
add.Mvector_M <- function() {

months_vec_M <-  months_vec_M[months_vec_M >= Tr]

#print("Adding elements to the list...")   
  # mortality.Mvector.males <- read.csv("C:\\FACCHINI_MT\\SOFTWARE COISPA\\under_construction\\Aladym_ r_9.2.2_GUI-ver\\gui\\TEMPLATE_naturalmortality.csv", sep=";")
   MM_matrix <- data.frame(matrix(0, nrow = length(months_vec_M), ncol=2)) 
   heading <- c("age_month", "M")
   colnames(MM_matrix) <- heading
   MM_matrix$age_month <- as.character(months_vec_M)
  if (!is.null(mortality.Mvector.males)) {
   MM_matrix$M <- mortality.Mvector.males$M[as.numeric(as.character(mortality.Mvector.males$age_month)) >= Tr]
   #print(MM_matrix)
  } else {
      MM_matrix$M <- 0
  }
   #print(MM_matrix)
   for (r in 1:nrow(MM_matrix)) { 
  MM_temp <- as.list(MM_matrix[r,]) 
  Mvector_M <<- c(Mvector_M, list(MM_temp)) 
  }

     mortality.Mvector.males  <<- MM_matrix
# print("Natural mortality (MALES) successfully added to the list!", quote=F)

}