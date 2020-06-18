# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

get_grid_selectivity <- function(loca_selectivity_distribution, loca_nruns) {

# loca_selectivity_distribution = selectivity_uncertainty_distribution_matrix
# loca_nruns= nruns

head_ <- c("run_N",	"param1",	"param2",	"param3",	"param4",	"param5",	"sel_type",	"fleet")
GRID_allfleets <-  data.frame(matrix(ncol=length(head_), nrow=0))
 colnames(GRID_allfleets) <- head_
 
for (nro in 1:nrow(loca_selectivity_distribution) ) {
 GRID <-  data.frame(matrix(ncol=length(head_), nrow=loca_nruns))
 colnames(GRID) <- head_

  # PARAM 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.na(loca_selectivity_distribution$param1_a[nro]) ) {
     for (i in 1:loca_nruns) {
       to_check1 =-1
     while (to_check1 < 0) {
         if (loca_selectivity_distribution$distribution[nro] == "Normal") {
           to_check1=rnorm(1,mean=loca_selectivity_distribution$param1_a[nro],sd=loca_selectivity_distribution$param1_b[nro]) 
      } else if (loca_selectivity_distribution$distribution[nro] == "Uniform") {
           to_check1= runif(1,min=loca_selectivity_distribution$param1_a[nro],max=loca_selectivity_distribution$param1_b[nro])
      } else if (loca_selectivity_distribution$distribution[nro] == "Lognormal"){
           to_check1= rlnorm(1,mean=loca_selectivity_distribution$param1_a[nro],sd=loca_selectivity_distribution$param1_b[nro])
      }
     }
        GRID[i,2]=to_check1

  } 
   }
  # PARAM 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if (!is.na(loca_selectivity_distribution$param2_a[nro]) ) {
     for (i in 1:loca_nruns) { 
        to_check1 =-1
       while (to_check1 < 0) {    
       if (loca_selectivity_distribution$distribution[nro] == "Normal") {
           to_check1=rnorm(1,mean=loca_selectivity_distribution$param2_a[nro],sd=loca_selectivity_distribution$param2_b[nro]) 
      } else if (loca_selectivity_distribution$distribution[nro] == "Uniform") {
           to_check1= runif(1,min=loca_selectivity_distribution$param2_a[nro],max=loca_selectivity_distribution$param2_b[nro])
      } else if (loca_selectivity_distribution$distribution[nro] == "Lognormal"){
           to_check1= rlnorm(1,mean=loca_selectivity_distribution$param2_a[nro],sd=loca_selectivity_distribution$param2_b[nro])
      }
      
          GRID[i,3]=to_check1
  } 
  }
   }
  
  # PARAM 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          if (!is.na(loca_selectivity_distribution$param3_a[nro]) ) {

    for (i in 1:loca_nruns) {
       to_check1 =-1
      while (to_check1 < 0) {     
       if (loca_selectivity_distribution$distribution[nro] == "Normal") {
           to_check1=rnorm(1,mean=loca_selectivity_distribution$param3_a[nro],sd=loca_selectivity_distribution$param3_b[nro]) 
      } else if (loca_selectivity_distribution$distribution[nro] == "Uniform") {
           to_check1= runif(1,min=loca_selectivity_distribution$param3_a[nro],max=loca_selectivity_distribution$param3_b[nro])
      } else if (loca_selectivity_distribution$distribution[nro] == "Lognormal"){
           to_check1= rlnorm(1,mean=loca_selectivity_distribution$param3_a[nro],sd=loca_selectivity_distribution$param3_b[nro])
      }
      
          GRID[i,4]=to_check1

      }  
  } 
  }

  # PARAM 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            if (!is.na(loca_selectivity_distribution$param4_a[nro]) ) {

     for (i in 1:loca_nruns) {
        to_check1 =-1  
       while (to_check1 < 0) {   
       if (loca_selectivity_distribution$distribution[nro] == "Normal") {
           to_check1=rnorm(1,mean=loca_selectivity_distribution$param4_a[nro],sd=loca_selectivity_distribution$param4_b[nro]) 
      } else if (loca_selectivity_distribution$distribution[nro] == "Uniform") {
           to_check1= runif(1,min=loca_selectivity_distribution$param4_a[nro],max=loca_selectivity_distribution$param4_b[nro])
      } else if (loca_selectivity_distribution$distribution[nro] == "Lognormal"){
           to_check1= rlnorm(1,mean=loca_selectivity_distribution$param4_a[nro],sd=loca_selectivity_distribution$param4_b[nro])
      }
   
          GRID[i,5]=to_check1
  } 
  }
  }
  
  # PARAM 5 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              if (!is.na(loca_selectivity_distribution$param5_a[nro]) ) {
  
     for (i in 1:loca_nruns) {
        to_check1 =-1
          while (to_check1 < 0) {     
       if (loca_selectivity_distribution$distribution[nro] == "Normal") {
           to_check1=rnorm(1,mean=loca_selectivity_distribution$param5_a[nro],sd=loca_selectivity_distribution$param5_b[nro]) 
      } else if (loca_selectivity_distribution$distribution[nro] == "Uniform") {
           to_check1= runif(1,min=loca_selectivity_distribution$param5_a[nro],max=loca_selectivity_distribution$param5_b[nro])
      } else if (loca_selectivity_distribution$distribution[nro] == "Lognormal"){
           to_check1= rlnorm(1,mean=loca_selectivity_distribution$param5_a[nro],sd=loca_selectivity_distribution$param5_b[nro])
      }

          GRID[i,6]=to_check1

      }  
  } 
  }
  
  GRID$run_N <- c(1:loca_nruns)
   GRID$sel_type <- loca_selectivity_distribution$sel_type[nro]
    GRID$fleet <- loca_selectivity_distribution$fleet[nro]
          
 GRID_allfleets <- data.frame(rbind(GRID_allfleets, GRID))

      
}


return(GRID_allfleets)
}