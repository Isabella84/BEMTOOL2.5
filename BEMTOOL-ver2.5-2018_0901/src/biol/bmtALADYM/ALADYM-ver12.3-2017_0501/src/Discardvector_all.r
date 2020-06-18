# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# THIS FUNCTION TRANSFORMS THE PROPORTION OF DISCARDED INDIVIDUALS BY AGE (IN YEARS)IN MONTHLY VECTOR 

Discardvector_all<-function(loca_sex, vect_L, prelife)   {

if (showCompTime)  {
Discardvector_all_ptm <- proc.time()  
}

if (FALSE) {
  loca_sex = "M"
  vect_L =  BAS$MLength
  prelife = F
}             
 
loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (prelife) {

 if (loca_sex == "M") {
    Dis_vector =  INP$D_vectorM_sim[INP$D_vectorM_sim$Year==years[1],]
    } else {
    Dis_vector = INP$D_vectorF_sim[INP$D_vectorF_sim$Year==years[1],]
    }

} else {

if (INP$Year_simulation == length(years)) {
       if (loca_sex == "M") {
      Dis_vector =  INP$D_vectorM_sim
    } else {
          Dis_vector =  INP$D_vectorF_sim
    }
} else {
     if (loca_sex == "M") {
    Dis_vector = INP$D_vectorM_fore
    } else {
    Dis_vector = INP$D_vectorF_fore
    }
}
  
  }
  
  
  allDiscard_len <- vector(mode = "list", length = length(FLEETSEGMENTS_names))
      
  
  for (loca_f in 1:length(FLEETSEGMENTS_names)) {

if (prelife) {
     discLen_mat <-  data.frame(matrix(NA, nrow= 1, ncol=length( vect_L)))
      all_years <- years[1]
} else {
if (INP$Year_simulation == length(years)) {
 discLen_mat <-  data.frame(matrix(NA, nrow= length(years), ncol=length( vect_L)))
   all_years <- years
 } else {
 discLen_mat <-  data.frame(matrix(NA, nrow= length(years_forecast), ncol=length( vect_L))) 
   all_years <- years_forecast
 }
 
 }
 
 
 if (loca_sex == "F") {

     n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])     
      n_ages <- n_ages - trunc(INP$tr/12)
 } else {

     n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])          
     n_ages <- n_ages - trunc(INP$tr/12)
 }
 
 first_age <- trunc(INP$tr/12)

ages_vect <- c(first_age:(n_ages+first_age-1))    
 
nyears = length(all_years)    
D_matrix = matrix (0, ncol=length(vect_L), nrow=nyears)

   for (i in 1:nyears) {
 
# modulo(INP$tr,12) 
  first_months_of_life <- 1:((first_age+1)*12-INP$tr+1)
 D_matrix[i , c(first_months_of_life)] = Dis_vector[Dis_vector$Year == all_years[i] &  Dis_vector$Age ==  ages_vect[1] , loca_f+2]
#  print(first_months_of_life) 
    
   for (ag in 2:(length(ages_vect))) {
                         # (12-tr+2):length(loca_Length_Average)
     if (first_age > 0) {  
  months_12 <- ((first_age+ages_vect[ag-1])*12-INP$tr+2):((first_age+ages_vect[ag])*12-INP$tr+1)  
  } else {
  months_12 <- ((ag-1)*12-INP$tr+2):((ag)*12-INP$tr+1)  
  }              
      D_matrix[i, months_12] = Dis_vector[Dis_vector$Year == all_years[i] &  Dis_vector$Age ==  ages_vect[ag] , loca_f+2]
      
#      print(months_12)
    }
    
 }
  
allDiscard_len[[loca_f]] <- D_matrix
}
 
    
 if (showCompTime)  {
 proc_ <- proc.time()
print(paste("Discardvector_all [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-Discardvector_all_ptm[3]),2), "sec" ), quote=F )   
rm(Discardvector_all_ptm)
}
    
return(allDiscard_len)
}
