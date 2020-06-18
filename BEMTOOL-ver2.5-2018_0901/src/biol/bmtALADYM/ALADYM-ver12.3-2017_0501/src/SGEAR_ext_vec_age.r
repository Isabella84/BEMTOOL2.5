# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

SGEAR_ext_vec_age <- function(loca_sex, vect_age) {             
# Sel_vect: matrix with the length or age classes on the first column and the proportion of retained by class on the second column
# Type: "A" (age) or "L" (length)

if (showCompTime)  {
SGEAR_ext_vec_age_ptm <- proc.time()
}

if (FALSE) {
  loca_sex = "F"
  vect_age =  loca_Age
}             
   
loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (INP$Year_simulation == length(years)) {
       if (loca_sex == "M") {
    Sel_vector = INP$Sel_vectorM
    } else {
    Sel_vector = INP$Sel_vectorF 
    }
} else {
     if (loca_sex == "M") {
    Sel_vector = INP$Sel_vectorM_fore
    } else {
    Sel_vector = INP$Sel_vectorF_fore 
    }
}
  

 allSelectivity_age <- vector(mode = "list", length = length(FLEETSEGMENTS_names))                  # GUI: ciclo sulla lista offspring
   
for (loca_f in 1:length(FLEETSEGMENTS_names)) {

if (INP$Year_simulation == length(years)) {
 selectAge_mat <-  data.frame(matrix(NA, nrow= length(years), ncol=length( vect_age)))
   all_years <- years
 } else {
 selectAge_mat <-  data.frame(matrix(NA, nrow= length(years_forecast), ncol=length( vect_age))) 
   all_years <- years_forecast
 }
    Type = INP$Type[loca_f]
  
      if (is.na(Type)) {
       Type=""
   }
  
    for (loca_y in 1:length(all_years)) {
    
  if (INP$Year_simulation == length(years)) {  
      loca_Sel_vector = Sel_vector[Sel_vector$Year==years[loca_y],c(1, loca_f+2)]
      } else {
       loca_Sel_vector = Sel_vector[Sel_vector$Year==years_forecast[loca_y],c(1, loca_f+2)]     
      }
            loca_temp <- vect_age
            loca_temp[] <- 0
   

            
 if (Type=="A")  {
rounded_age = round(vect_age,0)
#print(rounded_age)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! attenzione da eliminareeeeeeeeeeeeeeeeee
rounded_age[rounded_age == rounded_age[length(rounded_age)]] <- max(rounded_age)-1
#print(rounded_age)

for (i in 1:length(rounded_age)) {
loca_temp[i] <- loca_Sel_vector[loca_Sel_vector[,1]  == rounded_age[i],2]
}

selectAge_mat[loca_y,] <- loca_temp

}                     
      }
      allSelectivity_age[[loca_f]] <- selectAge_mat
}


if (showCompTime)  {
proc_ <- proc.time()
print(paste("SGEAR_ext_vec_age [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-SGEAR_ext_vec_age_ptm[3]),2), "sec" ) , quote=F )    
#print(proc.time() - SGEAR_ext_vec_ptm, quote=F ) 
 rm(SGEAR_ext_vec_age_ptm)
}

return(allSelectivity_age)
}

#INP$SG_TYPE=7
#para_Length_Age_Average=BAS$MLength
# Sel_vector = data.frame(read.table("Selectivity_ext.csv", sep=";",header=T)   )
# Sel_vector= Sel_vector[as.character(Sel_vector$Sex)=="M",]
# Sel_vector=Sel_vector[,c(1,2)]
# Type ="L"
# Type ="A"
# para_Length_Age_Average=BAS$MAge
# Sel_vector = data.frame(read.table("Selectivity_ext_age.csv", sep=";",header=T)   )
# Sel_vector= Sel_vector[as.character(Sel_vector$Sex)=="M",]
# Sel_vector=Sel_vector[,c(1,2)]
