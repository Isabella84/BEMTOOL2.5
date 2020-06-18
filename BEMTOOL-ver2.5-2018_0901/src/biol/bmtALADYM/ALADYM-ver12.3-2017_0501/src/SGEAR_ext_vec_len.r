# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

SGEAR_ext_vec_len <- function(loca_sex, vect_L, prelife) {             
# Sel_vect: matrix with the length or age classes on the first column and the proportion of retained by class on the second column
# Type: "A" (age) or "L" (length)

if (showCompTime)  {
SGEAR_ext_vec_len_ptm <- proc.time()
}


if (FALSE) {
  loca_sex = "F"
  vect_L =  BAS$FLength
  prelife = F
}             
  
  
   
loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (prelife) {

 if (loca_sex == "M") {
    Sel_vector = INP$Sel_vector_lenM[INP$Sel_vector_lenM$Year == years[1], ]
    } else {
    Sel_vector = INP$Sel_vector_lenF[INP$Sel_vector_lenF$Year == years[1], ]
    }

} else {

if (INP$Year_simulation == length(years)) {
       if (loca_sex == "M") {
    Sel_vector = INP$Sel_vector_lenM
    } else {
    Sel_vector = INP$Sel_vector_lenF 
    }
} else {
     if (loca_sex == "M") {
    Sel_vector = INP$Sel_vector_lenM_fore
    } else {
    Sel_vector = INP$Sel_vector_lenF_fore
    }
}
  
  }

 allSelectivity_len <- vector(mode = "list", length = length(FLEETSEGMENTS_names))                  # GUI: ciclo sulla lista offspring
   
for (loca_f in 1:length(FLEETSEGMENTS_names)) {

if (prelife) {
     selectLen_mat <-  data.frame(matrix(NA, nrow= 1, ncol=length( vect_L)))
      all_years <- years[1]
} else {
if (INP$Year_simulation == length(years)) {
 selectLen_mat <-  data.frame(matrix(NA, nrow= length(years), ncol=length( vect_L)))
   all_years <- years
 } else {
 selectLen_mat <-  data.frame(matrix(NA, nrow= length(years_forecast), ncol=length( vect_L))) 
   all_years <- years_forecast
 }
 
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
            loca_temp <- vect_L
            loca_temp[] <- 0
            
      if (Type=="L") {
rounded_size = round(vect_L,0)
# print(rounded_size)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! attenzione da eliminareeeeeeeeeeeeeeeeee
# rounded_size[rounded_size == rounded_size[length(rounded_size)]] <- max(rounded_size)-1
#print(rounded_age)

for (i in 1:length(rounded_size)){
loca_temp[i] <- loca_Sel_vector[loca_Sel_vector[,1]  == rounded_size[i],2]
}

selectLen_mat[loca_y,] <- loca_temp

}                    
      }
      allSelectivity_len[[loca_f]] <- selectLen_mat
}


if (showCompTime & !prelife)  {
proc_ <- proc.time()
print(paste("SGEAR_ext_vec_len [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-SGEAR_ext_vec_len_ptm[3]),2), "sec" ) , quote=F )    
#print(proc.time() - SGEAR_ext_vec_ptm, quote=F ) 
 rm(SGEAR_ext_vec_len_ptm)
}

return(allSelectivity_len)
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
