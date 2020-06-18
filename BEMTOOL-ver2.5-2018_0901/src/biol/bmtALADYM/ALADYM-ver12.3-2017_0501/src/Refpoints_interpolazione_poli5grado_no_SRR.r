# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




YpR_data = read.table(file=YpR_table,sep=";",header=TRUE)

if (nb_gears==1){
colnames(YpR_data) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR")
} else {
colnames(YpR_data) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR",paste("Annual_F_estimated_",gears,sep=""),paste("Yield_",gears,sep=""),paste("Landing_",gears,sep=""))
}

recruitment <- mean(INP$Recruits[(forecast-INP$Average_forecast*12+1):forecast])

#variables
x <<- YpR_data[,colnames(YpR_data) == "Annual_F_estimated"]
y <<- YpR_data$Total_Yield/recruitment
SSB <<- YpR_data$SSB_exploited_pop/recruitment 
biomass <<- YpR_data$Total_biomass_exploited_pop/recruitment
z_ <<- y *recruitment - 0.1 * YpR_data[1,colnames(YpR_data)=="Total_biomass_exploited_pop"] * x # sulla prima riga deve stare assolutamente F=0!!!
t_ <<- y * recruitment - 0.2 * YpR_data[1,colnames(YpR_data)=="Total_biomass_exploited_pop"] * x # sulla prima riga deve stare assolutamente F=0!!!


# models
mod.nls_Y<-try(nls(y~a*x^5 + b * x^4 + c*x^3 + d*x^2 + e*x + f,start=list(a=1,b=1,c=1,d=1,e=1,f=1)),silent=T) 
mod.nls_ssb <- try(nls(SSB~exp(a+x*b),start=list(a=1,b=1)),silent=T)                      
mod.nls_b <- try(nls(biomass~exp(a+x*b),start=list(a=1,b=1)),silent=T)
mod.nls_01<-try(nls(z_~a*x^5 + b * x^4 + c*x^3 + d*x^2+e*x+f,start=list(a=1,b=1,c=1,d=1,e=1,f=1)),silent=T)
mod.nls_02 <-  try(nls(t_~a*x^5 + b * x^4 + c*x^3 + d*x^2+e*x+f,start=list(a=1,b=1,c=1,d=1,e=1,f=1)),silent=T)

# coefficients
if(!is(mod.nls_Y,"try-error")){
coeff <<- coefficients(mod.nls_Y)
pred_y<<- coeff[1]*x^5 + coeff[2] * x^4 + coeff[3]*x^3 + coeff[4]*x^2 + coeff[5]*x + coeff[6]
}
if(!is(mod.nls_ssb,"try-error")){
coeff_ssb <-coefficients(mod.nls_ssb) 
pred_ssb<<-exp(coeff_ssb[1] + x * coeff_ssb[2])
}
if(!is(mod.nls_b,"try-error")){
coeff_B <-coefficients(mod.nls_b) 
pred_b<<-exp(coeff_B[1] + x * coeff_B[2])
}
if(!is(mod.nls_01,"try-error")){  
coeff2 <- coefficients(mod.nls_01) #function V for F0.1 calculation   V(F): V(F)=Y(F)-(0.1*Bo*F) 
pred_v <<-  coeff2[1] * x^5 + coeff2[2] * x^4 + coeff2[3] * x^3 + coeff2[4] * x^2 + coeff2[5] * x + coeff2[6]
}
if(!is(mod.nls_02,"try-error")){
coeff3 <- coefficients(mod.nls_02) #function T for F0.2 calculation   T(F): T(F)=Y(F)-(0.2*Bo*F) 
pred_t <<-  coeff3[1] * x^5 + coeff3[2] * x^4 + coeff3[3] * x^3 + coeff3[4] * x^2 + coeff3[5] * x + coeff3[6]
}

#roots
if(!is(mod.nls_Y,"try-error")){
radici = polyroot(c(coeff[5],2*coeff[4],3*coeff[3],4*coeff[2],5*coeff[1])) }
if(!is(mod.nls_01,"try-error")){
radiciz = polyroot(c(coeff2[5],2*coeff2[4],3*coeff2[3],4*coeff2[2],5*coeff2[1]))
}
if(!is(mod.nls_02,"try-error")){
radicit = polyroot(c(coeff3[5],2*coeff3[4],3*coeff3[3],4*coeff3[2],5*coeff3[1]))
}


# yield-per-recruit curve
if (!is(mod.nls_Y,"try-error")){   # if yield-per-recruit is successfully interpolated
Fmax = Re(as.complex(radici))[Re(as.complex(radici))>=0][1] 
Yield_Fmax =  coeff[1]*Fmax^5 + coeff[2] * Fmax^4 + coeff[3]*Fmax^3 + coeff[4]*Fmax^2+coeff[5]*Fmax+coeff[6]
Fmax_obs = x[which(y==max(y))]
Yield_Fmax_obs=max(y)
} else {
Fmax = NA
Yield_Fmax = NA
Fmax_obs = x[which(y==max(y))]
Yield_Fmax_obs=max(y)
}

if (!is(mod.nls_01,"try-error")){
F01= Re(as.complex(radiciz))[Re(as.complex(radiciz))>=0][1] 
Yield_F01 =  coeff[1]*F01^5 + coeff[2] * F01^4 + coeff[3]*F01^3 + coeff[4]*F01^2+coeff[5]*F01+coeff[6]
F01_obs = x[which(z_==max(z_))] #empirical
Yield_F01_obs= y[which(z_==max(z_))]

} else { 
F01= NA
Yield_F01 = NA
F01_obs = x[which(z_==max(z_))] #empirical
Yield_F01_obs= y[which(z_==max(z_))]
}

if (!is(mod.nls_02,"try-error")){
F02= Re(as.complex(radicit))[Re(as.complex(radicit))>=0][1] 
Yield_F02 =  coeff3[1]*F02^5 + coeff3[2] * F02^4 + coeff3[3]*F02^3 + coeff3[4]*F02^2+coeff3[5]*F02+coeff3[6]
F02_obs = x[which(t_==max(t_))] #empirical
Yield_F02_obs= y[which(t_==max(t_))]

 } else {
F02= NA
Yield_F02 =  NA
F02_obs = x[which(t_==max(t_))] #empirical
Yield_F02_obs= y[which(t_==max(t_))]
}        


# FCURRENT VALUES ARE EMPIRICAL!
Fcurr_values = data.frame(matrix(nrow=1,ncol=5))
Fcurr_values[1] = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Annual_F_estimated"]
Fcurr_values[2] = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Total_Yield"]/recruitment #}   # empirical
Fcurr_values[3]<-YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "SSB_exploited_pop"] /recruitment
Fcurr_values[4]<-YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Total_biomass_exploited_pop"]  /recruitment
Fcurr_values[5]<- recruitment

# ssb values 
if (!is(mod.nls_ssb,"try-error")) {
SSB_max  <-exp(coeff_ssb[1]+coeff_ssb[2]*Fmax)
SSB_max_obs <- SSB[which(y==max(y))]
SSB_01  <-  exp(coeff_ssb[1]+coeff_ssb[2]*F01 )
SSB_01_obs<-SSB[which(z_==max(z_))]     
SSB_02  <- exp(coeff_ssb[1]+coeff_ssb[2]*F02)
SSB_02_obs<-SSB[which(t_==max(t_))]
} else {
SSB_max_obs<- SSB[which(y==max(y))]
SSB_max  <- NA
SSB_01 <- NA
SSB_01_obs<-SSB[which(z_==max(z_))]
SSB_02  =  NA
SSB_02_obs<-SSB[which(t_==max(t_))]
}

# BIOMASS VALUES
if (!is(mod.nls_b,"try-error")) {
B_max  <-exp(coeff_B[1]+coeff_B[2]*Fmax )
B_max_obs  <-biomass[which(y==max(y))] 
B_01  <- exp(coeff_B[1]+coeff_B[2]*F01 ) 
B_01_obs <-biomass[which(z_==max(z_))]      
B_02  <-exp(coeff_B[1]+coeff_B[2]*F02 )   
B_02_obs<-biomass[which(t_==max(t_))]
} else {
B_max_obs  <-biomass[which(y==max(y))]
B_max <- NA
B_01  <- NA
B_01_obs <-biomass[which(z_==max(z_))]
B_02  <-NA
B_02_obs<-biomass[which(t_==max(t_))]

} 


#if (INP$FRLt==4) {
Ref_points = data.frame(matrix(nrow=6,ncol=5))
Ref_points[1,1] = Fmax
Ref_points[1,2] = Yield_Fmax
Ref_points[1,3] = SSB_max
Ref_points[1,4] = B_max
Ref_points[2,1] = F01  
Ref_points[2,2] = Yield_F01
Ref_points[2,3] = SSB_01
Ref_points[2,4] = B_01 
Ref_points[3,1] = F02 
Ref_points[3,2] = Yield_F02 
Ref_points[3,3] = SSB_02  
Ref_points[3,4] = B_02

# OBSERVED VALUES
Ref_points[4,1] = Fmax_obs
Ref_points[4,2] = Yield_Fmax_obs
Ref_points[4,3] = SSB_max_obs
Ref_points[4,4] = B_max_obs
Ref_points[5,1] = F01_obs  
Ref_points[5,2] = Yield_F01_obs
Ref_points[5,3] = SSB_01_obs
Ref_points[5,4] = B_01_obs 
Ref_points[6,1] = F02_obs 
Ref_points[6,2] = Yield_F02_obs 
Ref_points[6,3] = SSB_02_obs  
Ref_points[6,4] = B_02_obs
Ref_points[,5] = recruitment

Ref_points = rbind(Fcurr_values,Ref_points)
Reference_points = cbind(c("Fcurrent","Fmax", "F0.1", "F0.2","Fmax_empirical", "F0.1_empirical", "F0.2_empirical"),Ref_points)
colnames(Reference_points) = c("Reference point","Annual_F_estimated","Total Yield-per-Recruit","SSB-per-Recruit","B-per-Recruit","Recruits")


write.table(Reference_points, file=REFERENCEPOINTS_table,sep=";",row.names=FALSE)

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
#save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\Tables\\[", casestudy_name, "] Reference_points.csv", sep="")
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
#write.table(Reference_points, save_path,row.names=FALSE, sep=";")
