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

# variables
x <<- YpR_data[,colnames(YpR_data) == "Annual_F_estimated"]
y <<- YpR_data$Total_Yield
ssb <<- YpR_data$SSB_exploited_pop 
xx <<- YpR_data[,colnames(YpR_data) == "Annual_Z_estimated"]
yy <<- YpR_data$Biological_production
x_sub_set<-x	
y_sub_set<-y	
biomass = YpR_data$Total_biomass_exploited_pop
xx_sub_set<-xx
yy_sub_set<-yy




# models and coefficients
coeff <-coefficients(nls(y_sub_set~a*x_sub_set^2 + b * x_sub_set + c,start=list(a=1,b=1,c=0))) 
coeff_ssb <-try(coefficients(nls(ssb~exp(a + x_sub_set*b),start=list(a=1,b=1))),silent=T)
coeff_B <- try(coefficients(nls(biomass~exp(a + x_sub_set*b),start=list(a=1,b=1))),silent=T)
coeffZmbp <- try(coefficients(nls(yy_sub_set~a*xx_sub_set^2 + b * xx_sub_set + c,start=list(a=1,b=1,c=0))),silent=T)        # calcolo Zmbp   solo se c'è S-R relationship
coeffFmbp <- try(coefficients(nls(yy_sub_set~a*x_sub_set^2 + b * x_sub_set + c,start=list(a=1,b=1,c=0))),silent=T) 
coeff_lm = coefficients(lm (xx~x) )   # linear model Z  ~ F




if(!is(coeff,"try-error")){
pred_y<<-  coeff[1]*x_sub_set^2 + coeff[2] * x_sub_set + coeff[3] 
Fmax = -coeff[2]/(2*coeff[1]) # as.real(radici)[1]
Yield_Fmax =  coeff[1]*Fmax^2 + coeff[2] * Fmax + coeff[3]# coeff[1] * Fmax^2 + coeff[2] * Fmax + coeff[3]
} else {
Fmax =   x_sub_set[which(y_sub_set==max(y_sub_set))]
Yield_Fmax = max(y_sub_set) 
}

if(!is(coeffZmbp,"try-error")){
Zmbp = -coeffZmbp[2]/(2*coeffZmbp[1]) # as.real(radicimbp)[1]
} else {
Zmbp = xx_sub_set[which(yy_sub_set==max(yy_sub_set))]     # otherwise empirical
}

if(!is(coeffFmbp,"try-error")){
Fmbp = -coeffFmbp[2]/(2*coeffFmbp[1])
Bp_Fmbp =  coeffFmbp[1]*Fmbp^2 + coeffFmbp[2] * Fmbp + coeffFmbp[3]# coeff[1] * Fmax^2 + coeff[2] * Fmax + coeff[3]
BP_Fmax= coeffFmbp[1]*Fmax^2 + coeffFmbp[2] * Fmax + coeffFmbp[3]

} else {
Fmbp = x_sub_set[which(yy_sub_set==max(yy_sub_set))]
Bp_Fmbp =  yy_sub_set [which(x_sub_set==Fmbp)]
BP_Fmax=    yy_sub_set [which(abs(x_sub_set-Fmax)<0.01)][1]
}

if(!is(coeff,"try-error")){
Yield_Fmbp = coeff[1]*Fmbp^2 + coeff[2] * Fmbp + coeff[3]
} else {
Yield_Fmbp = y_sub_set[which(abs(x_sub_set-Fmax)<0.01)][1]
}


if(!is(coeff_B,"try-error")){
pred_b <<-exp(coeff_B[1] + x_sub_set * coeff_B[2])
B_max  <-exp(coeff_B[1]+coeff_B[2]*Fmax ) 
B_mbp  <-exp(coeff_B[1]+coeff_B[2]*Fmbp )
} else {
B_max  <- biomass[which(y==max(y))]   # otherwise empirical value
B_mbp  <-  biomass [which(abs(x_sub_set-Fmbp)<0.01)][1]
}

if(!is(coeff_ssb,"try-error")){
pred_ssb <<-  exp(coeff_ssb[1]+coeff_ssb[2]*x_sub_set )
SSB_max  <-exp(coeff_ssb[1]+coeff_ssb[2]*Fmax)
ssb_Fmbp <- exp(coeff_ssb[1]+coeff_ssb[2]*Fmbp)
} else {
SSB_max  <- ssb[which(y==max(y))]    # otherwise empirical value
ssb_Fmbp <- ssb [which(abs(x_sub_set-Fmbp)<0.01)][1]
}


# CURRENT VALUES ARE EMPIRICAL

Fcurr_values = data.frame(matrix(nrow=1,ncol=6))
Fcurr_values[1] = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Annual_F_estimated"]
Fcurr_values[2] = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Annual_Z_estimated"]   # Z
Fcurr_values[3] =  YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) ==  "Total_Yield"]  # Y
Fcurr_values[4]  = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) ==  "SSB_exploited_pop"]       # SSB
Fcurr_values[5]  = YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Biological_production"]  # biological production
Fcurr_values[6]  <-  YpR_data[which(YpR_data[,1]==1.0),colnames(YpR_data) == "Total_biomass_exploited_pop"]  #biomass


Ref_points = data.frame(matrix(nrow=2,ncol=6))
Ref_points[1,1] = Fmax                          #Fmsy
Ref_points[1,2] = coeff_lm[1]+coeff_lm[2]*Fmax     # Z associata a Fmsy
Ref_points[1,3] = Yield_Fmax  
Ref_points[1,4] =  SSB_max
Ref_points[1,5] = BP_Fmax
Ref_points[1,6] = B_max
Ref_points[2,1] = Fmbp
Ref_points[2,2] = coeff_lm[1]+coeff_lm[2]*Fmbp 
Ref_points[2,3] = Yield_Fmbp
Ref_points[2,4] = ssb_Fmbp
Ref_points[2,5]  = Bp_Fmbp
Ref_points[2,6] = B_mbp


colnames(Fcurr_values) = c("Annual_F_estimated","Annual_Z_estimated","Total_Yield","SSB","BP","B")
colnames(Ref_points) = c("Annual_F_estimated","Annual_Z_estimated","Total_Yield","SSB","BP","B")
Ref_points = rbind(Fcurr_values,Ref_points)
Reference_points = cbind(c("Fcurrent","Fmsy","Fmbp"),Ref_points)

colnames(Reference_points) = c("Reference_point","Annual_F_estimated","Annual_Z_estimated","Total_Yield","SSB","BP","B")
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
