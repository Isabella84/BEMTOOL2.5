# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




SGEAR <- function(SG_TYPE,param1, param2, param3, param4, param5, para_Length_Average) {

if (showCompTime)  {
SGEAR_ptm <- proc.time()
}



loca_temp <- 0

  # OGIVE-BASED
  
  if (SG_TYPE == 1) {   # 1. classical ogive
    loca_temp <- 1 / (1 + exp((log(9) / param2) * (param1 - para_Length_Average)))
  }

  if (SG_TYPE == 2) {     # 2. coupled ogive
    loca_L75p <- ((log(9)/param2)*param1+log(3))/(log(9)/param2)
    loca_D75p <- (param1 + param3) - loca_L75p
    loca_DSR  <- 2*(param3 - loca_D75p)
    loca_temp <- 1/(1+exp((log(9)/param2)*(param1 - para_Length_Average)))*1/(1+exp((-log(9)/loca_DSR) * (param3 - para_Length_Average)))
  }

  # GAUSSIAN-BASED
 if (SG_TYPE == 3) {     # 3. gaussian
 loca_temp <- exp(-((para_Length_Average-param1)^2)/(2*param2^2))
  # S(L)= exp(-(L-Lm)^2/(2*s^2)) 
}

if (SG_TYPE == 4) {        # 4. log-normal
loca_temp <- exp(param1-((param2^2)/2)-((log(para_Length_Average)-param1)^2)/(2*param2))/para_Length_Average 
}

 # ASYMMETRIC
if (SG_TYPE == 5) { # 5. bi-normal
#B =  1/(1 + param5*exp(((param1-param3)^2)/(2*param4^2)))
loca_temp <-  (exp(-((para_Length_Average-param1)^2)/(2*param2^2)) + param5 * exp(-((para_Length_Average-param3)^2)/(2*param4^2)))         #  B *
 #para_Length_Average=BAS_3$FLength
 # loca_temp <-  (exp(-((para_Length_Average-INP_3$param1[1])^2)/(2*INP_3$param2[1]^2)) + INP_3$param5[1] * exp(-((para_Length_Average-INP_3$param3[1])^2)/(2*INP_3$param4[1]^2)))         #  B *
}

if (SG_TYPE == 6) {   # 6. two-sided

loca_temp[which(para_Length_Average <=param1)] <- exp(-((para_Length_Average[which(para_Length_Average <=param1)]-param1)^2)/(2*param2^2))

loca_temp [which(para_Length_Average > param1)]<- exp(-((para_Length_Average[which(para_Length_Average > param1)]-param1)^2)/(2*param3^2)) 
}


if (showCompTime)  {
proc_ <- proc.time()
#print(paste("SGEAR [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-SGEAR_ptm[3]),2), "sec" ) , quote=F )    
#print(proc.time() - SGEAR_ext_vec_ptm, quote=F ) 
 rm(SGEAR_ptm)
}


#loca_temp[is.na(loca_temp)]<-0
  return(loca_temp)
}


# S(L)=1/(1+exp((2*ln(3)/SR)*(L50%-L))*1/(1+exp((-2*ln(3)/DSR)*(D50%-L))
#sel_mod1=SGEAR(1,15,5,NA,NA,NA,seq(1,60,1))
#sel_mod2=SGEAR(2,15,5,40,NA,NA,seq(1,60,1))
#sel_mod3=SGEAR(3,30,5,NA,NA,NA,seq(1,60,1))
#sel_mod4=SGEAR(4,log(30),log(5),NA,NA,NA,seq(1,60,1))
#sel_mod5=SGEAR(6,15,5,8,4,0.5,seq(1,60,1))
#sel_mod6=SGEAR(6,15,5,8,NA,NA,seq(1,60,1))
##
##
#par(mfrow=c(3,2))
#plot(sel_mod1,type="l",xlab="Length",main="classical ogive", ylab="")
#plot(sel_mod2,type="l",xlab="Length",main="deselection ogive", ylab="")
#plot(sel_mod3,type="l",xlab="Length",main="normal", ylab="")
#plot(sel_mod4,type="l",xlab="Length",main="log-normal", ylab="")
#plot(sel_mod5,type="l",xlab="Length",main="bi-normal", ylab="")
#plot(sel_mod6,type="l",xlab="Length",main="two-sided", ylab="")
###
#sel= SGEAR(5,60, 2, 20, 2, 0.5, seq(1,100,1) )
#plot( seq(1,100,1),sel)
