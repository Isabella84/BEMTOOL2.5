# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




DISCARDS<-function(Discard,param6,param7,Length){
loca_temp <- 0
if (!is.na(Discard)& as.character(Discard)=="0"){
Length <- 0
loca_temp <- Length
} else if(!is.na(Discard)& as.character(Discard)=="Y"){
loca_temp <- 1 / (1 + exp(-(log(9) / param7) * (param6 - Length)))
} 
 else if(is.na(Discard)){
 Length[] <- NA
loca_temp <- Length
 }
return(loca_temp)
}





#plot(logistic(p,t50,R,seq(-1,10,1))*100,type="l",col="red",lwd=5,xlab="year",ylab="% reduction")
#abline(c(0.61*100,seq(1,10,1)),c(0,0))
#abline(c(100,seq(1,10,1)),c(0,0))
#title("multiplier logistic reduction")
