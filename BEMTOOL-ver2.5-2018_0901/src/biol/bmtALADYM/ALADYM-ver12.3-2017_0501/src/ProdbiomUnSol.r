# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

#ProdbiomUnSol(Linf,k,t0,tmax,Mtmax,l50,a,b,ages)  {


ProdbiomUnSol <- function (Linf,k,t0,tmax,Mtmax,l50,a,b,ages)  {

if (FALSE) {
a=INP$MWLa
b=INP$MWLb
Linf = 368.8345342
k = 0.34045314
t0 = -1.2
loca_Age =  BAS$FAge
l50=mean(c(INP$FML50p_min, INP$FML50p_max))
tmax=INP$MGrowth_tend - 1
Mtmax=0.2
ages = BAS$MAge
}


#if(INP$tr>0){
#tl=INP$tr} else {
t1=1/365
#}
tm =t0 -log(1-l50/Linf)/k


loca_M = data.frame(matrix(0,ncol=length(ages),nrow=1))
#if (INP$FOPT_M_TYPE == 2) {
r=t1    
B=(b*log((1-exp(-k*(tmax-t0)))/(1-exp(-k*(r-t0))))-Mtmax*(tmax-r)) / (log(tmax/r) - log(tmax/tm)*(tmax-r)/(tmax-tm) )
Ma=Mtmax - log(tmax/tm)*B/(tmax-tm)
#age_up=(tmax*12)-INP$tr +1

  if(INP$tr==0) {
   ages[1]=r 
    }    
    for(a in c(1:(length(ages)-1))) {
    loca_M[a]= Ma+B/a
    }
loca_M[length(ages)] =  loca_M[length(ages)-1] 

return(as.numeric(loca_M))
# }
}

#MortPB=ProdbiomUnSol(Linf,k,t0,tmax,Mtmax,l50,a,b,ages)

