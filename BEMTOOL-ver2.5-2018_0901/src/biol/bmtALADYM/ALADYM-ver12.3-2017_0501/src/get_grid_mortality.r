# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


#INPUT
# METHOD: Chen&Watanabe, o ProdBiom, o Gislason
# Par è un valore e c'è solo per Prodbiom 

get_grid_mortality <- function(METHOD,Par,VB_CI,lengths,ages,l50,tmax,a,b) { # (INP$FOPT_M_TYPE,INP$FMtmax,INP$VB_gridF,BAS$FLength,BAS$FAge)#INP$MWLa
if (FALSE){
METHOD= INP$FOPT_M_TYPE
Par=INP$FMtmax
VB_CI=INP$VB_gridF
lengths=BAS$FLength
ages=BAS$FAge
l50=l50F
tmax=tmaxF
a=INP$FWLa
b=INP$FWLb
}
mort_CI=data.frame(matrix(0,nrow=length(lengths),ncol=INP$nruns))
#mort_CI_F=data.frame(matrix(0,nrow=length(BAS$FM),ncol=INP$nruns))

#VB_CI=read.table("grid_growthF.csv",sep=";",header=T)
#VB_CI_M=read.table("grid_growthM.csv",sep=";",header=T)

if(METHOD==2) { # Chen&Watanabe
for (i in (1:INP$nruns)){
mort_CI[,i]=ChenWat(VB_CI[i,2],VB_CI[i,3],ages) 
} 
} else if(METHOD==4) { #Prodbiom
#l50=mean(c(INP$MML50p_min, INP$MML50p_max))
#tmax=INP$MGrowth_tend - 1
for (i in (1:INP$nruns)){
#Mtmax=0.6                                        Linf,k,t0,tmax,Mtmax,l50,a,b,ages
mort_CI[,i]=ProdbiomUnSol(VB_CI[i,1],VB_CI[i,2],VB_CI[i,3],tmax,Par,l50,a,b,ages) 
}
} else if (METHOD==5) { #Gislason
for (i in (1:INP$nruns)){
mort_CI[,i]=Gislason(VB_CI[i,1],VB_CI[i,2],lengths)
}
}
return(mort_CI)
}
