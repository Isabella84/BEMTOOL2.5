# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# INPUT: 
# COMPLETARE!!!

sd_tolerance_k_Linf <<- 0.07

obj_k = function(k) {
length_new=vBF(Linf,k,params$t0,ages)
LS = sum((lengths_or-length_new)^2)
return(LS)
} 

obj_Linf = function(Linf) {
length_new=vBF(Linf,k,params$t0,ages)
LS = sum((lengths_or-length_new)^2)
return(LS)
}

vBF = function(Linf,k,t0,ages){
Lt=Linf*(1-exp(-k*(ages-t0)))
return(Lt)
}

ChenWat <- function (loca_rFGrowth_K,loca_rFGrowth_t0,loca_Age)  {
if (FALSE) {
loca_rFGrowth_K = INP$VB_gridF[i,2]
loca_rFGrowth_t0 = INP$VB_gridF[i,3]
loca_Age =  BAS$FAge
}
loca_M = data.frame(matrix(0,ncol=length(loca_Age),nrow=1))
#if (INP$FOPT_M_TYPE == 2) {    
    loca_tM <- -(log(abs(1 - exp(loca_rFGrowth_K * loca_rFGrowth_t0))) / loca_rFGrowth_K) + loca_rFGrowth_t0;
    loca_A0 <- 1 - exp(-loca_rFGrowth_K * (loca_tM - loca_rFGrowth_t0));
    loca_A1 <- loca_rFGrowth_K * exp(-loca_rFGrowth_K * (loca_tM - loca_rFGrowth_t0));
    loca_A2 <- -loca_rFGrowth_K^2 * exp(-loca_rFGrowth_K * (loca_tM - loca_rFGrowth_t0)) / 2;
    for(loca_i in (1:length(loca_Age))) {
      if(loca_Age[loca_i] < loca_tM) {
        loca_M[loca_i] <- loca_rFGrowth_K / (1 - exp(-loca_rFGrowth_K * (loca_Age[loca_i] - loca_rFGrowth_t0)));
      } else {
        loca_M[loca_i] <- loca_rFGrowth_K / (loca_A0 + loca_A1 * (loca_Age[loca_i] - loca_tM) + loca_A2 * (loca_Age[loca_i] - loca_tM)^2);
      }
    }
return(loca_M)
# }
}



if (FALSE) {
Unc_growth="Y"
Linf<- log(INP$FGrowth_Linf_max) # 1450
#Linf_M<- INP$MGrowth_Linf_max  # 1450
SD_Linf=log( 5 )#INP$FGrowth_Linf_min
#SD_LinfM= INP$MGrowth_Linf_min

K<- log(INP$FGrowth_K_max) # 1450
#K_M<- INP$MGrowth_K_max  # 1450
SD_K= 0.12 #INP$FGrowth_K_min
#SD_KM= INP$MGrowth_K_min

t0<- INP$FGrowth_t0_max # 1450
lengths =BAS$FLength
    loca_GRID = ext_vector_growth_F
ages =BAS$FAge 
params=data.frame(Linf=Linf, K=K, K=K)
SDs=data.frame(SD_Linf= SD_Linf, SD_K=SD_K)
Free="K" # or "K"
APPROACH="D" # or "E"
DIST= "Lognormal"
nruns=500
path=NA # "C:\\ALADYM-ver10.2.1-2016\\griglia_usata_APPROCCIO2_HOCKEYSTICK_DPS19.csv"
}

# GRID = get_grid (APPROACH,DIST,Free,params,SDs,nruns,path) 
# write.table(GRID,"grid_growth.csv",sep=";",row.names=F)


get_grid_growth <- function(APPROACH,DIST,Free,params,SDs,nruns,ages,lengths_or, loca_GRID) {

if (Free == "Linf") {
k_or = params$K   
GRID=data.frame(Linf=rep(params$Linf,nruns),K=rep(k_or,nruns))
    if (APPROACH=="D") { 
            if (DIST == "Normal"){
            for (i in 1:nruns){
            Linf <<- rnorm(1,mean=params$Linf,sd=SDs$SD_Linf)
            if (Linf<0){
            nruns=nruns+1
            } else {
            k_new = optim(k_or,obj_k,method="Brent",lower = (k_or-(k_or*sd_tolerance_k_Linf)), upper = (k_or+(k_or*sd_tolerance_k_Linf)))$par
            #Linf_new = optim(Linf_or,obj_Linf,method="Brent",lower = (Linf_or-SD_Linf), upper = (Linf_or+SD_Linf))$par
            GRID[i,1]=Linf
            GRID[i,2]=k_new
            }
            }
            } else if (DIST == "Lognormal") {
            #k_or = exp(k_or)   
            for (i in 1:nruns){
            Linf <<- rlnorm(1,params$Linf,SDs$SD_Linf)
            k_new = optim(k_or,obj_k,method="Brent",lower = (k_or-(k_or*sd_tolerance_k_Linf)), upper = (k_or+(k_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf
            GRID[i,2]=k_new
            }            
            } else if (DIST == "Uniform") {
            for (i in 1:nruns){
            Linf <<- runif(1,min=(params[,1]),max=(SDs[,1]))
            k_new = optim(k_or,obj_k,method="Brent",lower = (k_or-(k_or*sd_tolerance_k_Linf)), upper = (k_or+(k_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf
            GRID[i,2]=k_new
            } 
            
            }
  colnames(GRID)= c("Linf","K") 
  
         
  } else if (APPROACH=="E"){
        for (i in 1:nruns) {
            Linf <<- loca_GRID[i]
            k_new = optim(k_or,obj_k,method="Brent",lower = (k_or-(k_or*sd_tolerance_k_Linf)), upper = (k_or+(k_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf
            GRID[i,2]=k_new
            }
       colnames(GRID)= c("Linf","K")       
  }
} else {        # Free = k
Linf_or = params$Linf
GRID=data.frame(Linf=rep(Linf_or,nruns),K=rep(params$K,nruns))
if (APPROACH=="D") {
            if (DIST == "Normal"){
            for (i in 1:nruns){ 
            k <<- rnorm(1,params$K,SDs$SD_K)  
            Linf_new = optim(Linf_or,obj_Linf,method="Brent",lower = (Linf_or-(Linf_or*sd_tolerance_k_Linf)), upper = (Linf_or+(Linf_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf_new
            GRID[i,2]=k
            }

            } else if (DIST == "Lognormal") {
            #Linf_or = exp(Linf_or)
            for (i in 1:nruns){
            k <<- rlnorm(1,params$K,SDs$SD_K)
            Linf_new = optim(Linf_or,obj_Linf,method="Brent",lower = (Linf_or-(Linf_or*sd_tolerance_k_Linf)), upper = (Linf_or+(Linf_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf_new
            GRID[i,2]=k
            }            
            } else if (DIST == "Uniform") {
            for (i in 1:nruns){
            k <<- runif(1,min=(params[,2]),max=(SDs[,2]))
            Linf_new = optim(Linf_or,obj_Linf,method="Brent",lower = (Linf_or-(Linf_or*sd_tolerance_k_Linf)), upper = (Linf_or+(Linf_or*sd_tolerance_k_Linf)),control=list(trace=20))$par                     
            GRID[i,1]=Linf_new
            GRID[i,2]=k
            } 
            
            }
  colnames(GRID)= c("Linf","K")        
  
  
  
  } else if (APPROACH=="E") {

            for (i in 1:nruns) { 
            k <<- loca_GRID[i]
            Linf_new = optim(Linf_or,obj_Linf,method="Brent",lower = (Linf_or-(Linf_or*sd_tolerance_k_Linf)), upper = (Linf_or+(Linf_or*sd_tolerance_k_Linf)))$par
            GRID[i,1]=Linf_new
            GRID[i,2]=k
            }

  colnames(GRID)= c("Linf","K")        

  }

}
return(GRID)
}
