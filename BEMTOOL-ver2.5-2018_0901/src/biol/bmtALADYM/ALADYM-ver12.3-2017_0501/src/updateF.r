# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




updateF<-function(Fm,loca_Nsimulation2){
gears = ncol(Fm)-2
years = length(unique(Fm$Year))
ages =  length(unique(Fm$Age))
Fbygear = data.frame(matrix(nrow = ((loca_Nsimulation2-1)*ages/12),ncol=gears))
  for (i in 1:gears){
    matrixF = data.frame(matrix(nrow=years,ncol=ages))
    medie =  data.frame(matrix(nrow=0,ncol=ages))
    colnames(matrixF)=unique(Fm$Age)
    Fm_temp = data.frame(Fm[,2+i])    
    
    for (y in 1:years) {
    matrixF[y,] = Fm_temp[((y-1)*ages+1):(y*ages),]     
         # matrice per 1 attrezzo per anni (righe) - età (colonne)
    }
    
    for (a in 1:ages)  {
    medie[1,a] = mean(matrixF[(years-INP$Average_forecast+1):years,a])   
    } 
    
    medie_temp=medie[1,]
    
    for (n in 2: ((loca_Nsimulation2-1)/12) ){
    medie=cbind(medie_temp,medie)
    } 
   for (r in 1:nrow(Fbygear)){     
   Fbygear[r,i] <- data.frame(medie[1,r])
   }
  } 
      
  A = data.frame(rep(unique(Fm$Age),(loca_Nsimulation2-1)/12)  )

  Y = data.frame(rep(1,ages))
  for (z in 2:((loca_Nsimulation2-1)/12)){ 
  Y_temp = data.frame(rep(z,ages))
  colnames(Y_temp)=colnames(Y)
  Y = rbind(Y,Y_temp)
  }
  #Y = data.frame(Y,ncol=1)    
  AY=cbind(A,Y)
  AY[,2] <- years_forecast[as.numeric(as.character(AY[,2])) ]
  Fbygear = data.frame(cbind(AY,Fbygear))
  colnames(Fbygear) = colnames(Fm)

return (Fbygear)
}
