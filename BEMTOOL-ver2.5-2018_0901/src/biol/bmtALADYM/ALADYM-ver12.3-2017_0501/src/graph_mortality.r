# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


jpeg("Uncertainty on natural mortality_males.jpg",res=300,unit="cm",height=21,width=30)
k_CI_M = quantile(INP$VB_gridM[,2],c(0.05,0.95,0.5))
t0_M =INP$VB_gridM[1,3]
plot(BAS$MAge,ChenWat(k_CI_M[3],t0_M,BAS$MAge),main="Uncertainty on natural mortality (males)",xlab="ages(years)",ylab="M",ylim=c(0.17,1),type="l",col="blue")
#for (i in 1:nrow(Pairs)){
#lines(ages,ChenWatanabe(Pairs[i,2]),col=palette[i])
#}
#points(ages,ChenWatanabe(k_or),pch=16)
lines(BAS$MAge,ChenWat(k_CI_M[1],t0_M,BAS$MAge),lty=2,lwd=2,col="blue")
lines(BAS$MAge,ChenWat(k_CI_M[2],t0_M,BAS$MAge),lty=2,lwd=2,col="blue")
dev.off()





jpeg("graphs/Uncertainty on natural mortality_females.jpg",res=300,unit="cm",height=21,width=30)
k_CI_F = quantile(INP$VB_gridF[,2],c(0.05,0.95,0.5))
t0_F =INP$VB_gridF[1,3]
plot(BAS$FAge,ChenWat(k_CI_F[3],t0_F,BAS$FAge),main="Uncertainty on natural mortality (females)",xlab="ages(years)",ylab="M",ylim=c(0.17,1),type="l",col="blue")
#for (i in 1:nrow(Pairs)){
#lines(ages,ChenWatanabe(Pairs[i,2]),col=palette[i])
#}
#points(ages,ChenWatanabe(k_or),pch=16)
lines(BAS$FAge,ChenWat(k_CI_F[1],t0_F,BAS$FAge),lty=2,lwd=2,col="blue")
lines(BAS$FAge,ChenWat(k_CI_F[2],t0_F,BAS$FAge),lty=2,lwd=2,col="blue")
dev.off()

