# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


jpeg(paste(graphsDIR_input, "/Uncertainty on VBF_males.jpg", sep=""),res=300,unit="cm",height=21,width=30)
Linf_CI_M = quantile(INP$VB_gridM[,1],c(0.05,0.95))
k_CI_M = quantile(INP$VB_gridM[,2],c(0.05,0.95))
t0_M =INP$VB_gridM[1,3]
plot(BAS$MAge,BAS$MLength,type="l",lwd=2,ylim=c(0,1.1*max(BAS$MLength)),main="Uncertainty on VBF (males)",col="blue",xlab="Age(years)",ylab="Length (mm)")
lines(BAS$MAge,vBF(Linf_CI_M[1],k_CI_M[1],t0_M,BAS$MAge),lty=2,lwd=2,col="blue")
lines(BAS$MAge,vBF(Linf_CI_M[2],k_CI_M[2],t0_M,BAS$MAge),lty=2,lwd=2,col="blue")
dev.off()

jpeg(paste(graphsDIR_input, "/Uncertainty on VBF_females.jpg", sep=""),res=300,unit="cm",height=21,width=30)
Linf_CI_F = quantile(INP$VB_gridF[,1],c(0.05,0.95))
k_CI_F = quantile(INP$VB_gridF[,2],c(0.05,0.95))
t0_F =INP$VB_gridF[1,3]
plot(BAS$FAge,BAS$FLength,type="l",lwd=2,ylim=c(0,1.1*max(BAS$FLength)),main="Uncertainty on VBF (females)",col="blue",xlab="Age(years)",ylab="Length (mm)")
lines(BAS$FAge,vBF(Linf_CI_F[1],k_CI_F[1],t0_F,BAS$FAge),lty=2,lwd=2,col="blue")
lines(BAS$FAge,vBF(Linf_CI_F[2],k_CI_F[2],t0_F,BAS$FAge),lty=2,lwd=2,col="blue")
dev.off()

