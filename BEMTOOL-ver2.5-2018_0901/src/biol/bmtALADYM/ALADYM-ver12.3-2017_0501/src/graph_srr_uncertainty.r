# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


jpeg("Graphs/Uncertainty on SRR.jpg",res=300,unit="cm",height=21,width=30)
SSB_toplot = seq(0, 2*max(read.table(POPULATION_table,header=TRUE,sep=";")$SSB_exploited_pop),length.out=100)
par1=quantile(INP$SRR_CI_a_b_grid[,1],c(0.05,0.95,0.5))
par2=quantile(INP$SRR_CI_a_b_grid[,2],c(0.05,0.95,0.5))
if (!is.na(INP$SRR_CI_a_b_grid[1,3])){
par3=quantile(INP$SRR_CI_a_b_grid[,3],c(0.05,0.95,0.5))
} else {
par3=rep(NA,3)
}
Rec_toplot= RFSS(INP$S_unit, SSB_toplot, INP$FRLt_fore, par1[3], par2[3], par3[3], INP$Recruits)
Rec_toplot_up = RFSS(INP$S_unit, SSB_toplot, INP$FRLt_fore, par1[2], par2[2], par3[2], INP$Recruits)
Rec_toplot_low = RFSS(INP$S_unit, SSB_toplot, INP$FRLt_fore, par1[1], par2[1], par3[1], INP$Recruits)

plot(SSB_toplot, Rec_toplot, main="Uncertainty on SRR",xlab="SSB ",ylab="Recruitment (thousands)",type="l",col="blue")

lines(SSB_toplot, Rec_toplot_up,col="blue",lwd=2,lty=2)
lines(SSB_toplot, Rec_toplot_low,col="blue",lwd=2,lty=2)
dev.off()





