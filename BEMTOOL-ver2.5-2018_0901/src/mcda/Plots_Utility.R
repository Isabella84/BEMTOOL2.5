# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



###################################
### Plots of utility functions
###################################
source(paste(getwd(), "/src/mcda/Utility_parameters.R", sep=""))
source(paste(getwd(), "/src/mcda/Functions.R", sep=""))

SSB0_plot=1000
SSBMSY_plot=500
Fmax_plot=3
FMSY_plot=1
Ymax_plot=1000
MEY_plot=1000
#CW_plot=3000
MNW_plot=1000
CE_plot=1000

### biological

jpeg(file='Utility_SSB.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(seq(0,SSB0_plot),sapply(seq(0,SSB0_plot),USSB,SSB0=SSB0_plot,SSBMSY=SSBMSY_plot),
xlab='SSB',ylab='Utility',xaxt='n',yaxt='n',type='l',ylim=c(0,1),xlim=c(0,SSB0_plot),bty='n')
axis(1,pos=0,at=c(0,0.2*SSB0_plot,SSBMSY_plot,SSB0_plot),c('0','20%SSB',expression(paste(SSB[MSY])),expression(paste(SSB[0]))))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
title('Utility function for spawning stock biomass',cex.main=0.9)
segments(0.2*SSB0_plot,0,0.2*SSB0_plot,u_ssb_0.2,col='red',lty=2)
segments(0.2*SSB0_plot,u_ssb_0.2,0,u_ssb_0.2,col='red',lty=2)
segments(SSBMSY_plot,0,SSBMSY_plot,u_ssb_msy,col='blue',lty=2)
segments(SSBMSY_plot,u_ssb_msy,0,u_ssb_msy,col='blue',lty=2)
dev.off()

jpeg(file='Utility_F.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(seq(0,Fmax_plot,by=0.01),sapply(seq(0,Fmax_plot,by=0.01),UF,FMSY=FMSY_plot),xlab='F',ylab='Utility',xaxt='n',type='l',yaxt='n',
bty='n',ylim=c(0,1), xlim=c(0,Fmax_plot))
axis(1,pos=0,at=c(0,FMSY_plot,2*FMSY_plot,Fmax_plot),c('0','Fmsy','2*Fmsy',''))
title('Utility function for fishing mortality',cex.main=0.9)
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
segments(FMSY_plot,0,FMSY_plot,u_f_msy,col='blue',lty=2)
segments(FMSY_plot,u_f_msy,0,u_f_msy,col='blue',lty=2)
segments(2*FMSY_plot,0,2*FMSY_plot,u_f_2msy,col='red',lty=2)
segments(2*FMSY_plot,u_f_2msy,0,u_f_2msy,col='red',lty=2)
dev.off()

jpeg(file='Utility_Y.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(seq(0,Ymax_plot),sapply(seq(0,Ymax_plot),UY,MSY=Ymax_plot,u_y_msy=u_y_msy,u_y_0.5msy=u_y_0.5msy),xlab='Y',ylab='Utility',xaxt='n',type='l',yaxt='n',bty='n',xlim=c(0,Ymax_plot),ylim=c(0,1))
axis(1,pos=0,at=c(0,Ymax_plot/2,Ymax_plot),c('0','0.5 MSY','MSY'))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
title('Utility function for yield',cex.main=0.9)
segments(Ymax_plot,0,Ymax_plot,u_y_msy,col='blue',lty=2)
segments(Ymax_plot,u_y_msy,0,u_y_msy,col='blue',lty=2)
segments(Ymax_plot/2,0,Ymax_plot/2,u_y_0.5msy,col='red',lty=2)
segments(Ymax_plot/2,u_y_0.5msy,0,u_y_0.5msy,col='red',lty=2)
dev.off()

jpeg(file='Utility_D.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(UD,from=0,to=1,xlab='Discard rate',ylab='Utility',xaxt='n',yaxt='n',bty='n')
axis(1,pos=0,at=c(0,0.25,0.5,1),c('0','0.25','0.5','1'))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
title('Utility function for discard',cex.main=0.9)
segments(0.25,0,0.25,u_d_0.25,col='blue',lty=2)
segments(0.25,u_d_0.25,0,u_d_0.25,col='blue',lty=2)
segments(0.5,0,0.5,u_d_0.5,col='red',lty=2)
segments(0.5,u_d_0.5,0,u_d_0.5,col='red',lty=2)
dev.off()



### socioeconomic

jpeg(file='Utility_RBER.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(URBER,from=0,to=4,ylab='Utility',xaxt='n',xlab='RBER',yaxt='n',bty='n')
title('Utility function for Revenues/BER',cex.main=0.9)
axis(1,pos=0,at=c(0,1,1.5,4),c('0','1','1.5',''))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
segments(1,0,1,URBER(1),col='blue',lty=2)
segments(1,URBER(1),0,URBER(1),col='blue',lty=2)
segments(1.5,0,1.5,URBER(1.5),col='red',lty=2)
segments(1.5,URBER(1.5),0,URBER(1.5),col='red',lty=2)
dev.off()


jpeg(file='Utility_Profits.jpg',width=21, height=21, bg="white", units="cm",res=200)  
string=ifelse(GVA_or_ROI_or_PROFITS=='GVA','GVA',ifelse(GVA_or_ROI_or_PROFITS=='ROI','ROI','PROFITS'))
string_title=paste('Utility function for',string)
plot(seq(0,MEY_plot),sapply(seq(0,MEY_plot),UGVA,MEY=MEY_plot,u_gva_mey=u_gva_mey,u_gva_0.5mey=u_gva_0.5mey),type='l',xlab=string,ylab='Utility',xaxt='n',yaxt='n',bty='n',ylim=c(0,1),xlim=c(0,MEY_plot))
title(string_title,cex.main=0.9)
axis(1,pos=0,at=c(0,MEY_plot/2,MEY_plot),c('0','0.5MEY','MEY'))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
segments(MEY_plot,0,MEY_plot,u_gva_mey,col='blue',lty=2)
segments(MEY_plot,u_gva_mey,0,u_gva_mey,col='blue',lty=2)
segments(MEY_plot/2,0,MEY_plot/2,u_gva_0.5mey,col='red',lty=2)
segments(MEY_plot/2,u_gva_0.5mey,0,u_gva_0.5mey,col='red',lty=2)
dev.off()

jpeg(file='Utility_WAGE.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(seq(0,7*MNW_plot),sapply(seq(0,7*MNW_plot),UWAGE,MNW=MNW_plot),type='l',xlab='WAGE',ylab='Utility',xaxt='n',yaxt='n',bty='n',
ylim=c(0,1),xlim=c(0,7*MNW_plot))
title('Utility function for Wage',cex.main=0.9)
axis(1,pos=0,at=c(0,MNW_plot,10*MNW_plot),c('0','MNW',''))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
segments(MNW_plot,0,MNW_plot,u_wage_mnw,col='blue',lty=2)
segments(MNW_plot,u_wage_mnw,0,u_wage_mnw,col='blue',lty=2)
dev.off()

jpeg(file='Utility_EMPL.jpg',width=21, height=21, bg="white", units="cm",res=200)  
plot(seq(0,2*CE_plot),sapply(seq(0,2*CE_plot),UEMPL,CE=CE_plot),type='l',xlab='EMPL',ylab='Utility',xaxt='n',yaxt='n',bty='n',ylim=c(0,1),xlim=c(0,2*CE_plot))
title('Utility function for Employment',cex.main=0.9)
axis(1,pos=0,at=c(0,CE_plot/2,CE_plot,2*CE_plot),c('0','CE/2','CE',''))
axis(2,pos=0,at=seq(0,1,by=0.2),labels=seq(0,1,by=0.2))
segments(CE_plot,0,CE_plot,u_empl_ce,col='blue',lty=2)
segments(CE_plot,u_empl_ce,0,u_empl_ce,col='blue',lty=2)
segments(CE_plot/2,0,CE_plot/2,u_empl_0.5ce,col='red',lty=2)
segments(CE_plot/2,u_empl_0.5ce,0,u_empl_0.5ce,col='red',lty=2)
dev.off()


