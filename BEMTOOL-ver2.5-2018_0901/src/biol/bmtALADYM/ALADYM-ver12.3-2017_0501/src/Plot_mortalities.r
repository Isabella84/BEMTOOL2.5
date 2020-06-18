# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



Plot_mortalities <- function() {
 #-----------------------immagini:---------------------------------
# Mortality estimations.jpg
# Mortality estimations_F.jpg
#------------------------------------------------------------------------------
  loca_xa <- 1:(GLO$L_number / INP$Time_slice)
  gears= as.character(t(FLEETSEGMENTS_names))  
  
if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}
  
  mortality = read.table(MORTALITY_table,header=TRUE,sep=";")

if (nb_gears!=1){
colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))
}  else {
colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
}
   
   if (RUN_CI_FORE) {
mortality_1 = read.table(paste(MORTALITY_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
mortality_2 = mortality_1
mortality_4 = mortality_1
mortality_5 = mortality_1

mortality_1 <- mortality_1[mortality_1$percentile == 0.05, 1:(ncol(mortality_1)-1)]
mortality_5 <- mortality_5[mortality_5$percentile == 0.95, 1:(ncol(mortality_5)-1)]
mortality_2 <- mortality_2[mortality_2$percentile == 0.25, 1:(ncol(mortality_2)-1)]
mortality_4 <- mortality_4[mortality_4$percentile == 0.75, 1:(ncol(mortality_4)-1)]

if (nb_gears!=1){

colnames (mortality_1) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))
colnames (mortality_2) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))
 colnames (mortality_4) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))
colnames (mortality_5) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))


}  else {

colnames (mortality_1) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
colnames (mortality_2) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
colnames (mortality_4) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
colnames (mortality_5) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")

}
	 }
   

 #---------------------------salvataggio immagini -------------------------------
# Mortality estimations.jpg
# Mortality estimations_F.jpg
#------------------------------------------------------------------------------

  jpeg(file=MORTALITIES_Z_graph, width=30, height=16, bg="white", units="cm", res=200)
 par(mfrow=c(1,2), oma = c(0, 0, 3, 0))
#  plot(0,0,type="n",main="",axes=FALSE,xlab="",ylab="")
#   mtext(paste("ALADYM  ", GLO$ThisIsVersion),cex=0.7,side=4,outer=FALSE)

if (!RUN_CI_FORE) { 
	Ymin = min(mortality[,colnames(mortality)== "Annual_Z_estimated"], na.rm=T)
	Ymax =  max(mortality[,colnames(mortality)== "Annual_Z_estimated"], na.rm=T)
	} else {
Ymin = min(mortality[,colnames(mortality)== "Annual_Z_estimated"], mortality_1[,colnames(mortality_1)== "Annual_Z_estimated"], mortality_5[,colnames(mortality_5)== "Annual_Z_estimated"],  mortality_2[,colnames(mortality_2)== "Annual_Z_estimated"],  mortality_4[,colnames(mortality_4)== "Annual_Z_estimated"], na.rm=T)
Ymax = max(mortality[,colnames(mortality)== "Annual_Z_estimated"], mortality_1[,colnames(mortality_1)== "Annual_Z_estimated"], mortality_5[,colnames(mortality_5)== "Annual_Z_estimated"],  mortality_2[,colnames(mortality_2)== "Annual_Z_estimated"],  mortality_4[,colnames(mortality_4)== "Annual_Z_estimated"], na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)
	 
	 #grafico 15 : MZ annual calculated
  plot(loca_xa, mortality[,colnames(mortality)== "Annual_Z_estimated"], type="l",  main="Annual Z" , xlab="Time_slice [year]", ylab="Z [year-1]",lwd = 2,col="blue", ylim=Ylim)
    points(loca_xa, mortality[,colnames(mortality)== "Annual_Z_estimated"], pch=20, col="blue")
   mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
   
     if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    }  
    
  if (RUN_CI_FORE) {
  
   polygon(c(loca_xa, rev(loca_xa)), c( mortality_1[,colnames(mortality_1)== "Annual_Z_estimated"], rev(mortality_2[,colnames(mortality_2)== "Annual_Z_estimated"])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_2[,colnames(mortality_2)== "Annual_Z_estimated"], rev(mortality[,colnames(mortality)== "Annual_Z_estimated"])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality[,colnames(mortality)== "Annual_Z_estimated"], rev(mortality_4[,colnames(mortality_4)== "Annual_Z_estimated"])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_4[,colnames(mortality_4)== "Annual_Z_estimated"], rev(mortality_5[,colnames(mortality_5)== "Annual_Z_estimated"])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, mortality_1[,colnames(mortality_1)== "Annual_Z_estimated"],  type="l", lty=2) 
    	lines(loca_xa, mortality_2[,colnames(mortality_2)== "Annual_Z_estimated"] ,  type="l", lty=3) 
    	lines(loca_xa, mortality_4[,colnames(mortality_4)== "Annual_Z_estimated"] ,  type="l", lty=3) 
       lines(loca_xa, mortality_5[,colnames(mortality_5)== "Annual_Z_estimated"],  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )
  
#    lines(loca_xa, mortality_inf[,colnames(mortality_inf)== "Annual_Z_estimated"]  ,  type="l", lty=2) 
#	  lines(loca_xa, mortality_sup[,colnames(mortality_sup)== "Annual_Z_estimated"],  type="l", lty=3) 
#	  legend ("topright", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("blue","black","black"), bty="n" )
	}      
 

 
#  dev.off()

 
  #salvataggio immagine
#  jpeg(file=MORTALITIES_F_graph, width=21, height=21, bg="white", units="cm", res=200)
#  par(mfrow=c(2,2))
#  
	
	# grafico 18 : F annual calculated males
 
 if (!RUN_CI_FORE) { 
		Ymin = min(mortality[,colnames(mortality)== "Annual_F_estimated"], na.rm=T)
	Ymax = max(mortality[,colnames(mortality)== "Annual_F_estimated"], na.rm=T)
	
	} else {
Ymin = min(mortality[,colnames(mortality)== "Annual_F_estimated"], mortality_1[,colnames(mortality_1)== "Annual_F_estimated"], mortality_5[,colnames(mortality_5)== "Annual_F_estimated"], mortality_2[,colnames(mortality_2)== "Annual_F_estimated"], mortality_4[,colnames(mortality_4)== "Annual_F_estimated"], na.rm=T)
Ymax = max(mortality[,colnames(mortality)== "Annual_F_estimated"], mortality_1[,colnames(mortality_1)== "Annual_F_estimated"], mortality_5[,colnames(mortality_5)== "Annual_F_estimated"], mortality_2[,colnames(mortality_2)== "Annual_F_estimated"], mortality_4[,colnames(mortality_4)== "Annual_F_estimated"],  na.rm=T)
	}
	
			Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)
  
  plot(loca_xa,mortality[,colnames(mortality)=="Annual_F_estimated"] ,type="l", main="Annual F" , xlab="Time_slice [year]", ylab="Male F [year-1]",lwd = 2, col="blue", ylim=Ylim)
  points(loca_xa, mortality[,colnames(mortality)== "Annual_F_estimated"], pch=20, col="blue")
   mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
   
      if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
	 
 if (RUN_CI_FORE) {
 
    polygon(c(loca_xa, rev(loca_xa)), c( mortality_1[,colnames(mortality_1)== "Annual_F_estimated"], rev(mortality_2[,colnames(mortality_2)== "Annual_F_estimated"])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_2[,colnames(mortality_2)== "Annual_F_estimated"], rev(mortality[,colnames(mortality)== "Annual_F_estimated"])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality[,colnames(mortality)== "Annual_F_estimated"], rev(mortality_4[,colnames(mortality_4)== "Annual_F_estimated"])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_4[,colnames(mortality_4)== "Annual_F_estimated"], rev(mortality_5[,colnames(mortality_5)== "Annual_F_estimated"])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, mortality_1[,colnames(mortality_1)== "Annual_F_estimated"],  type="l", lty=2) 
    	lines(loca_xa, mortality_2[,colnames(mortality_2)== "Annual_F_estimated"] ,  type="l", lty=3) 
    	lines(loca_xa, mortality_4[,colnames(mortality_4)== "Annual_F_estimated"] ,  type="l", lty=3) 
       lines(loca_xa, mortality_5[,colnames(mortality_5)== "Annual_F_estimated"],  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )
 
   # lines(loca_xa, mortality_inf[,colnames(mortality_inf)== "Annual_F_estimated"]  ,  type="l", lty=2) 
#	  lines(loca_xa, mortality_sup[,colnames(mortality_sup)== "Annual_F_estimated"],  type="l", lty=3) 
#	  legend ("topright", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("blue","black","black"), bty="n" )
	}   	 

         mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()
 
  
  
}
