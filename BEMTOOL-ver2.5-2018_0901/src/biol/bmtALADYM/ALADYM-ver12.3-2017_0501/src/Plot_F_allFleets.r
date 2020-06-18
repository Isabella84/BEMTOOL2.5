# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


#---------------------------- immagini:-----------------------------------------
# F by gear
#-------------------------------------------------------------------------------

Plot_F_allFleets<- function() {

gears= as.character(t(FLEETSEGMENTS_names))

mortality = read.table(MORTALITY_table,header=TRUE,sep=";")

if (nb_gears!=1) {
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
mortality_2 <- mortality_2[mortality_2$percentile == 0.25, 1:(ncol(mortality_2)-1)]
mortality_4 <- mortality_4[mortality_4$percentile == 0.75, 1:(ncol(mortality_4)-1)]
mortality_5 <- mortality_5[mortality_5$percentile == 0.95, 1:(ncol(mortality_5)-1)]


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


#Ymin = ifelse(nb_gears==1, min(mortality[,ncol(mortality)]),min(mortality[,(ncol(mortality)-2*nb_gears+3):ncol(mortality)]))
#Ymax = ifelse(nb_gears==1, max(mortality[,ncol(mortality)]),max(mortality[,(ncol(mortality)-2*nb_gears+3):ncol(mortality)]))



loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}



Fattrezzi= paste("Annual_F_estimated_",gears,sep="")

nb_graphs=nb_gears + 1
nb_sheets= ifelse(modulo(nb_graphs,8)==0, as.integer(nb_graphs/8),as.integer(nb_graphs/8)+1)


if (nb_gears==1) {
		mortality <- data.frame(mortality[,colnames(mortality) == "Annual_F_estimated"] )          
		colnames(mortality)="Annual_F_estimated"
} else {
		mortality <- mortality[,colnames(mortality) %in% c("Annual_F_estimated",paste("Annual_F_estimated_",gears,sep="")) ]     
}  


 for (i in 1:nb_sheets) {
 
  if (nb_sheets!= 1) {
nome_file <- paste(F_BYGEAR_graph_grouped,"_sheet", i,".jpg", sep="")
} else {
nome_file <- paste(F_BYGEAR_graph_grouped,".jpg", sep="")
}
 if (nb_gears <= 4) {
      jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
    par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
 } else {   
 jpeg(file=nome_file, width=35, height=21, bg="white", units="cm",res=200)   
    par(mfrow=c(2,4), oma = c(0, 0, 3, 0))
 }
 
     if ((8*i)> nb_graphs ){
        nb_loops = nb_graphs - 8*(i-1)
     }  else {
        nb_loops = 8*i
     }
          
          for (m in (8*i-7):(8*(i-1)+(nb_loops))) {
           if (m==1){
           
if (! RUN_CI_FORE) {          
Ymin = min(mortality$Annual_F_estimated, na.rm=T)             
Ymax = max(mortality$Annual_F_estimated, na.rm=T)

} else  {
Ymin = min(mortality$Annual_F_estimated, mortality_1$Annual_F_estimated, mortality_2$Annual_F_estimated, mortality_4$Annual_F_estimated, mortality_5$Annual_F_estimated, na.rm=T)             
Ymax =  max(mortality$Annual_F_estimated, mortality_1$Annual_F_estimated, mortality_2$Annual_F_estimated, mortality_4$Annual_F_estimated, mortality_5$Annual_F_estimated, na.rm=T)  
}

Ylim=c(Ymin,(Ymax+(Ymax-Ymin)/3) ) 
          
           plot(loca_xa, mortality$Annual_F_estimated,   type="l",  main="Total F", xlab="Time_slice [year]", ylab="F[year-1]", ylim=Ylim,lwd=2) 
           points(loca_xa, mortality$Annual_F_estimated,   type="p",  pch=20) 
           mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE) #  
    
    if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1,cex=0.55) 
    } 
           
           if (RUN_CI_FORE) {
           
polygon(c(loca_xa, rev(loca_xa)), c(mortality_1$Annual_F_estimated, rev(mortality_2$Annual_F_estimated)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_2$Annual_F_estimated, rev(mortality$Annual_F_estimated)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality$Annual_F_estimated, rev(mortality_4$Annual_F_estimated)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_4$Annual_F_estimated, rev(mortality_5$Annual_F_estimated)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, mortality_1$Annual_F_estimated,  type="l", lty=2) 
    	lines(loca_xa, mortality_2$Annual_F_estimated ,  type="l", lty=3) 
    	lines(loca_xa,mortality_4$Annual_F_estimated,  type="l", lty=3) 
       lines(loca_xa, mortality_5$Annual_F_estimated,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )
         
#    lines(loca_xa, mortality_inf$Annual_F_estimated,  type="l", lty=2) 
#	  lines(loca_xa, mortality_sup$Annual_F_estimated ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("black","black","black"), bty="n" )
              }
           
           } else { 
               if(nb_gears!=1) {
    
    if (! RUN_CI_FORE) {          
Ymin = min(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))], na.rm=T)             
Ymax = max(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))], na.rm=T)

} else  {
Ymin = min(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))], mortality_1[,which(colnames(mortality_1)==as.character(Fattrezzi[m-1]))], mortality_2[,which(colnames(mortality_2)==as.character(Fattrezzi[m-1]))], mortality_4[,which(colnames(mortality_4)==as.character(Fattrezzi[m-1]))], mortality_5[,which(colnames(mortality_5)==as.character(Fattrezzi[m-1]))], na.rm=T)             
Ymax =  max(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))], mortality_1[,which(colnames(mortality_1)==as.character(Fattrezzi[m-1]))], mortality_2[,which(colnames(mortality_2)==as.character(Fattrezzi[m-1]))], mortality_4[,which(colnames(mortality_4)==as.character(Fattrezzi[m-1]))], mortality_5[,which(colnames(mortality_5)==as.character(Fattrezzi[m-1]))], na.rm=T)  
}

Ylim=c(Ymin,(Ymax+(Ymax-Ymin)/3) )            
               
plot(loca_xa,mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))],type="l",   main=paste("F for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "F[year-1]",lwd = 2,col = m,ylim=Ylim)  
points(loca_xa,mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))],type="p",   pch=20, col = m) 
 mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
            
                if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
            
                if (RUN_CI_FORE) {
                
polygon(c(loca_xa, rev(loca_xa)), c( mortality_1[,which(colnames(mortality_1)==as.character(Fattrezzi[m-1]))], rev(mortality_2[,which(colnames(mortality_2)==as.character(Fattrezzi[m-1]))])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_2[,which(colnames(mortality_2)==as.character(Fattrezzi[m-1]))], rev(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality[,which(colnames(mortality)==as.character(Fattrezzi[m-1]))], rev(mortality_4[,which(colnames(mortality_4)==as.character(Fattrezzi[m-1]))])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_4[,which(colnames(mortality_4)==as.character(Fattrezzi[m-1]))], rev(mortality_5[,which(colnames(mortality_5)==as.character(Fattrezzi[m-1]))])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa,  mortality_1[,which(colnames(mortality_1)==as.character(Fattrezzi[m-1]))],  type="l", lty=2) 
    	lines(loca_xa,  mortality_2[,which(colnames(mortality_2)==as.character(Fattrezzi[m-1]))] ,  type="l", lty=3) 
    	lines(loca_xa,  mortality_4[,which(colnames(mortality_4)==as.character(Fattrezzi[m-1]))],  type="l", lty=3) 
       lines(loca_xa,  mortality_5[,which(colnames(mortality_5)==as.character(Fattrezzi[m-1]))],  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )            
                
#    lines(loca_xa, mortality_inf[,which(colnames(mortality_inf)==as.character(Fattrezzi[m-1]))],  type="l", lty=2) 
#	  lines(loca_xa, mortality_sup[,which(colnames(mortality_sup)==as.character(Fattrezzi[m-1]))] ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
               } else {
    plot(loca_xa,mortality$Annual_F_estimated,type="l",  main=paste("F for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "F[year-1]",lwd = 2,col = m,ylim=Ylim) 
    points(loca_xa,mortality$Annual_F_estimated,type="p",   pch=20, col=m) 
               mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
 
 if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
 
    if (RUN_CI_FORE) {
    
    polygon(c(loca_xa, rev(loca_xa)), c(mortality_1$Annual_F_estimated, rev(mortality_2$Annual_F_estimated)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_2$Annual_F_estimated, rev(mortality$Annual_F_estimated)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality$Annual_F_estimated, rev(mortality_4$Annual_F_estimated)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(mortality_4$Annual_F_estimated, rev(mortality_5$Annual_F_estimated)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, mortality_1$Annual_F_estimated,  type="l", lty=2) 
    	lines(loca_xa, mortality_2$Annual_F_estimated ,  type="l", lty=3) 
    	lines(loca_xa,mortality_4$Annual_F_estimated,  type="l", lty=3) 
       lines(loca_xa, mortality_5$Annual_F_estimated,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )
       
    
#    lines(loca_xa, mortality_inf[,ncol(mortality_inf)],  type="l", lty=2) 
#	  lines(loca_xa, mortality_sup[,ncol(mortality_sup)] ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
               }
           }  
       
       }
        mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)

dev.off() 
} 


}
