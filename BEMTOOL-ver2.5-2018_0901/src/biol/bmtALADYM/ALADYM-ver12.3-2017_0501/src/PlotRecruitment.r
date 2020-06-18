# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

PlotRecruitment <- function() {
# Comparison between Input recruitment and recruitment with noise             # AGGIUNTO

#if (INP$FRLt==4)      {

recruit_to_plot <- read.csv(RECRUITMENT_table, sep=";")

min_val = min( recruit_to_plot$Recruitment)
lim_val = c(min_val, max(recruit_to_plot$Recruitment, na.rm=T) + (max(recruit_to_plot$Recruitment, na.rm=T)-min_val)/2)
 
 if (RUN_CI_FORE) {
recruit_to_plot_inf= read.table(paste(RECRUITMENT_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
recruit_to_plot_inf_025 = recruit_to_plot_inf #read.table(paste(RECRUITMENT_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
recruit_to_plot_sup = recruit_to_plot_inf
recruit_to_plot_sup_075 = recruit_to_plot_inf

recruit_to_plot_inf <- recruit_to_plot_inf[recruit_to_plot_inf$percentile == 0.05, 1:(ncol(recruit_to_plot_inf)-1)]
recruit_to_plot_sup <- recruit_to_plot_sup[recruit_to_plot_sup$percentile == 0.95, 1:(ncol(recruit_to_plot_sup)-1)]

recruit_to_plot_inf_025 <- recruit_to_plot_inf_025[recruit_to_plot_inf_025$percentile == 0.25, 1:(ncol(recruit_to_plot_inf_025)-1)]
recruit_to_plot_sup_075 <- recruit_to_plot_sup_075[recruit_to_plot_sup_075$percentile == 0.75, 1:(ncol(recruit_to_plot_sup_075)-1)]

min_val = min( recruit_to_plot$Recruitment, recruit_to_plot_inf$Recruitment, recruit_to_plot_sup$Recruitment, recruit_to_plot_inf_025$Recruitment, recruit_to_plot_sup_075$Recruitment, na.rm=T)
lim_val = c(min_val, max(recruit_to_plot$Recruitment, recruit_to_plot_inf$Recruitment, recruit_to_plot_sup$Recruitment, recruit_to_plot_inf_025$Recruitment, recruit_to_plot_sup_075$Recruitment,  na.rm=T, na.rm=T) + (max(recruit_to_plot$Recruitment, recruit_to_plot_inf$Recruitment, recruit_to_plot_sup$Recruitment,recruit_to_plot_inf_025$Recruitment, recruit_to_plot_sup_075$Recruitment,  na.rm=T, na.rm=T)-min_val)/5)
 }
 

jpeg(file=RECRUITMENT_graph, width=15, height=15, bg="white", units="cm",res=200) 
 par(  mar=c(5, 5, 7, 5), xpd=T)     
#loca_xa <- 1:(GLO$L_number / INP$Time_slice)
# Recruitment = INP$Recruits # * INP$Fertility_Rate_esteso         # meanWequals(INP$Recruits, GLO$L_number + 1, INP$Time_slice)
plot(recruit_to_plot$Year, recruit_to_plot$Recruitment, main=paste("Recruitment - ",BMT_SPECIES[ALADYM_spe],sep=""),xlab="Years",ylab="thousands",type="b", pch=20, ylim=lim_val, col="blue")    # sono già in migliaia
 mtext( GLO$ThisIsVersion,cex=1,side=4,outer=FALSE) 
	  if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), lim_val, col="red", lty=2)
    text(rep(years[length(years)],2)+1, min_val, label="FORECAST", pos=3, cex=0.55) 
    } 
 if (RUN_CI_FORE) {
 
 polygon(c(as.numeric(as.character(recruit_to_plot$Year)), rev(as.numeric(as.character(recruit_to_plot$Year)))), c(recruit_to_plot_inf$Recruitment, rev(recruit_to_plot_inf_025$Recruitment)), col = alpha("black", 0.05), border = NA)
polygon(c(as.numeric(as.character(recruit_to_plot$Year)), rev(as.numeric(as.character(recruit_to_plot$Year)))), c(recruit_to_plot_inf_025$Recruitment, rev(recruit_to_plot$Recruitment)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(recruit_to_plot$Year)), rev(as.numeric(as.character(recruit_to_plot$Year)))), c(recruit_to_plot$Recruitment, rev(recruit_to_plot_sup_075$Recruitment)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(recruit_to_plot$Year)), rev(as.numeric(as.character(recruit_to_plot$Year)))), c(recruit_to_plot_sup_075$Recruitment, rev(recruit_to_plot_sup$Recruitment)), col = alpha("black", 0.05), border = NA)
  
 
    lines(as.numeric(as.character(recruit_to_plot$Year)), recruit_to_plot_inf$Recruitment ,  type="l", lty=2)
    lines(as.numeric(as.character(recruit_to_plot$Year)), recruit_to_plot_inf_025$Recruitment ,  type="l", lty=3) 
    lines(as.numeric(as.character(recruit_to_plot$Year)), recruit_to_plot_sup_075$Recruitment ,  type="l", lty=3)  
	  lines(as.numeric(as.character(recruit_to_plot$Year)), recruit_to_plot_sup$Recruitment,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,1,1,1),lty=c(2,2,1,3,3),legend=c("0.05", "0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )
 }
 
 
#Recruitment_with_noise = (SRO$FFPopulation[,1] + SRO$MFPopulation[,1])/1000
#lines(loca_xa,meanWequals(Recruitment_with_noise,GLO$L_number + 1, INP$Time_slice),main="Recruitment",xlab="Years",ylab="thousands", lwd=1,col="blue")    # sono già in migliaia
 #legend("top", c("Input Recruitment","Recruitment with noise"),col=c("black","blue"), lwd=c(3,1),  bty="n", inset=c(0,-0.15))         #,    , y.intersp=1.1

dev.off() 

#}

}