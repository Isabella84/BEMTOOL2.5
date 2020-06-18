# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




PlotRPs <- function() {

gears= as.character(t(FLEETSEGMENTS_names))
#loca_xa <- 1:(GLO$L_number / INP$Time_slice)
YpR_data = read.table(file=YpR_table,sep=";",header=TRUE)

if (nb_gears==1){
colnames(YpR_data) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR")
} else {
colnames(YpR_data) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR",paste("Annual_F_estimated_",gears,sep=""),paste("Yield_",gears,sep=""),paste("Landing_",gears,sep=""))
}

Ymax = max(YpR_data$Total_Yield)
Ymin = min(YpR_data$Total_Yield)
Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/5)

jpeg(file=RPs_graph, width=21, height=21, bg="white", units="cm",res=200)
      
      plot(YpR_data[,1], YpR_data$Total_Yield, type="p", xlab="F factor", ylab="[t]", lwd = 2,col="black",ylim=Ylim)
      
      if (INP$FRLt==4){
      title(paste("Yield-per-recruit [t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      } else {
      title(paste("Yield [t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      }
      if (nb_gears!=1)  {
      for (gear in 1:nb_gears) {
      points(YpR_data[,1], YpR_data[,colnames(YpR_data)==paste("Yield_",gears,sep="")[gear]] , lwd = 2,col=10+gear,lty=4)
      }
      }
      Rp= read.table(file=REFERENCEPOINTS_table,sep=";",header=T)
      colnames(Rp) = c("Reference_point","Annual_F_estimated","Total_Yield","SSB","Recruits") 
      
      factors= Rp$Annual_F_estimated/Rp$Annual_F_estimated[1]
      
      for (i in 1:nrow(Rp)) {
      #points(factors[i],Rp$Total_Yield[i],col=10+i,type="p",pch=17,cex=1.5)
      #a=c(factors[i],0)
      #b=c(factors[i],Rp$Total_Yield[i])
      abline(v=factors[i],col=1+i,lwd=2.5)
      }
      
      if (exists("pred_y")){
      lines(YpR_data[,1], pred_y)
      }
      #legend("topleft",paste(as.character(Rp$Reference_point)," = ",round(Rp$Annual_F_estimated,2)), pch=17,col =seq(11,(10+nrow(Rp)),1),cex = 1 ) 
      legend("topleft",paste(as.character(Rp$Reference_point)," = ",round(Rp$Annual_F_estimated,2)), pch=95, col=c(2:(1+nrow(Rp))), bty="n", cex=1)    
       
      mtext( GLO$ThisIsVersion,cex=1,side=4,outer=FALSE)    
dev.off()

if (INP$FRLt==4) {           # if no SR relationship
recruitment <- mean(INP$Recruits[(forecast-INP$Average_forecast*12+1):forecast])

#variables
x <<- YpR_data[,colnames(YpR_data) == "Annual_F_estimated"]
y <<- YpR_data$Total_Yield/recruitment
SSB <<- YpR_data$SSB_exploited_pop/recruitment 
biomass <<- YpR_data$Total_biomass_exploited_pop/recruitment
z_ <<- y *recruitment - 0.1 * YpR_data[1,colnames(YpR_data)=="Total_biomass_exploited_pop"] * x # sulla prima riga deve stare assolutamente F=0!!!
t_ <<- y*recruitment  - 0.2 * YpR_data[1,colnames(YpR_data)=="Total_biomass_exploited_pop"] * x # sulla prima riga deve stare assolutamente F=0!!!


jpeg(file=F01_graph, width=21, height=21, bg="white", units="cm",res=200)
      
      plot(YpR_data[,1], z_, type="p", main=paste("F01 curve - ", BMT_SPECIES[ALADYM_spe], sep=""), xlab="F factor", ylab="", lwd = 2,col="black")
      if (exists("pred_v")){
      lines(YpR_data[,1], pred_v,type="l",pch=17,cex=1.5)
      }
      
      mtext( GLO$ThisIsVersion,cex=1,side=4,outer=FALSE)    
dev.off()

jpeg(file=F02_graph, width=21, height=21, bg="white", units="cm",res=200)
      
      plot(YpR_data[,1], t_, type="p", main=paste("F02 curve - ", BMT_SPECIES[ALADYM_spe], sep=""), xlab="F factor", ylab="", lwd = 2,col="black")
      if (exists("pred_t")){
      lines(YpR_data[,1], pred_t,type="l",pch=17,cex=1.5)
      }
      
      mtext(GLO$ThisIsVersion,cex=1,side=4,outer=FALSE)    
dev.off()
}

jpeg(file=SSB_graph, width=21, height=21, bg="white", units="cm",res=200)
      
      plot(YpR_data[,1], SSB, type="p",  xlab="F factor", ylab="[t]", lwd = 2,col="black")
      if (INP$FRLt==4){
      title(paste("SSB-per-recruit curve [t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      } else {
      title(paste("SSB curve[t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      }
      if (exists("pred_ssb")){
      lines(YpR_data[,1], pred_ssb)
      }
      
      mtext( GLO$ThisIsVersion,cex=1,side=4,outer=FALSE)    
dev.off()


jpeg(file=B_graph, width=21, height=21, bg="white", units="cm",res=200)
      
      plot(YpR_data[,1], biomass, type="p", xlab="F factor", ylab="[t]", lwd = 2,col="black")
            if (INP$FRLt==4){
      title(paste("Biomass-per-recruit curve [t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      } else {
      title(paste("Biomass curve[t] - ", BMT_SPECIES[ALADYM_spe], sep=""))
      }
      if (exists("pred_b")){
      lines(YpR_data[,1], pred_b)
      }
      
      mtext(GLO$ThisIsVersion,cex=1,side=4,outer=FALSE)    
dev.off()

}
