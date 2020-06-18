# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#---------------------------- immagini:-----------------------------------------
# Yield by gear
# Landing by gear
# Discard by gear
#-------------------------------------------------------------------------------

Plot_yield_allFleets<- function() {

gears = as.character(t(FLEETSEGMENTS_names))
production = read.table(PRODUCTION_table,header=TRUE,sep=";")
   
if(nb_gears==1) {
colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation")
}  else {
colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio",paste("LandingObligation_",gears,sep="") )
}

                                                      
if (RUN_CI_FORE) {

production_inf <- read.table(paste(PRODUCTION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
production_sup <- production_inf 
production_inf_025 <- production_inf
production_sup_075 <- production_inf

production_inf <- production_inf[production_inf$percentile == 0.05, 1:(ncol(production_inf)-1)]
production_sup <- production_sup[production_sup$percentile == 0.95, 1:(ncol(production_sup)-1)]
production_inf_025 <- production_inf_025[production_inf_025$percentile == 0.25, 1:(ncol(production_inf_025)-1)]
production_sup_075 <- production_sup_075[production_sup_075$percentile == 0.75, 1:(ncol(production_sup_075)-1)]


  if(nb_gears==1) {
  
colnames (production_inf) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation")  
colnames (production_sup) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation") 
colnames (production_inf_025) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation")
colnames (production_sup_075) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation") 

}  else {

colnames (production_inf) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio", paste("LandingObligation_",gears,sep=""))
colnames (production_sup) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio", paste("LandingObligation_",gears,sep=""))
colnames (production_inf_025) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio", paste("LandingObligation_",gears,sep=""))
colnames (production_sup_075) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio", paste("LandingObligation_",gears,sep=""))

}

}



# loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}


 
#-------------------------------------------------------------------------------
# Total catch per attrezzo
#-------------------------------------------------------------------------------    

if (nb_gears==1) {
		production_yield <- data.frame(production[,colnames(production) == "Total_Yield"] )          
		colnames(production_yield)="Total_Yield"
} else {
		production_yield <- production[,colnames(production) %in% c("Total_Yield",paste("Yield_",gears,sep="")) ]     
}  

if (RUN_CI_FORE) {
if (nb_gears==1){
production_yield_1 <- data.frame(production_inf[,colnames(production_inf) == "Total_Yield"]  )          
colnames(production_yield_1)="Total_Yield"
production_yield_5 <- data.frame(production_sup[,colnames(production_sup) == "Total_Yield"]  )          
colnames(production_yield_5)="Total_Yield"

production_yield_2 <- data.frame(production_inf_025[,colnames(production_inf_025) == "Total_Yield"]  )          
colnames(production_yield_2)="Total_Yield"
production_yield_4 <- data.frame(production_sup_075[,colnames(production_sup_075) == "Total_Yield"]  )          
colnames(production_yield_4)="Total_Yield"

} else{
production_yield_1 <- production_inf[,colnames(production_inf) %in% c("Total_Yield",paste("Yield_",gears,sep="")) ] 
production_yield_5 <- production_sup[,colnames(production_sup) %in% c("Total_Yield",paste("Yield_",gears,sep="")) ] 

production_yield_2 <- production_inf_025[,colnames(production_inf_025) %in% c("Total_Yield",paste("Yield_",gears,sep="")) ] 
production_yield_4 <- production_sup_075[,colnames(production_sup_075) %in% c("Total_Yield",paste("Yield_",gears,sep="")) ]          
}

}


nb_graphs=nb_gears + 1
nb_sheets= ifelse(modulo(nb_graphs,8)==0, as.integer(nb_graphs/8),as.integer(nb_graphs/8)+1)




for (i in 1:nb_sheets) {
if (nb_sheets!= 1) {
nome_file <- paste(YIELDBYGEAR_graph_grouped,"_sheet", i,".jpg", sep="")
} else {
nome_file <- paste(YIELDBYGEAR_graph_grouped,".jpg", sep="")
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
           
         
if (!RUN_CI_FORE) {
Ymin = min(production_yield$Total_Yield,na.rm=TRUE)
Ymax = max(production_yield$Total_Yield,na.rm=TRUE)
} else {
Ymin = min(production_yield$Total_Yield,production_yield_1$Total_Yield,production_yield_2$Total_Yield,production_yield_4$Total_Yield,production_yield_5$Total_Yield, na.rm=TRUE)
Ymax = max(production_yield$Total_Yield,production_yield_1$Total_Yield,production_yield_2$Total_Yield,production_yield_4$Total_Yield,production_yield_5$Total_Yield, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
                                                                       
           plot(loca_xa, production_yield$Total_Yield,   type="l",   main="Total Yield", xlab="Time_slice [year]", ylab="tons", ylim=Ylim,lwd=2)  # 
           points(loca_xa, production_yield$Total_Yield,   type="p",  pch=20, cex=1.5)    
              mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
           
if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
           
 if (RUN_CI_FORE) {

     polygon(c(loca_xa, rev(loca_xa)), c( production_yield_1$Total_Yield, rev(production_yield_2$Total_Yield)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_2$Total_Yield, rev(production_yield$Total_Yield)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield$Total_Yield, rev(production_yield_4$Total_Yield)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_4$Total_Yield, rev(production_yield_5$Total_Yield)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_yield_1$Total_Yield,  type="l", lty=2) 
    	lines(loca_xa, production_yield_2$Total_Yield ,  type="l", lty=3) 
    	lines(loca_xa, production_yield_4$Total_Yield ,  type="l", lty=3) 
       lines(loca_xa, production_yield_5$Total_Yield,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )
 
#    lines(loca_xa, production_yield_inf$Total_Yield,  type="l", lty=2) 
#	  lines(loca_xa, production_yield_sup$Total_Yield ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("black","black","black"), bty="n" )
              }
              
           } else {
           if (nb_gears!=1){          # loca_xa,production[,4+m-1]
           
           
 if (!RUN_CI_FORE) {
Ymin = min(production_yield[,m],na.rm=TRUE)
Ymax = max(production_yield[,m],na.rm=TRUE)
} else {
Ymin = min(production_yield[,m],production_yield_1[,m],production_yield_2[,m],production_yield_4[,m],production_yield_5[,m], na.rm=TRUE)
Ymax = max(production_yield[,m],production_yield_1[,m],production_yield_2[,m],production_yield_4[,m],production_yield_5[,m], na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
                                                           
           plot(loca_xa,production_yield[,m] ,type="l",  main=paste("Yield for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,ylim=Ylim, col = m)  
               points(loca_xa, production_yield[,m],   type="p",  pch=20, col = m, cex=1.5)    
              mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
      
            if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
      
       if (RUN_CI_FORE) {
       
        polygon(c(loca_xa, rev(loca_xa)), c( production_yield_1[,m], rev(production_yield_2[,m])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_2[,m], rev(production_yield[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield[,m], rev(production_yield_4[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_4[,m], rev(production_yield_5[,m])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_yield_1[,m],  type="l", lty=2) 
    	lines(loca_xa, production_yield_2[,m],  type="l", lty=3) 
    	lines(loca_xa, production_yield_4[,m],  type="l", lty=3) 
       lines(loca_xa, production_yield_5[,m],  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )
       
       
#    lines(loca_xa, production_yield_inf[,m],  type="l", lty=2) 
#	  lines(loca_xa, production_yield_sup[,m] ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
           } else {
           
if (!RUN_CI_FORE) {
Ymin = min(production_yield$Total_Yield,na.rm=TRUE)
Ymax = max(production_yield$Total_Yield,na.rm=TRUE)
} else {
Ymin = min(production_yield$Total_Yield,production_yield_1$Total_Yield,production_yield_2$Total_Yield,production_yield_4$Total_Yield,production_yield_5$Total_Yield, na.rm=TRUE)
Ymax = max(production_yield$Total_Yield,production_yield_1$Total_Yield,production_yield_2$Total_Yield,production_yield_4$Total_Yield,production_yield_5$Total_Yield, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
           
   plot(loca_xa,production_yield$Total_Yield,type="l",  main=paste("Yield for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,ylim=Ylim, col = m)  
                      points(loca_xa, production_yield$Total_Yield,   type="p",  pch=20, col = m, cex=1.5)    
              mtext(GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
  
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
  
  if (RUN_CI_FORE) {
  
       polygon(c(loca_xa, rev(loca_xa)), c( production_yield_1$Total_Yield, rev(production_yield_2$Total_Yield)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_2$Total_Yield, rev(production_yield$Total_Yield)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield$Total_Yield, rev(production_yield_4$Total_Yield)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_yield_4$Total_Yield, rev(production_yield_5$Total_Yield)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_yield_1$Total_Yield,  type="l", lty=2) 
    	lines(loca_xa, production_yield_2$Total_Yield ,  type="l", lty=3) 
    	lines(loca_xa, production_yield_4$Total_Yield ,  type="l", lty=3) 
       lines(loca_xa, production_yield_5$Total_Yield,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )
  
#    lines(loca_xa, production_yield_inf$Total_Yield,  type="l", lty=2) 
#	  lines(loca_xa, production_yield_sup$Total_Yield ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
           }
                  }
     }
   #  mtext(paste("ALADYM  ", GLO$ThisIsVersion),cex=0.7,side=4,outer=FALSE)
        mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()
} 
 

#-------------------------------------------------------------------------------
# Landing per attrezzo
#-------------------------------------------------------------------------------    


if (nb_gears==1){
production_landing <- data.frame(production[,colnames(production) == "Total_Landing"])
colnames(production_landing)= "Total_Landing"           
} else{
production_landing <- production[,colnames(production) %in% c("Total_Landing",paste("Landing_",gears,sep="")) ]     
}
Ymin = min(production_landing, na.rm=T)             
Ymax = max(production_landing, na.rm=T)

if (RUN_CI_FORE) {
if (nb_gears==1){
production_landing_1 <- data.frame(production_inf[,colnames(production_inf) == "Total_Landing"])
colnames(production_landing_1)= "Total_Landing"      
production_landing_5 <- data.frame(production_sup[,colnames(production_sup) == "Total_Landing"])
colnames(production_landing_5)= "Total_Landing" 

production_landing_2 <- data.frame(production_inf_025[,colnames(production_inf_025) == "Total_Landing"]  )          
colnames(production_landing_2)="Total_Landing"
production_landing_4 <- data.frame(production_sup_075[,colnames(production_sup_075) == "Total_Landing"]  )          
colnames(production_landing_4)="Total_Landing"
         
} else{
production_landing_1 <- production_inf[,colnames(production_inf) %in% c("Total_Landing",paste("Landing_",gears,sep="")) ]   
production_landing_5 <- production_sup[,colnames(production_sup) %in% c("Total_Landing",paste("Landing_",gears,sep="")) ]

production_landing_2 <- production_inf_025[,colnames(production_inf_025) %in% c("Total_Landing",paste("Landing_",gears,sep="")) ]   
production_landing_4 <- production_sup_075[,colnames(production_sup_075) %in% c("Total_Landing",paste("Landing_",gears,sep="")) ]        
}

}

 for (i in 1:nb_sheets) {
if (nb_sheets!= 1) {
nome_file <- paste(LANDINGBYGEAR_graph_grouped,"_sheet", i,".jpg", sep="")
} else {
nome_file <- paste(LANDINGBYGEAR_graph_grouped,".jpg", sep="")
}

            
             if (nb_gears <= 4) {
      jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
    par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
 } else {  
  jpeg(file=nome_file, width=35, height=21, bg="white", units="cm",res=200)  # windows() 
    par(mfrow=c(2,4), oma = c(0, 0, 3, 0))
 }
            
     if ((8*i)> nb_graphs ){
        nb_loops = nb_graphs - 8*(i-1)
     }  else {
        nb_loops = 8*i
     }
     
       for (m in (8*i-7):(8*(i-1)+(nb_loops))) {
           if (m==1){ 
           
           if (!RUN_CI_FORE) {
Ymin = min(production_landing$Total_Landing,na.rm=TRUE)
Ymax = max(production_landing$Total_Landing,na.rm=TRUE)
} else {
Ymin = min(production_landing$Total_Landing,production_landing_1$Total_Landing,production_landing_2$Total_Landing,production_landing_4$Total_Landing,production_landing_5$Total_Landing, na.rm=TRUE)
Ymax = max(production_landing$Total_Landing,production_landing_1$Total_Landing,production_landing_2$Total_Landing,production_landing_4$Total_Landing,production_landing_5$Total_Landing, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
           
                         
           plot(loca_xa, production_landing$Total_Landing,   type="l",  main="Total Landing (total)", xlab="Time_slice [year]", ylab="tons", ylim=Ylim,lwd=2)  #  
            points(loca_xa,production_landing$Total_Landing,   type="p",  pch=20, cex=1.5)    
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
            
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
  
            
             if (RUN_CI_FORE) {
             
     polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1$Total_Landing, rev(production_landing_2$Total_Landing)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2$Total_Landing, rev(production_landing$Total_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing$Total_Landing, rev(production_landing_4$Total_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4$Total_Landing, rev(production_landing_5$Total_Landing)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1$Total_Landing,  type="l", lty=2) 
    	lines(loca_xa, production_landing_2$Total_Landing ,  type="l", lty=3) 
    	lines(loca_xa, production_landing_4$Total_Landing ,  type="l", lty=3) 
       lines(loca_xa, production_landing_5$Total_Landing,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )          
             
#    lines(loca_xa, production_landing_inf$Total_Landing,  type="l", lty=2) 
#	  lines(loca_xa, production_landing_sup$Total_Landing ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("black","black","black"), bty="n" )
              }
           } else { 
           if (nb_gears!=1){                     # production[,(4+4*nb_gears)+m-1]
           
             
 if (!RUN_CI_FORE) {
Ymin = min(production_landing[,m],na.rm=TRUE)
Ymax = max(production_landing[,m],na.rm=TRUE)
} else {
Ymin = min(production_landing[,m],production_landing_1[,m],production_landing_2[,m],production_landing_4[,m],production_landing_5[,m], na.rm=TRUE)
Ymax = max(production_landing[,m],production_landing_1[,m],production_landing_2[,m],production_landing_4[,m],production_landing_5[,m], na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
                
      
           plot(loca_xa,production_landing[,m],type="l",  main=paste("Landing for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,col = m,ylim=Ylim)  
     points(loca_xa,production_landing[,m],   type="p",  pch=20, col = m, cex=1.5)    
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
           
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
  
           
              if (RUN_CI_FORE) {
              
       polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1[,m], rev(production_landing_2[,m])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2[,m], rev(production_landing[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing[,m], rev(production_landing_4[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4[,m], rev(production_landing_5[,m])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1[,m],  type="l", lty=2) 
    	lines(loca_xa, production_landing_2[,m],  type="l", lty=3) 
    	lines(loca_xa, production_landing_4[,m],  type="l", lty=3) 
       lines(loca_xa, production_landing_5[,m],  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )
                
              
              
#    lines(loca_xa, production_landing_inf[,m],  type="l", lty=2) 
#	  lines(loca_xa, production_landing_sup[,m] ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
           } else {                # production[,7]
           
           if (!RUN_CI_FORE) {
Ymin = min(production_landing$Total_Landing,na.rm=TRUE)
Ymax = max(production_landing$Total_Landing,na.rm=TRUE)
} else {
Ymin = min(production_landing$Total_Landing,production_landing_1$Total_Landing,production_landing_2$Total_Landing,production_landing_4$Total_Landing,production_landing_5$Total_Landing, na.rm=TRUE)
Ymax = max(production_landing$Total_Landing,production_landing_1$Total_Landing,production_landing_2$Total_Landing,production_landing_4$Total_Landing,production_landing_5$Total_Landing, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
                
           
    plot(loca_xa, production_landing$Total_Landing,type="l",main=paste("Landing for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,col = m,ylim=Ylim)  
              points(loca_xa,production_landing$Total_Landing,   type="p",  pch=20, col = m, cex=1.5)   
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
    
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
   
    
     if (RUN_CI_FORE) {
     
          polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1$Total_Landing, rev(production_landing_2$Total_Landing)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2$Total_Landing, rev(production_landing$Total_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing$Total_Landing, rev(production_landing_4$Total_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4$Total_Landing, rev(production_landing_5$Total_Landing)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1$Total_Landing,  type="l", lty=2) 
    	lines(loca_xa, production_landing_2$Total_Landing ,  type="l", lty=3) 
    	lines(loca_xa, production_landing_4$Total_Landing ,  type="l", lty=3) 
       lines(loca_xa, production_landing_5$Total_Landing,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )         
#    lines(loca_xa, production_landing_inf$Total_Landing,  type="l", lty=2) 
#	  lines(loca_xa, production_landing_sup$Total_Landing ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
           }  
       }
       
       }
         mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()
} 


#-------------------------------------------------------------------------------
#Discard  per attrezzo
#-------------------------------------------------------------------------------    

if (nb_gears==1) {
production_discard <- data.frame(production[,colnames(production) == "Total_Discard"] )      
colnames(production_discard)="Total_Discard"     
} else {
production_discard <- production[,colnames(production) %in% c("Total_Discard",paste("Discard_",gears,sep="")) ]     
}


if (RUN_CI_FORE) {
if (nb_gears==1){
production_discard_inf <- data.frame(production_inf[,colnames(production_inf) == "Total_Discard"] )      
colnames(production_discard_inf)="Total_Discard"  
production_discard_sup <- data.frame(production_sup[,colnames(production_sup) == "Total_Discard"] )      
colnames(production_discard_sup)="Total_Discard"     
} else{
production_discard_inf <- production_inf[,colnames(production_inf) %in% c("Total_Discard",paste("Discard_",gears,sep="")) ]     
production_discard_sup <- production_sup[,colnames(production_sup) %in% c("Total_Discard",paste("Discard_",gears,sep="")) ]     
}


}


#
#if(nb_gears!=1){
#Ymin = ifelse(any(!is.na(INP$Discard))& any(INP$Discard != 0), min(production[,(1+9*nb_gears):(1+10*nb_gears)]),NA)
#Ymax = ifelse(any(!is.na(INP$Discard))& any(INP$Discard != 0),max(production[,(1+9*nb_gears):(1+10*nb_gears)]),NA)
#} else {
#Ymin = ifelse(any(!is.na(INP$Discard))& any(INP$Discard != 0),min(production[,10]),NA)
#Ymax = ifelse(any(!is.na(INP$Discard))& any(INP$Discard != 0),max(production[,10]),NA)
#}




 for (i in 1:nb_sheets) {
if (nb_sheets!= 1) {
nome_file <- paste(DISCARDBYGEAR_graph_grouped,"_sheet", i,".jpg", sep="")
} else {
nome_file <- paste(DISCARDBYGEAR_graph_grouped,".jpg", sep="")
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
           
             if (!RUN_CI_FORE) {
Ymin = min(production_discard$Total_Discard,na.rm=TRUE)
Ymax = max(production_discard$Total_Discard,na.rm=TRUE)
} else {
Ymin = min(production_discard$Total_Discard,production_discard_1$Total_Discard,production_discard_2$Total_Discard,production_discard_4$Total_Discard,production_discard_5$Total_Discard, na.rm=TRUE)
Ymax = max(production_discard$Total_Discard,production_discard_1$Total_Discard,production_discard_2$Total_Discard,production_discard_4$Total_Discard,production_discard_5$Total_Discard, na.rm=TRUE)
}

if (Ymin == Ymax & Ymin == 0) {
  Ymin=0
  Ymax=2
  Ylim=c(0,2)
} else {
  Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
}
		   
           
		                                                    # which(is.na(production_discard$Total_Discard))
		    if (!all( is.na(production_discard$Total_Discard[] ) ) ) {
		   
            production_discard$Total_Discard[which(is.na(production_discard$Total_Discard))] <- 0
           plot(loca_xa, production_discard$Total_Discard,   type="l",  main="Total Discard", xlab="Time_slice [year]", ylab="tons", ylim=Ylim,lwd=2)  # 
               points(loca_xa, production_discard$Total_Discard,    type="p",  pch=20,  cex=1.5)    
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
            
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
          
             
                  if (RUN_CI_FORE) {
                  
                   polygon(c(loca_xa, rev(loca_xa)), c( production_discard_1$Total_Discard, rev(production_discard_2$Total_Discard)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_2$Total_Discard, rev(production_discard$Total_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard$Total_Discard, rev(production_discard_4$Total_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_4$Total_Discard, rev(production_discard_5$Total_Discard)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_discard_1$Total_Discard,  type="l", lty=2) 
    	lines(loca_xa, production_discard_2$Total_Discard ,  type="l", lty=3) 
    	lines(loca_xa, production_discard_4$Total_Discard ,  type="l", lty=3) 
       lines(loca_xa, production_discard_5$Total_Discard,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )    
                  
                  
#    lines(loca_xa, production_discard_inf$Total_Discard,  type="l", lty=2) 
#	  lines(loca_xa, production_discard_sup$Total_Discard ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
        
						 }  else {
           plot(1,1,type="n",main="Total Discard", xlab = "Time slice [year]", ylab = "tons")
            text(1,1, "Not Available", cex=1.5,side=4,outer=FALSE)
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
         
             }
		
           } else { 
           if((nb_gears!=1)) {		   # production[,(1+9*nb_gears)+m-1]

                        if (!RUN_CI_FORE) {
Ymin = min(production_discard[,m],na.rm=TRUE)
Ymax = max(production_discard[,m],na.rm=TRUE)
} else {
Ymin = min(production_discard[,m],production_discard_1[,m],production_discard_2[,m],production_discard_4[,m],production_discard_5[,m], na.rm=TRUE)
Ymax = max(production_discard[,m],production_discard_1[,m],production_discard_2[,m],production_discard_4[,m],production_discard_5[,m], na.rm=TRUE)
}

if (Ymin == Ymax & Ymin == 0) {
  Ymin=0
  Ymax=2
  Ylim=c(0,2)
} else {
  Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
}

					 if (!all( is.na(production_discard[,m]))) { 
					 
    plot(loca_xa,production_discard[,m],type="l",  main=paste(" Discard for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,col = m,ylim=Ylim)  
                   points(loca_xa, production_discard[,m],    type="p",  pch=20, col = m, cex=1.5)    
                   mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
     
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
   
     
     if (RUN_CI_FORE) {
     
                    polygon(c(loca_xa, rev(loca_xa)), c( production_discard_1[,m], rev(production_discard_2[,m])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_2[,m], rev(production_discard[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard[,m], rev(production_discard_4[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_4[,m], rev(production_discard_5[,m])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_discard_1[,m],  type="l", lty=2) 
    	lines(loca_xa, production_discard_2[,m] ,  type="l", lty=3) 
    	lines(loca_xa, production_discard_4[,m] ,  type="l", lty=3) 
       lines(loca_xa, production_discard_5[,m],  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black",m,"black","black"), bty="n" )        
     
#    lines(loca_xa, production_discard_inf[,m],  type="l", lty=2) 
#	  lines(loca_xa, production_discard_sup[,m] ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }     

} else { 
        plot(1,1,type="n",main=paste("Discard for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons")
             text(1,1, "Not Available", cex=1.5,side=4,outer=FALSE)
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
			}

					}   else {                # production[,7]
	
	 if (!RUN_CI_FORE) {
Ymin = min(production_discard$Total_Discard,na.rm=TRUE)
Ymax = max(production_discard$Total_Discard,na.rm=TRUE)
} else {
Ymin = min(production_discard$Total_Discard,production_discard_1$Total_Discard,production_discard_2$Total_Discard,production_discard_4$Total_Discard,production_discard_5$Total_Discard, na.rm=TRUE)
Ymax = max(production_discard$Total_Discard,production_discard_1$Total_Discard,production_discard_2$Total_Discard,production_discard_4$Total_Discard,production_discard_5$Total_Discard, na.rm=TRUE)
}

if (Ymin == Ymax & Ymin == 0) {
  Ymin=0
  Ymax=2
  Ylim=c(0,2)
} else {
  Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
}
		   
	
	if (!all( is.na(production_discard$Total_Discard) )) {  	
					
   plot(loca_xa, production_discard$Total_Discard,type="l", main=paste("Discard for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "tons",lwd = 2,col = m,ylim=Ylim)  
                       points(loca_xa, production_discard$Total_Discard,    type="p",  pch=20, col = m, cex=1.5)    
    mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
             
              if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.55) 
    } 
  
   
      if (RUN_CI_FORE) {
 polygon(c(loca_xa, rev(loca_xa)), c( production_discard_1$Total_Discard, rev(production_discard_2$Total_Discard)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_2$Total_Discard, rev(production_discard$Total_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard$Total_Discard, rev(production_discard_4$Total_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_4$Total_Discard, rev(production_discard_5$Total_Discard)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_discard_1$Total_Discard,  type="l", lty=2) 
    	lines(loca_xa, production_discard_2$Total_Discard ,  type="l", lty=3) 
    	lines(loca_xa, production_discard_4$Total_Discard ,  type="l", lty=3) 
       lines(loca_xa, production_discard_5$Total_Discard,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","black","black","black"), bty="n" )    
#    lines(loca_xa, production_discard_inf$Total_Discard,  type="l", lty=2) 
#	  lines(loca_xa, production_discard_sup$Total_Discard ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c(m,"black","black"), bty="n" )
              }
			  
}  else {
           plot(1,1,type="n",main="Total Discard", xlab = "Time slice [year]", ylab = "tons")
            text(1,1, "Not Available", cex=1.5,side=4,outer=FALSE)
             mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
         
             }
			
			     }  
       }
     }
          mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
dev.off() 
}


}