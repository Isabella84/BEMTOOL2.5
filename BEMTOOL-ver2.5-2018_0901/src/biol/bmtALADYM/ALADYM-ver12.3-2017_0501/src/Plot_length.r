# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#---------------------------- immagini:-----------------------------------------
# Mean length
# Mean length in catch
# Mean length in landing
# Mean length in discard
#-------------------------------------------------------------------------------

Plot_length <- function() {

gears <- FLEETSEGMENTS_names

population = read.table(POPULATION_table,header=TRUE,sep=";")
production = read.table(PRODUCTION_table,header=TRUE,sep=";")
   
if (nb_gears==1) {

colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio", "LandingObligation")

}  else {

colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""),"Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""),paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))

}


if (RUN_CI_FORE) {

# read population table
population_inf <- read.table(paste(POPULATION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
#population_inf = population_inf[population_inf$Year %in% loca_xa, ]
population_sup <- population_inf
population_sup_075 <- population_inf
population_inf_025 <- population_inf

population_inf <- population_inf[population_inf$percentile == 0.05, 1:(ncol(population_inf)-1)]
population_sup <- population_sup[population_sup$percentile == 0.95, 1:(ncol(population_sup)-1)]
population_inf_025 <- population_inf_025[population_inf_025$percentile == 0.25, 1:(ncol(population_inf_025)-1)]
population_sup_075 <- population_sup_075[population_sup_075$percentile == 0.75, 1:(ncol(population_sup_075)-1)]

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

colnames (production_inf) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""),"Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""),paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))
colnames (production_sup) =c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""),"Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""),paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))
colnames (production_inf_025) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""),"Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""),paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))
colnames (production_sup_075) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""),"Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""),paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))

}

}
 
#
#
#
#
## ************************************************************************************* 
# lunghezza media
## ************************************************************************************* 


gears= as.character(t(FLEETSEGMENTS_names))
loca_xa <- 1:(GLO$L_number / INP$Time_slice) 

if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}

# legenda
# windows(width=21, height=21)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#print("Saving ML pop expl in progress...", quote=F)
      
# jpeg(file=MEANLENGTH_graph, width=21, height=21, bg="white", units="cm",res=200) 
jpeg(file=MEANLENGTH_graph_expl, width=21, height=21, bg="white", units="cm",res=200)   
 # par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
 
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 
	if (!RUN_CI_FORE) { 
	Ymin = min(population$Mean_length_of_exploited_pop, na.rm=T)
	Ymax =  max(population$Mean_length_of_exploited_pop, na.rm=T)
  } else {
  
Ymin = min(population$Mean_length_of_exploited_pop, population_inf$Mean_length_of_exploited_pop, population_sup$Mean_length_of_exploited_pop, population_inf_025$Mean_length_of_exploited_pop, population_sup_075$Mean_length_of_exploited_pop, na.rm=T)
Ymax = max(population$Mean_length_of_exploited_pop, population_inf$Mean_length_of_exploited_pop, population_sup$Mean_length_of_exploited_pop, population_inf_025$Mean_length_of_exploited_pop, population_sup_075$Mean_length_of_exploited_pop, na.rm=T)
	}
	
Ylim = c(Ymin, Ymax + ( (Ymax-Ymin)/3) )


# grafico 1: Fished length mean
plot(loca_xa, population$Mean_length_of_exploited_pop,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length of exploited population"), xlab="Time_slice [year]", ylab="mm",lwd = 2, col="red",ylim=Ylim)
mtext( GLO$ThisIsVersion,side=4,outer=FALSE) #  
  	
if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
} 
    
    if (RUN_CI_FORE) {

	 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Mean_length_of_exploited_pop, rev(population_inf_025$Mean_length_of_exploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Mean_length_of_exploited_pop, rev(population$Mean_length_of_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Mean_length_of_exploited_pop, rev(population_sup_075$Mean_length_of_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Mean_length_of_exploited_pop, rev(population_sup$Mean_length_of_exploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Mean_length_of_exploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Mean_length_of_exploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Mean_length_of_exploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Mean_length_of_exploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
	}  
        mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
dev.off()


## ************************************************************************************* 
# grafico 2 : Unfished length mean
## ************************************************************************************* 

#print("Saving ML pop unexpl in progress...", quote=F)

jpeg(file=MEANLENGTH_graph_unexpl, width=21, height=21, bg="white", units="cm",res=200)   

 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 
#           mtext(paste("ALADYM  ", GLO$ThisIsVersion),cex=0.7,side=4,outer=FALSE) #  

	if (!RUN_CI_FORE) {                                  
	Ymin = min(population$Mean_length_of_unexploited_pop, na.rm=T)
	Ymax = max(population$Mean_length_of_unexploited_pop, na.rm=T)
	} else {
Ymin = min(population$Mean_length_of_unexploited_pop, population_inf$Mean_length_of_unexploited_pop, population_sup$Mean_length_of_unexploited_pop, population_inf_025$Mean_length_of_unexploited_pop, population_sup_075$Mean_length_of_unexploited_pop,  na.rm=T)
Ymax =  max(population$Mean_length_of_unexploited_pop, population_inf$Mean_length_of_unexploited_pop, population_sup$Mean_length_of_unexploited_pop, population_inf_025$Mean_length_of_unexploited_pop, population_sup_075$Mean_length_of_unexploited_pop,  na.rm=T)

	}

  Ylim = c(Ymin, Ymax + ( (Ymax-Ymin)/3) )	


# grafico 2: Unfished length mean
plot(loca_xa, population$Mean_length_of_unexploited_pop,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length of unexploited population"), xlab="Time_slice [year]", ylab="mm",lwd = 2, col="green",ylim=Ylim)
           mtext( GLO$ThisIsVersion,side=4,outer=FALSE) #  
  
       if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  	if (RUN_CI_FORE) {
  	
 	 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Mean_length_of_unexploited_pop, rev(population_inf_025$Mean_length_of_unexploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Mean_length_of_unexploited_pop, rev(population$Mean_length_of_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Mean_length_of_unexploited_pop, rev(population_sup_075$Mean_length_of_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Mean_length_of_unexploited_pop, rev(population_sup$Mean_length_of_unexploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Mean_length_of_unexploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Mean_length_of_unexploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Mean_length_of_unexploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Mean_length_of_unexploited_pop,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","green","black","black"), bty="n" )  	

	}  
         mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()


# grafico 3: Fished spawners length mean 

#print("Saving ML pop SS expl in progress...", quote=F)

jpeg(file=MEANLENGTH_graph_SS_expl, width=21, height=21, bg="white", units="cm",res=200)   

 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)

	if (!RUN_CI_FORE) { 
	Ymin = min(population$Mean_length_SS_of_exploited_pop, na.rm=T)
	Ymax = max(population$Mean_length_SS_of_exploited_pop, na.rm=T)
	} else {
Ymin = min(population$Mean_length_SS_of_exploited_pop, population_inf$Mean_length_SS_of_exploited_pop, population_sup$Mean_length_SS_of_exploited_pop, population_inf_025$Mean_length_SS_of_exploited_pop, population_sup_075$Mean_length_SS_of_exploited_pop, na.rm=T)
Ymax = max(population$Mean_length_SS_of_exploited_pop, population_inf$Mean_length_SS_of_exploited_pop, population_sup$Mean_length_SS_of_exploited_pop, population_inf_025$Mean_length_SS_of_exploited_pop, population_sup_075$Mean_length_SS_of_exploited_pop, na.rm=T)
}

   Ylim = c(Ymin, Ymax + ( (Ymax-Ymin)/3) )	

# grafico 3:  Fished spawners length mean 
plot(loca_xa, population$Mean_length_SS_of_exploited_pop,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length SS of exploited population"), xlab="Time_slice [year]", ylab="mm",lwd = 2, lty=2, col="red",ylim=Ylim)
           mtext(GLO$ThisIsVersion,side=4,outer=FALSE) #  
  
       if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  	if (RUN_CI_FORE) {
  	
polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Mean_length_SS_of_exploited_pop, rev(population_inf_025$Mean_length_SS_of_exploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Mean_length_SS_of_exploited_pop, rev(population$Mean_length_SS_of_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Mean_length_SS_of_exploited_pop, rev(population_sup_075$Mean_length_SS_of_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Mean_length_SS_of_exploited_pop, rev(population_sup$Mean_length_SS_of_exploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Mean_length_SS_of_exploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Mean_length_SS_of_exploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Mean_length_SS_of_exploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Mean_length_SS_of_exploited_pop,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )  	

	}  
      mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()

## grafico 4: Unfished spawners length mean

#print("Saving ML pop SS unexpl in progress...", quote=F)

 jpeg(file=MEANLENGTH_graph_SS_unexpl, width=21, height=21, bg="white", units="cm",res=200)   

 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 
	if (!RUN_CI_FORE) { 
	Ymin = min(population$Mean_length_SS_of_unexploited_pop, na.rm=T)
	Ymax = max(population$Mean_length_SS_of_unexploited_pop, na.rm=T)
	} else {
Ymin = min(population$Mean_length_SS_of_unexploited_pop, population_inf$Mean_length_SS_of_unexploited_pop, population_sup$Mean_length_SS_of_unexploited_pop, population_inf_025$Mean_length_SS_of_unexploited_pop, population_sup_075$Mean_length_SS_of_unexploited_pop, na.rm=T)
Ymax = max(population$Mean_length_SS_of_unexploited_pop, population_inf$Mean_length_SS_of_unexploited_pop, population_sup$Mean_length_SS_of_unexploited_pop, population_inf_025$Mean_length_SS_of_unexploited_pop, population_sup_075$Mean_length_SS_of_unexploited_pop, na.rm=T)
}

   Ylim = c(Ymin, Ymax + ( (Ymax-Ymin)/3) )	

# grafico 4:  Unfished spawners length mean 
plot(loca_xa, population$Mean_length_SS_of_unexploited_pop,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length SS of unexploited population"), xlab="Time_slice [year]", ylab="mm",lwd = 2, lty=2, col="green",ylim=Ylim)
           mtext( GLO$ThisIsVersion,side=4,outer=FALSE) #  
  
  if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  	if (RUN_CI_FORE) {
  	
polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Mean_length_SS_of_unexploited_pop, rev(population_inf_025$Mean_length_SS_of_unexploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Mean_length_SS_of_unexploited_pop, rev(population$Mean_length_SS_of_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Mean_length_SS_of_unexploited_pop, rev(population_sup_075$Mean_length_SS_of_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Mean_length_SS_of_unexploited_pop, rev(population_sup$Mean_length_SS_of_unexploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Mean_length_SS_of_unexploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Mean_length_SS_of_unexploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Mean_length_SS_of_unexploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Mean_length_SS_of_unexploited_pop,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","green","black","black"), bty="n" )  	

	}  

      mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)

dev.off()


#
#
#
#
#-------------------------------------------------------------------------------
# lunghezza media per attrezzo - Yield
#-------------------------------------------------------------------------------  


if(nb_gears==1) {
columns_to_plot = c("Mean_length_in_catch")
}  else {
columns_to_plot = c("Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""))
}

production_catch <- data.frame(production[, colnames(production) %in% columns_to_plot]  )
colnames(production_catch)  <-  columns_to_plot

 if (RUN_CI_FORE) {
production_catch_1 <-  data.frame(production_inf[, colnames(production_inf) %in% columns_to_plot]  )
production_catch_2 <- data.frame(production_inf_025[, colnames(production_inf_025) %in% columns_to_plot]  )
production_catch_4 <- data.frame(production_sup_075[, colnames(production_sup_075) %in% columns_to_plot]   )
production_catch_5 <- data.frame(production_sup[, colnames(production_sup) %in% columns_to_plot]  )

if (length(columns_to_plot) == 1) {
colnames(production_catch_1) <-  "Mean_length_in_catch"
colnames(production_catch_2) <-  "Mean_length_in_catch"
colnames(production_catch_4) <-  "Mean_length_in_catch"
colnames(production_catch_5) <-  "Mean_length_in_catch"
}

}

if (!RUN_CI_FORE) {
Ymin = min(production_catch,na.rm=TRUE)
Ymax = max(production_catch,na.rm=TRUE)
} else {
Ymin = min(production_catch,production_catch_1,production_catch_2,production_catch_4,production_catch_5, na.rm=TRUE)
Ymax = max(production_catch,production_catch_1,production_catch_2,production_catch_4,production_catch_5, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )


nb_graphs=nb_gears + 1


for (m in 1:nb_graphs) {
#print(paste("Saving ML in Catch by gear in progress [", m,"]...", sep=""), quote=F)

if (m==1) {
nome_file <- paste(MEANLENGTHINCATCHBYGEAR_graph,"_total.jpg", sep="")
} else {
nome_file <- paste(MEANLENGTHINCATCHBYGEAR_graph,"_", m-1,".jpg", sep="")
}

jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)

#for (m in (8*i-7):(8*(i-1)+(nb_loops))) {


if (m==1) {


if (!RUN_CI_FORE) {
Ymin = min(production_catch$Mean_length_in_catch,na.rm=TRUE)
Ymax = max(production_catch$Mean_length_in_catch,na.rm=TRUE)
} else {
Ymin = min(production_catch$Mean_length_in_catch,production_catch_1$Mean_length_in_catch,production_catch_2$Mean_length_in_catch,production_catch_4$Mean_length_in_catch,production_catch_5$Mean_length_in_catch, na.rm=TRUE)
Ymax = max(production_catch$Mean_length_in_catch,production_catch_1$Mean_length_in_catch,production_catch_2$Mean_length_in_catch,production_catch_4$Mean_length_in_catch,production_catch_5$Mean_length_in_catch, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )


plot(loca_xa, production_catch$Mean_length_in_catch,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length in catch (total)"), xlab="Time_slice [year]", ylab="mm", ylim=Ylim,lwd=2, col="blue")  #  
mtext( GLO$ThisIsVersion,side=4,outer=FALSE)

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 

if (RUN_CI_FORE) {

polygon(c(loca_xa, rev(loca_xa)), c(production_catch_1$Mean_length_in_catch, rev(production_catch_2$Mean_length_in_catch)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_2$Mean_length_in_catch, rev(production_catch$Mean_length_in_catch)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch$Mean_length_in_catch, rev(production_catch_4$Mean_length_in_catch)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_4$Mean_length_in_catch, rev(production_catch_5$Mean_length_in_catch)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_catch_1$Mean_length_in_catch,  type="l", lty=2) 
    	lines(loca_xa, production_catch_2$Mean_length_in_catch ,  type="l", lty=3) 
    	lines(loca_xa, production_catch_4$Mean_length_in_catch ,  type="l", lty=3) 
       lines(loca_xa, production_catch_5$Mean_length_in_catch,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  	
	}  
} else {

if (nb_gears != 1) {            #5+nb_gears+m-1

 if (!RUN_CI_FORE) {
Ymin = min(production_catch[,m],na.rm=TRUE)
Ymax = max(production_catch[,m],na.rm=TRUE)
} else {
Ymin = min(production_catch[,m],production_catch_1[,m],production_catch_2[,m],production_catch_4[,m],production_catch_5[,m], na.rm=TRUE)
Ymax = max(production_catch[,m],production_catch_1[,m],production_catch_2[,m],production_catch_4[,m],production_catch_5[,m], na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )

plot(loca_xa,production_catch[,m],type="b",  pch=20,  main=paste(BMT_SPECIES[ALADYM_spe], "- ML in catch for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "mm",lwd = 2, col="blue", ylim=Ylim)  
mtext(GLO$ThisIsVersion,side=4,outer=FALSE)

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 

if (RUN_CI_FORE) {
                                                          #  5+nb_gears+m-1
 polygon(c(loca_xa, rev(loca_xa)), c( production_catch_1[,m], rev(production_catch_2[,m])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_2[,m], rev(production_catch[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch[,m], rev(production_catch_4[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_4[,m], rev(production_catch_5[,m])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_catch_1[,m],  type="l", lty=2) 
    	lines(loca_xa, production_catch_2[,m] ,  type="l", lty=3) 
    	lines(loca_xa, production_catch_4[,m] ,  type="l", lty=3) 
       lines(loca_xa, production_catch_5[,m],  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  	
	}  

} else {  

 if (!RUN_CI_FORE) {
Ymin = min(production_catch[,1],na.rm=TRUE)
Ymax = max(production_catch[,1],na.rm=TRUE)
} else {
Ymin = min(production_catch[,1],production_catch_1[,1],production_catch_2[,1],production_catch_4[,1],production_catch_5[,1], na.rm=TRUE)
Ymax = max(production_catch[,1],production_catch_1[,1],production_catch_2[,1],production_catch_4[,1],production_catch_5[,1], na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
                    # 5
plot(loca_xa, production_catch[,1],type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- ML in catch for",as.character(FLEETSEGMENTS_names[1])), xlab = "Time slice [year]", ylab = "mm",lwd = 2,col ="blue",ylim=Ylim) 
mtext(GLO$ThisIsVersion,side=4,outer=FALSE)

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 

if (RUN_CI_FORE) {

 polygon(c(loca_xa, rev(loca_xa)), c( production_catch_1[,1], rev(production_catch_2[,1])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_2[,1], rev(production_catch[,1])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch[,1], rev(production_catch_4[,1])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_catch_4[,1], rev(production_catch_5[,1])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_catch_1[,1],  type="l", lty=2) 
    	lines(loca_xa, production_catch_2[,1] ,  type="l", lty=3) 
    	lines(loca_xa, production_catch_4[,1] ,  type="l", lty=3) 
       lines(loca_xa, production_catch_5[,1],  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  	
	} 
}
} 
mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
dev.off()

}


#} 
 


#
#
#
#
#-------------------------------------------------------------------------------
# lunghezza media per attrezzo - Landing
#-------------------------------------------------------------------------------    



if(nb_gears==1) {
columns_to_plot = c("Mean_length_in_Landing")
}  else {
columns_to_plot = c("Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""))
}

production_landing <- data.frame(production[, colnames(production) %in% columns_to_plot]) 
colnames(production_landing)  <-  columns_to_plot

 if (RUN_CI_FORE) {
production_landing_1 <- data.frame(production_inf[, colnames(production_inf) %in% columns_to_plot] )
production_landing_2 <- data.frame(production_inf_025[, colnames(production_inf_025) %in% columns_to_plot]  )
production_landing_4 <- data.frame(production_sup_075[, colnames(production_sup_075) %in% columns_to_plot] ) 
production_landing_5 <- data.frame(production_sup[, colnames(production_sup) %in% columns_to_plot]    )

if (length(columns_to_plot) == 1) {
colnames(production_landing_1) <-  "Mean_length_in_Landing"
colnames(production_landing_2) <-  "Mean_length_in_Landing"
colnames(production_landing_4) <-  "Mean_length_in_Landing"
colnames(production_landing_5) <-  "Mean_length_in_Landing"
}

}


for (m in 1:nb_graphs) {
#  print(paste("Saving ML in Landing by gear in progress [", m,"]...", sep=""), quote=F)
if (m==1) {
nome_file <- paste(MEANLENGTHINLANDINGBYGEAR_graph,"_total.jpg", sep="")
} else {
nome_file <- paste(MEANLENGTHINLANDINGBYGEAR_graph,"_", m-1,".jpg", sep="")
}

jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)

if (m==1){



if (!RUN_CI_FORE) {
Ymin = min(production_landing$Mean_length_in_Landing,na.rm=TRUE)
Ymax = max(production_landing$Mean_length_in_Landing,na.rm=TRUE)
} else {
Ymin = min(production_landing$Mean_length_in_Landing,production_landing_1$Mean_length_in_Landing,production_landing_2$Mean_length_in_Landing,production_landing_4$Mean_length_in_Landing,production_landing_5$Mean_length_in_Landing, na.rm=TRUE)
Ymax = max(production_landing$Mean_length_in_Landing,production_landing_1$Mean_length_in_Landing,production_landing_2$Mean_length_in_Landing,production_landing_4$Mean_length_in_Landing,production_landing_5$Mean_length_in_Landing, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )

#if(!is.na(min(meanWequals(SRO$Landing_length_mean[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice)))){
plot(loca_xa, production_landing$Mean_length_in_Landing,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Mean length in Landing (total)"), xlab="Time_slice [year]", ylab="mm", ylim=Ylim,lwd=2, col="blue") 
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE) #

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
	
	if (RUN_CI_FORE) {
	
	 polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1$Mean_length_in_Landing, rev(production_landing_2$Mean_length_in_Landing)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2$Mean_length_in_Landing, rev(production_landing$Mean_length_in_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing$Mean_length_in_Landing, rev(production_landing_4$Mean_length_in_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4$Mean_length_in_Landing, rev(production_landing_5$Mean_length_in_Landing)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1$Mean_length_in_Landing,  type="l", lty=2) 
    	lines(loca_xa, production_landing_2$Mean_length_in_Landing ,  type="l", lty=3) 
    	lines(loca_xa, production_landing_4$Mean_length_in_Landing ,  type="l", lty=3) 
       lines(loca_xa, production_landing_5$Mean_length_in_Landing,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  

	}  
                                                           #& (!is.na(min(meanWequals(SRO$Landing_length_mean_gears[,m-1],GLO$L_number+1, INP$Time_slice))))) 
} else {
if (nb_gears!=1) { 

 if (!RUN_CI_FORE) {
Ymin = min(production_landing[,m],na.rm=TRUE)
Ymax = max(production_landing[,m],na.rm=TRUE)
} else {
Ymin = min(production_landing[,m],production_landing_1[,m],production_landing_2[,m],production_landing_4[,m],production_landing_5[,m], na.rm=TRUE)
Ymax = max(production_landing[,m],production_landing_1[,m],production_landing_2[,m],production_landing_4[,m],production_landing_5[,m], na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )

 
plot(loca_xa,production_landing[,m],type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- ML in Landing for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "mm",lwd = 2,col = "blue",ylim=Ylim)  
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE)

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  if (RUN_CI_FORE) {    
  
 polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1[,m], rev(production_landing_2[,m])), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2[,m], rev(production_landing[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing[,m], rev(production_landing_4[,m])), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4[,m], rev(production_landing_5[,m])), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1[,m],  type="l", lty=2) 
    	lines(loca_xa, production_landing_2[,m] ,  type="l", lty=3) 
    	lines(loca_xa, production_landing_4[,m] ,  type="l", lty=3) 
       lines(loca_xa, production_landing_5[,m],  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  
  
	}  
  
}  else  {  # = 1

if (!RUN_CI_FORE) {
Ymin = min(production_landing$Mean_length_in_Landing,na.rm=TRUE)
Ymax = max(production_landing$Mean_length_in_Landing,na.rm=TRUE)
} else {
Ymin = min(production_landing$Mean_length_in_Landing,production_landing_1$Mean_length_in_Landing,production_landing_2$Mean_length_in_Landing,production_landing_4$Mean_length_in_Landing,production_landing_5$Mean_length_in_Landing, na.rm=TRUE)
Ymax = max(production_landing$Mean_length_in_Landing,production_landing_1$Mean_length_in_Landing,production_landing_2$Mean_length_in_Landing,production_landing_4$Mean_length_in_Landing,production_landing_5$Mean_length_in_Landing, na.rm=TRUE)
}
Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )

plot(loca_xa,production_landing$Mean_length_in_Landing,type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- ML in Landing for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "mm",lwd = 2,col = "blue",ylim=Ylim)  
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE)
 
      if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
    if (RUN_CI_FORE) {
    
	 polygon(c(loca_xa, rev(loca_xa)), c( production_landing_1$Mean_length_in_Landing, rev(production_landing_2$Mean_length_in_Landing)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_2$Mean_length_in_Landing, rev(production_landing$Mean_length_in_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing$Mean_length_in_Landing, rev(production_landing_4$Mean_length_in_Landing)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_landing_4$Mean_length_in_Landing, rev(production_landing_5$Mean_length_in_Landing)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_landing_1$Mean_length_in_Landing,  type="l", lty=2) 
    	lines(loca_xa, production_landing_2$Mean_length_in_Landing ,  type="l", lty=3) 
    	lines(loca_xa, production_landing_4$Mean_length_in_Landing ,  type="l", lty=3) 
       lines(loca_xa, production_landing_5$Mean_length_in_Landing,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )  

	}  
  
  } 
	} 
#}

mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
dev.off()
} 



#
#
#
#
#-------------------------------------------------------------------------------
# lunghezza media per attrezzo - Discard
#-------------------------------------------------------------------------------    

if(nb_gears==1) {
columns_to_plot = c("Mean_length_in_Discard")
}  else {
columns_to_plot = c("Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""))
}

production_discard <- data.frame(production[, colnames(production) %in% columns_to_plot] )
colnames(production_discard) <- columns_to_plot

 if (RUN_CI_FORE) {
production_discard_1 <- data.frame(production_inf[, colnames(production_inf) %in% columns_to_plot]) 
production_discard_2 <- data.frame(production_inf_025[, colnames(production_inf_025) %in% columns_to_plot]) 
production_discard_4 <- data.frame(production_sup_075[, colnames(production_sup_075) %in% columns_to_plot]) 
production_discard_5 <- data.frame(production_sup[, colnames(production_sup) %in% columns_to_plot] )

if (length(columns_to_plot) == 1) {
colnames(production_discard_1) <-  "Mean_length_in_Discard"
colnames(production_discard_2) <-  "Mean_length_in_Discard"
colnames(production_discard_4) <-  "Mean_length_in_Discard"
colnames(production_discard_5) <-  "Mean_length_in_Discard"
}

}


       for (m in 1:nb_graphs) {
#         print(paste("Saving ML in Discard by gear in progress [", m,"]...", sep=""), quote=F)
         
if (m==1) {
nome_file <- paste(MEANLENGTHINDISCARDBYGEAR_graph,"_total.jpg", sep="")
} else {
nome_file <- paste(MEANLENGTHINDISCARDBYGEAR_graph,"_", m-1,".jpg", sep="")
}  
  
     
           if (m==1) {
           
           production_discard$Mean_length_in_Discard[which(is.na(production_discard$Mean_length_in_Discard))] <- 0
           
		   if (!RUN_CI_FORE) {
Ymin = min(production_discard$Mean_length_in_Discard,na.rm=TRUE)
Ymax = max(production_discard$Mean_length_in_Discard,na.rm=TRUE)
} else {
Ymin = min(production_discard$Mean_length_in_Discard,production_discard_1$Mean_length_in_Discard,production_discard_2$Mean_length_in_Discard,production_discard_4$Mean_length_in_Discard,production_discard_5$Mean_length_in_Discard, na.rm=TRUE)
Ymax = max(production_discard$Mean_length_in_Discard,production_discard_1$Mean_length_in_Discard,production_discard_2$Mean_length_in_Discard,production_discard_4$Mean_length_in_Discard,production_discard_5$Mean_length_in_Discard, na.rm=TRUE)
}

if (Ymin == Ymax & Ymin == 0) {
  Ymin=0
  Ymax=2
  Ylim=c(0,2)
} else {
  Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
}
		   
		   
		   
           if (!all( production_discard$Mean_length_in_Discard==0)) {
           
             jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)

           
           plot(loca_xa, production_discard$Mean_length_in_Discard,   type="b",  pch=20, main="Mean length in Discard (total)", xlab="Time_slice [year]", ylab="mm", ylim=Ylim,lwd=2, col="blue")             
             mtext( GLO$ThisIsVersion,side=4,outer=FALSE)
       
            if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
       
            if (RUN_CI_FORE) {
  
     polygon(c(loca_xa, rev(loca_xa)), c( production_discard_1$Mean_length_in_Discard, rev(production_discard_2$Mean_length_in_Discard)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_2$Mean_length_in_Discard, rev(production_discard$Mean_length_in_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard$Mean_length_in_Discard, rev(production_discard_4$Mean_length_in_Discard)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_discard_4$Mean_length_in_Discard, rev(production_discard_5$Mean_length_in_Discard)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_discard_1$Mean_length_in_Discard,  type="l", lty=2) 
    	lines(loca_xa, production_discard_2$Mean_length_in_Discard ,  type="l", lty=3) 
    	lines(loca_xa, production_discard_4$Mean_length_in_Discard ,  type="l", lty=3) 
       lines(loca_xa, production_discard_5$Mean_length_in_Discard,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )         
 
	}       
  
  dev.off()
						 }  

           }  else {                     # & 
           if( (nb_gears!=1) ) {
           					 
           main_ <- paste("Mean length in Discard for",as.character(FLEETSEGMENTS_names[m-1]))
           
              to_plot_lengths <- production_discard[,m]
              
              if (RUN_CI_FORE) {
							production_1  <- production_discard_1[,m]
							production_1[which(is.na(production_1))]  <- 0
							
							production_2  <- production_discard_2[,m]
							production_2[which(is.na(production_2))]  <- 0
							
							production_4  <- production_discard_4[,m]
							production_4[which(is.na(production_4))]  <- 0
							
              production_5  <- production_discard_5[,m]
              production_5[which(is.na(production_5))]  <- 0
              
              }
              } else {
							  to_plot_lengths <- production_discard[,1]
							 if (RUN_CI_FORE) {
							 production_1  <- production_discard_1[,1]
							 	production_1[which(is.na(production_1))]  <- 0
							 	
							 	 production_2  <- production_discard_2[,1]
							 	production_2[which(is.na(production_2))]  <- 0
							 	
							 	production_4  <- production_discard_4[,1]
							 	production_4[which(is.na(production_4))]  <- 0
							 	
              production_5  <- production_discard_5[,1]
              production_5[which(is.na(production_5))]  <- 0
              }
							}
            
						to_plot_lengths[which(is.na(to_plot_lengths))]  <- 0

						
		   if (!RUN_CI_FORE) {
Ymin = min(to_plot_lengths,na.rm=TRUE)
Ymax = max(to_plot_lengths,na.rm=TRUE)
} else {
Ymin = min(to_plot_lengths,production_1,production_2,production_4,production_5, na.rm=TRUE)
Ymax = max(to_plot_lengths,production_1,production_2,production_4,production_5, na.rm=TRUE)
}

if (Ymin == Ymax & Ymin == 0) {
  Ymin=0
  Ymax=2
  Ylim=c(0,2)
} else {
  Ylim=c(Ymin,(Ymax+((Ymax-Ymin)/3)) )
}
		   						
			
					 if (!all( to_plot_lengths==0)) {  
				 
		jpeg(file=nome_file, width=21, height=21, bg="white", units="cm",res=200)   
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 
   plot(loca_xa,to_plot_lengths,type="b",  pch=20, main=paste("Mean length in Discard for",as.character(FLEETSEGMENTS_names[m-1])), xlab = "Time slice [year]", ylab = "mm",lwd = 2,col = "blue",ylim=Ylim)  
            mtext(GLO$ThisIsVersion,side=4,outer=FALSE)
        
             if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
        
           if (RUN_CI_FORE) {
           
polygon(c(loca_xa, rev(loca_xa)), c( production_1, rev(production_2)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_2, rev(to_plot_lengths)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(to_plot_lengths, rev(production_4)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_4, rev(production_5)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_1,  type="l", lty=2) 
    	lines(loca_xa, production_2 ,  type="l", lty=3) 
    	lines(loca_xa, production_4 ,  type="l", lty=3) 
       lines(loca_xa, production_5,  type="l", lty=2)  	

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" ) 
  
  }   
  
  dev.off()
     
      }      
             }  

}

}
