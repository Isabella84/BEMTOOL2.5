# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



PlotYear <- function() {

#source("src/utility.r", local=TRUE)
gears = FLEETSEGMENTS_names
#------------------------------------------------------------------------------
#------------------------------------- IMAGES:---------------------------------- 
# Indicators.jpg
# Biomasses.jpg
# Production.jpg
#------------------------------------------------------------------------------

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

 
loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}
# **************************************************************************************************************************************	 
# grafico 1 : Biomassa popolazione sfruttata
# **************************************************************************************************************************************	 
# Biomasses.jpg  



  jpeg(file=BIOMASSES_graph_biomass_expl, width=21, height=21, bg="white", units="cm",res=200)
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)

	if (!RUN_CI_FORE) { 
	Ymin = min(population$Total_biomass_exploited_pop, na.rm=T)
	Ymax = max(population$Total_biomass_exploited_pop, na.rm=T) 
	} else {
Ymin = min(population$Total_biomass_exploited_pop, population_inf$Total_biomass_exploited_pop, population_sup$Total_biomass_exploited_pop, population_inf_025$Total_biomass_exploited_pop, population_sup_075$Total_biomass_exploited_pop, na.rm=T)
Ymax =  max(population$Total_biomass_exploited_pop, population_inf$Total_biomass_exploited_pop, population_sup$Total_biomass_exploited_pop, population_inf_025$Total_biomass_exploited_pop, population_sup_075$Total_biomass_exploited_pop, na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)

  plot(loca_xa, population$Total_biomass_exploited_pop ,  type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Biomass of exploited population"), xlab="Time_slice [year]", ylab="tons", lwd=2, col="red",ylim=Ylim) 
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE)

       if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1, Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 

	if (RUN_CI_FORE) {
	
	 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Total_biomass_exploited_pop, rev(population_inf_025$Total_biomass_exploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Total_biomass_exploited_pop, rev(population$Total_biomass_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Total_biomass_exploited_pop, rev(population_sup_075$Total_biomass_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Total_biomass_exploited_pop, rev(population_sup$Total_biomass_exploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Total_biomass_exploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Total_biomass_exploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Total_biomass_exploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Total_biomass_exploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
	  
	
#  lines(loca_xa, population_inf$Total_biomass_exploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$Total_biomass_exploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}  
	
	dev.off()
  
# **************************************************************************************************************************************	   
# grafico 2 : Biomassa popolazione non sfruttata
# **************************************************************************************************************************************	 


	if (!RUN_CI_FORE) { 
	Ymin = min(population$Total_biomass_unexploited_pop, na.rm=T)
	Ymax = max(population$Total_biomass_unexploited_pop, na.rm=T) 
	} else {
Ymin = min(population$Total_biomass_unexploited_pop, population_inf$Total_biomass_unexploited_pop, population_sup$Total_biomass_unexploited_pop, population_inf_025$Total_biomass_unexploited_pop, population_sup_075$Total_biomass_unexploited_pop, na.rm=T)
Ymax =  max(population$Total_biomass_unexploited_pop, population_inf$Total_biomass_unexploited_pop, population_sup$Total_biomass_unexploited_pop, population_inf_025$Total_biomass_unexploited_pop, population_sup_075$Total_biomass_unexploited_pop, na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)



     
       jpeg(file=BIOMASSES_graph_biomass_unexpl, width=21, height=21, bg="white", units="cm",res=200)
       par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 # plot(loca_xa, meanWequals(SRO$UBiomass   , GLO$L_number + 1, INP$Time_slice), type="l", main="Biomass of unexploited  population", xlab="Time_slice [year]", ylab="[t]",, col="green",lwd=2,ylim=Ylim)   Total_biomass_unexploited_pop
 plot(loca_xa, population$Total_biomass_unexploited_pop, type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Biomass of unexploited  population"), xlab="Time_slice [year]", ylab="tons", col="green",lwd=2,ylim=Ylim)  
    mtext( GLO$ThisIsVersion,side=4,outer=FALSE)    

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 

	if (RUN_CI_FORE) {
	
		 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$Total_biomass_unexploited_pop, rev(population_inf_025$Total_biomass_unexploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$Total_biomass_unexploited_pop, rev(population$Total_biomass_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$Total_biomass_unexploited_pop, rev(population_sup_075$Total_biomass_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$Total_biomass_unexploited_pop, rev(population_sup$Total_biomass_unexploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$Total_biomass_unexploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$Total_biomass_unexploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$Total_biomass_unexploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$Total_biomass_unexploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","green","black","black"), bty="n" )

	
	
#  lines(loca_xa, population_inf$Total_biomass_unexploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$Total_biomass_unexploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("green","black","black"), bty="n" )
	}  

  dev.off()
  

#Ymin = min(population$SSB_exploited_pop)
#Ylim = c(Ymin, max(SRO$USSBiomass)+ (max(SRO$USSBiomass)-Ymin)/5)
# **************************************************************************************************************************************	 
  # grafico 3: SSB popolazione sfruttata
# **************************************************************************************************************************************	 


      jpeg(file=BIOMASSES_graph_SSB_expl, width=21, height=21, bg="white", units="cm",res=200)    
        par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
	if (!RUN_CI_FORE) { 
	Ymin = min(population$SSB_exploited_pop, na.rm=T)
	Ymax = max(population$SSB_exploited_pop, na.rm=T) 
	} else {
Ymin = min(population$SSB_exploited_pop, population_inf$SSB_exploited_pop, population_sup$SSB_exploited_pop, population_inf_025$SSB_exploited_pop, population_sup_075$SSB_exploited_pop, na.rm=T)
Ymax =  max(population$SSB_exploited_pop, population_inf$SSB_exploited_pop, population_sup$SSB_exploited_pop, population_inf_025$SSB_exploited_pop, population_sup_075$SSB_exploited_pop, na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)
  
  plot(loca_xa, population$SSB_exploited_pop,       type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- SSB of exploited population"), xlab="Time_slice [year]", ylab="tons", col="red",lwd=2, lty=2,ylim=Ylim)
    mtext( GLO$ThisIsVersion,side=4,outer=FALSE)    
    
     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 	
  
  
  if (RUN_CI_FORE) {
  
    
		 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$SSB_exploited_pop, rev(population_inf_025$SSB_exploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$SSB_exploited_pop, rev(population$SSB_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$SSB_exploited_pop, rev(population_sup_075$SSB_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$SSB_exploited_pop, rev(population_sup$SSB_exploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$SSB_exploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$SSB_exploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$SSB_exploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$SSB_exploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )


#  lines(loca_xa, population_inf$SSB_exploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$SSB_exploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(2,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}  
	
	dev.off()
	
	
# **************************************************************************************************************************************	 
  # grafico 4 : SSB Biomassa popolazione non sfruttata
# **************************************************************************************************************************************	 


       jpeg(file=BIOMASSES_graph_SSB_unexpl, width=21, height=21, bg="white", units="cm",res=200)
              par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
	if (!RUN_CI_FORE) { 
	Ymin = min(population$SSB_unexploited_pop, na.rm=T)
	Ymax = max(population$SSB_unexploited_pop, na.rm=T) 
	} else {
Ymin = min(population$SSB_unexploited_pop, population_inf$SSB_unexploited_pop, population_sup$SSB_unexploited_pop, population_inf_025$SSB_unexploited_pop, population_sup_075$SSB_unexploited_pop, na.rm=T)
Ymax =  max(population$SSB_unexploited_pop, population_inf$SSB_unexploited_pop, population_sup$SSB_unexploited_pop, population_inf_025$SSB_unexploited_pop, population_sup_075$SSB_unexploited_pop, na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)
  
  plot(loca_xa, population$SSB_unexploited_pop, type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- SSB of unexploited population"), xlab="Time_slice [year]", ylab="tons", col="green",lwd=2,lty=2,ylim=Ylim)
  #plot(loca_xa, meanWequals(SRO$USSBiomass   , GLO$L_number + 1, INP$Time_slice),       type="l", main="SSB of unexploited  population    ", xlab="Time_slice [year]", ylab="[t]", col="green",lwd=2,lty=2,ylim=Ylim)
     mtext( GLO$ThisIsVersion,side=4,outer=FALSE)    
  
       if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  	if (RUN_CI_FORE) {
  			 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$SSB_unexploited_pop, rev(population_inf_025$SSB_unexploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$SSB_unexploited_pop, rev(population$SSB_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$SSB_unexploited_pop, rev(population_sup_075$SSB_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$SSB_unexploited_pop, rev(population_sup$SSB_unexploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$SSB_unexploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$SSB_unexploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$SSB_unexploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$SSB_unexploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,2,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","green","black","black"), bty="n" )

  	
#  lines(loca_xa, population_inf$SSB_unexploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$SSB_unexploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(2,2,3),legend=c("median", "0.05", "0.95"),col=c("green","black","black"), bty="n" )
	}    


  dev.off()
		 
      if (!RUN_CI_FORE) {
#x=(BAS$MUSS_Number+BAS$FUSS_Number)/1000 
#Ylim = c(Ymin, max(x[is.finite(x)])+ (max(x[is.finite(x)])-Ymin)/5)
# **************************************************************************************************************************************	   
  # grafico 5 : SS in numbers exploited    
# **************************************************************************************************************************************	 
      jpeg(file=BIOMASSES_graph_SSnumber_expl, width=21, height=21, bg="white", units="cm",res=200)
      par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
	if (!RUN_CI_FORE) { 
	Ymin = min(population$SS_NUMBERS_exploited_pop, na.rm=T)
	Ymax = max(population$SS_NUMBERS_exploited_pop, na.rm=T) 
	} else {
Ymin = min(population$SS_NUMBERS_exploited_pop, population_inf$SS_NUMBERS_exploited_pop, population_sup$SS_NUMBERS_exploited_pop, population_inf_025$SS_NUMBERS_exploited_pop, population_sup_075$SS_NUMBERS_exploited_pop, na.rm=T)
Ymax =  max(population$SS_NUMBERS_exploited_pop, population_inf$SS_NUMBERS_exploited_pop, population_sup$SS_NUMBERS_exploited_pop, population_inf_025$SS_NUMBERS_exploited_pop, population_sup_075$SS_NUMBERS_exploited_pop, na.rm=T)
	}
	
		Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)

  plot(loca_xa, population$SS_NUMBERS_exploited_pop,       type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- SS in number of exploited  population"), xlab="Time_slice [year]", ylab="thousands", col="red",lwd=2,lty=3,ylim=Ylim)
   mtext( GLO$ThisIsVersion,side=4,outer=FALSE)    
 
      if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
	if (RUN_CI_FORE) {
	
	  			 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$SS_NUMBERS_exploited_pop, rev(population_inf_025$SS_NUMBERS_exploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$SS_NUMBERS_exploited_pop, rev(population$SS_NUMBERS_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$SS_NUMBERS_exploited_pop, rev(population_sup_075$SS_NUMBERS_exploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$SS_NUMBERS_exploited_pop, rev(population_sup$SS_NUMBERS_exploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$SS_NUMBERS_exploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$SS_NUMBERS_exploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$SS_NUMBERS_exploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$SS_NUMBERS_exploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,3,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )

  	
	
#  lines(loca_xa, population_inf$SS_NUMBERS_exploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$SS_NUMBERS_exploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(3,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}  

    dev.off()
 }   
    
# **************************************************************************************************************************************	 
# grafico 6 : SS in numbers unexploited
# **************************************************************************************************************************************	 
 	
  if (!RUN_CI_FORE) {
  
     jpeg(file=BIOMASSES_graph_SSnumber_unexpl, width=21, height=21, bg="white", units="cm",res=200)
 	     par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
	if (!RUN_CI_FORE) { 
	Ymin = min(population$SS_NUMBERS_unexploited_pop, na.rm=T)
	Ymax = max(population$SS_NUMBERS_unexploited_pop, na.rm=T) 
	} else {
Ymin = min(population$SS_NUMBERS_unexploited_pop, population_inf$SS_NUMBERS_unexploited_pop, population_sup$SS_NUMBERS_unexploited_pop, population_inf_025$SS_NUMBERS_unexploited_pop, population_sup_075$SS_NUMBERS_unexploited_pop, na.rm=T)
Ymax =  max(population$SS_NUMBERS_unexploited_pop, population_inf$SS_NUMBERS_unexploited_pop, population_sup$SS_NUMBERS_unexploited_pop, population_inf_025$SS_NUMBERS_unexploited_pop, population_sup_075$SS_NUMBERS_unexploited_pop, na.rm=T)
	}
	
	Ylim = c(Ymin, Ymax + (Ymax-Ymin)/3)
	
plot(loca_xa, population$SS_NUMBERS_unexploited_pop,type="b",   pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- SS in number of unexploited  population"), xlab="Time_slice [year]", ylab="thousands", col="green",lwd=2,lty=3,ylim=Ylim)
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE)

     if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1,cex=0.9) 
    } 

	if (RUN_CI_FORE) {

	  			 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$SS_NUMBERS_unexploited_pop, rev(population_inf_025$SS_NUMBERS_unexploited_pop)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$SS_NUMBERS_unexploited_pop, rev(population$SS_NUMBERS_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$SS_NUMBERS_unexploited_pop, rev(population_sup_075$SS_NUMBERS_unexploited_pop)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$SS_NUMBERS_unexploited_pop, rev(population_sup$SS_NUMBERS_unexploited_pop)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$SS_NUMBERS_unexploited_pop,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$SS_NUMBERS_unexploited_pop ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$SS_NUMBERS_unexploited_pop ,  type="l", lty=3) 
       lines(loca_xa, population_sup$SS_NUMBERS_unexploited_pop,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,3,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","green","black","black"), bty="n" )

  		
	
#  lines(loca_xa, population_inf$SS_NUMBERS_unexploited_pop ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$SS_NUMBERS_unexploited_pop ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(3,2,3),legend=c("median", "0.05", "0.95"),col=c("green","black","black"), bty="n" )
	}    
         mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
  dev.off()
  }

# **************************************************************************************************************************************	 
 # Production.jpg
 # **************************************************************************************************************************************	 


#	if (!RUN_CI_FORE) { 
#Ymax = max(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,na.rm=T)
#Ymin = min(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,na.rm=T)
#	} else {
#Ymax = max(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,production_inf$Biological_Production,production_inf$Total_Yield,production_inf$Natural_death_biomass,production_sup$Biological_Production,production_sup$Total_Yield,production_sup$Natural_death_biomass, na.rm=T)
#Ymin = min(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,production_inf$Biological_Production,production_inf$Total_Yield,production_inf$Natural_death_biomass,production_sup$Biological_Production,production_sup$Total_Yield,production_sup$Natural_death_biomass, na.rm=T)
#	} 
#Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/5)

  jpeg(file=PRODUCTION_graph_bio_production, width=21, height=21, bg="white", units="cm",res=200)
    par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
  #par(mfrow=c(2,2))

#Ymax = max(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,na.rm=T)
#Ymin = min(production$Biological_Production,production$Total_Yield,production$Natural_death_biomass,na.rm=T)
#Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/5)

    # legenda
 # plot(0,0,type="n",main="",axes=FALSE,xlab="",ylab="")
 
 
 	if (!RUN_CI_FORE) { 
	Ymin = min(production$Biological_Production, na.rm=T)
	Ymax = max(production$Biological_Production, na.rm=T) 
	} else {
Ymin = min(production$Biological_Production, production_inf$Biological_Production, production_sup$Biological_Production, production_inf_025$Biological_Production, production_sup_075$Biological_Production, na.rm=T)
Ymax =  max(production$Biological_Production, production_inf$Biological_Production, production_sup$Biological_Production, production_inf_025$Biological_Production, production_sup_075$Biological_Production, na.rm=T)
	}

Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/3)
 
  
  # grafico 7  : Biological Production 
  plot(loca_xa, production$Biological_Production, type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Biological Production"), xlab="Time_slice [year]", ylab="tons", lwd = 2,col="red",ylim=Ylim)
  mtext( GLO$ThisIsVersion,side=4,outer=FALSE)
 
      if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
 
 	if (RUN_CI_FORE) {
 	
 	  
	  			 polygon(c(loca_xa, rev(loca_xa)), c( production_inf$Biological_Production, rev(production_inf_025$Biological_Production)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_inf_025$Biological_Production, rev(production$Biological_Production)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production$Biological_Production, rev(production_sup_075$Biological_Production)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_sup_075$Biological_Production, rev(production_sup$Biological_Production)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_inf$Biological_Production,  type="l", lty=2) 
    	lines(loca_xa, production_inf_025$Biological_Production ,  type="l", lty=3) 
    	lines(loca_xa, production_sup_075$Biological_Production ,  type="l", lty=3) 
       lines(loca_xa, production_sup$Biological_Production,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
 	
#    lines(loca_xa, production_inf$Biological_Production ,  type="l", lty=2) 
#	  lines(loca_xa, production_sup$Biological_Production ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}     
dev.off()



  jpeg(file=PRODUCTION_graph_death_biomass, width=21, height=21, bg="white", units="cm",res=200)
    par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
 	if (!RUN_CI_FORE) { 
	Ymin = min(production$Natural_death_biomass, na.rm=T)
	Ymax = max(production$Natural_death_biomass, na.rm=T) 
	} else {
Ymin = min(production$Natural_death_biomass, production_inf$Natural_death_biomass, production_sup$Natural_death_biomass, production_inf_025$Natural_death_biomass, production_sup_075$Natural_death_biomass, na.rm=T)
Ymax =  max(production$Natural_death_biomass, production_inf$Natural_death_biomass, production_sup$Natural_death_biomass, production_inf_025$Natural_death_biomass, production_sup_075$Natural_death_biomass, na.rm=T)
	}

Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/3)
   
 # grafico 8 : Death_Biomass
  plot(loca_xa, production$Natural_death_biomass,   type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Natural death biomass"), xlab="Time_slice [year]", ylab="tons",lwd = 2,col="red",ylim=Ylim)
    mtext(GLO$ThisIsVersion,side=4,outer=FALSE)
  
       if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
  if (RUN_CI_FORE) {
  
   	  			 polygon(c(loca_xa, rev(loca_xa)), c( production_inf$Natural_death_biomass, rev(production_inf_025$Natural_death_biomass)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_inf_025$Natural_death_biomass, rev(production$Natural_death_biomass)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production$Natural_death_biomass, rev(production_sup_075$Natural_death_biomass)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_sup_075$Natural_death_biomass, rev(production_sup$Natural_death_biomass)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_inf$Natural_death_biomass,  type="l", lty=2) 
    	lines(loca_xa, production_inf_025$Natural_death_biomass ,  type="l", lty=3) 
    	lines(loca_xa, production_sup_075$Natural_death_biomass ,  type="l", lty=3) 
       lines(loca_xa, production_sup$Natural_death_biomass,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
  
#    lines(loca_xa, production_inf$Natural_death_biomass ,  type="l", lty=2) 
#	  lines(loca_xa, production_sup$Natural_death_biomass ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}     


  dev.off()
  
  

 # if (!RUN_CI_FORE) { 
#	Ymin = min(production$Total_Yield, na.rm=T)
#	Ymax = max(production$Total_Yield, na.rm=T) 
#	} else {
#Ymin = min(production$Total_Yield, production_inf$Total_Yield, production_sup$Total_Yield, production_inf_025$Total_Yield, production_sup_075$Total_Yield, na.rm=T)
#Ymax =  max(production$Total_Yield, production_inf$Total_Yield, production_sup$Total_Yield, production_inf_025$Total_Yield, production_sup_075$Total_Yield, na.rm=T)
#	}
#
#Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/3)
#   
#
#
#  # grafico 9 : Yield
#  plot(loca_xa, production$Total_Yield,   type="b",  pch=20, main="Total Yield", xlab="Time_slice [year]", ylab="[t]",lwd = 2, col="red",ylim=Ylim)
#  mtext( GLO$ThisIsVersion,cex=0.7,side=4,outer=FALSE)
# 
#      if (INP$Year_simulation != length(years)) {
#    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
#    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.55) 
#    } 
# 
#    if (RUN_CI_FORE) {
#    
#       	  			 polygon(c(loca_xa, rev(loca_xa)), c( production_inf$Total_Yield, rev(production_inf_025$Total_Yield)), col = alpha("black", 0.05), border = NA)
#polygon(c(loca_xa, rev(loca_xa)), c(production_inf_025$Total_Yield, rev(production$Total_Yield)), col = alpha("black", 0.15), border = NA)
#polygon(c(loca_xa, rev(loca_xa)), c(production$Total_Yield, rev(production_sup_075$Total_Yield)), col = alpha("black", 0.15), border = NA)
#polygon(c(loca_xa, rev(loca_xa)), c(production_sup_075$Total_Yield, rev(production_sup$Total_Yield)), col = alpha("black", 0.05), border = NA)
#    
#    lines(loca_xa, production_inf$Total_Yield,  type="l", lty=2) 
#    	lines(loca_xa, production_inf_025$Total_Yield ,  type="l", lty=3) 
#    	lines(loca_xa, production_sup_075$Total_Yield ,  type="l", lty=3) 
#       lines(loca_xa, production_sup$Total_Yield,  type="l", lty=2) 
#
#	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
#    
##    lines(loca_xa, production_inf$Total_Yield ,  type="l", lty=2) 
##	  lines(loca_xa, production_sup$Total_Yield ,  type="l", lty=3) 
##	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
#	}    
#	
#  dev.off()
# 
  
# Indicators.jpg  

#Ylim=c(0,1) 

  # grafico 10 :    ESSB/USSB  

  
  jpeg(file=INDICATORS_graph_SPR, width=21, height=21, bg="white", units="cm",res=200)
 par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
  # legenda
  # plot(0,0,type="n",main="",axes=FALSE,xlab="",ylab="")
  
	if (!RUN_CI_FORE) { 
	Ymin = min(population$ESSBratioUSSB, na.rm=T)
	Ymax = max(population$ESSBratioUSSB, na.rm=T) 
	} else {
Ymin = min(population$ESSBratioUSSB, population_inf$ESSBratioUSSB, population_sup$ESSBratioUSSB, population_inf_025$ESSBratioUSSB, population_sup_075$ESSBratioUSSB, na.rm=T)
Ymax =  max(population$ESSBratioUSSB, population_inf$ESSBratioUSSB, population_sup$ESSBratioUSSB, population_inf_025$ESSBratioUSSB, population_sup_075$ESSBratioUSSB, na.rm=T)
	}
Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/3)
      
  
  plot(loca_xa, population$ESSBratioUSSB,       type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Spawning Potential Ratio"), xlab="Time_slice [year]", ylab="ESSB/USSB", lwd = 2,  col="red",ylim=Ylim)
   mtext( GLO$ThisIsVersion,side=4,outer=FALSE) 
 
      if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
    if (RUN_CI_FORE) {
    
    	  			 polygon(c(loca_xa, rev(loca_xa)), c( population_inf$ESSBratioUSSB, rev(population_inf_025$ESSBratioUSSB)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_inf_025$ESSBratioUSSB, rev(population$ESSBratioUSSB)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population$ESSBratioUSSB, rev(population_sup_075$ESSBratioUSSB)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(population_sup_075$ESSBratioUSSB, rev(population_sup$ESSBratioUSSB)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, population_inf$ESSBratioUSSB,  type="l", lty=2) 
    	lines(loca_xa, population_inf_025$ESSBratioUSSB ,  type="l", lty=3) 
    	lines(loca_xa, population_sup_075$ESSBratioUSSB ,  type="l", lty=3) 
       lines(loca_xa, population_sup$ESSBratioUSSB,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,3,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )

    
#    lines(loca_xa, population_inf$ESSBratioUSSB ,  type="l", lty=2) 
#	  lines(loca_xa, population_sup$ESSBratioUSSB ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}    
	
dev.off()



  jpeg(file=INDICATORS_graph_DR, width=21, height=21, bg="white", units="cm",res=200)
	   par(mar=c(5, 5, 5, 5))  # c(bottom, left, top, right)
#	Discard_ratio <- production$Total_Discard / production$Total_Landing

#production$Discard_ratio  <- production$Total_Discard / production$Total_Landing
#if (RUN_CI_FORE) {
#production_inf$Discard_ratio  <- production_inf$Total_Discard / production_inf$Total_Landing
#production_sup$Discard_ratio  <- production_sup$Total_Discard / production_sup$Total_Landing
#production_inf_025$Discard_ratio <- production_inf_025$Total_Discard / production_inf_025$Total_Landing
#production_sup_075$Discard_ratio <- production_sup_075$Total_Discard / production_sup_075$Total_Landing
#}

if (!RUN_CI_FORE) { 
Ymax = max( production$Discard_ratio ,na.rm=T)
Ymin = min(  production$Discard_ratio ,na.rm=T)
	} else {
Ymax = max( production$Discard_ratio , production_inf$Discard_ratio ,production_sup$Discard_ratio ,  production_inf_025$Discard_ratio ,production_sup_075$Discard_ratio , na.rm=T)
Ymin = min( production$Discard_ratio , production_inf$Discard_ratio ,production_sup$Discard_ratio ,  production_inf_025$Discard_ratio ,production_sup_075$Discard_ratio , na.rm=T)
	} 
Ylim = c(Ymin, Ymax+ (Ymax-Ymin)/5)


if (!all( is.na( production$Discard_ratio  ) ) ) {

  plot(loca_xa, production$Discard_ratio,  type="b",  pch=20, main=paste(BMT_SPECIES[ALADYM_spe], "- Total discard ratio"), xlab="Time_slice [year]", ylab="Dis/Lan", lwd = 2,col="red",ylim=Ylim)
  mtext(GLO$ThisIsVersion,side=4,outer=FALSE)
 
      if (INP$Year_simulation != length(years)) {
    lines(rep(years[length(years)],2), Ylim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,Ylim[2], label="FORECAST", pos=1, cex=0.9) 
    } 
  
   if (RUN_CI_FORE) {
   
polygon(c(loca_xa, rev(loca_xa)), c( production_inf$Discard_ratio, rev(production_inf_025$Discard_ratio)), col = alpha("black", 0.05), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_inf_025$Discard_ratio, rev(production$Discard_ratio)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production$Discard_ratio, rev(production_sup_075$Discard_ratio)), col = alpha("black", 0.15), border = NA)
polygon(c(loca_xa, rev(loca_xa)), c(production_sup_075$Discard_ratio, rev(production_sup$Discard_ratio)), col = alpha("black", 0.05), border = NA)
    
    lines(loca_xa, production_inf$Discard_ratio,  type="l", lty=2) 
    	lines(loca_xa, production_inf_025$Discard_ratio ,  type="l", lty=3) 
    	lines(loca_xa, production_sup_075$Discard_ratio ,  type="l", lty=3) 
       lines(loca_xa, production_sup$Discard_ratio,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","red","black","black"), bty="n" )
   
#    lines(loca_xa, (production_inf$Total_Discard / production_inf$Total_Landing)  ,  type="l", lty=2) 
#	  lines(loca_xa, (production_sup$Total_Discard / production_sup$Total_Landing)  ,  type="l", lty=3) 
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c("median", "0.05", "0.95"),col=c("red","black","black"), bty="n" )
	}    
 
 }  else {
           plot(1,1,type="n",main="Total discard ratio", xlab="Time_slice [year]", ylab="Dis/Lan")
            text(1,1, "Not Available", cex=1.5,side=4,outer=FALSE)
             mtext( GLO$ThisIsVersion,side=4,outer=FALSE)
         
             } 
 
  dev.off()

}
