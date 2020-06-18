# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveKobePlot <- function(all_the_years) {
if (!IN_BEMTOOL) {
BMT_SPECIES =  new_aldPopulation@scientific_name
# all_the_years = c(years, years_forecast )
}
symb_kobe <- c(1)
col_kobe <- c("blue", "black", "grey", "dark green", "purple", "blue", "black", "grey", "dark green", "purple")
 plot_title <- paste("KOBE plot -", BMT_SPECIES)

  plot_path <- KOBE_graph
  #plot_title <- paste("[", casestudy_name, "] KOBE plot SIM", sep="")
  plot_title_sub <- paste( "[years ", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")

  referencepoints_tbl <- read.csv(REFERENCEPOINTS_table, sep=";")
  mortalities_tbl <- read.csv(MORTALITY_table, sep=";")
  
  population_tbl <- read.csv(POPULATION_table, sep=";")

  if (length(FLEETSEGMENTS_names) == 1) {
  F_ <- as.numeric(as.character(mortalities_tbl[,14]) )
  } else {
    F_ <- as.numeric(as.character(mortalities_tbl[,(14+(length(FLEETSEGMENTS_names)*3))]) )
  }

  SSB_ <- as.numeric(as.character(population_tbl[,6] ))
  
if ((!is.na(is.na(referencepoints_tbl[3,2]) &  (referencepoints_tbl[3,2]!=0) )) | (!is.na(is.na(referencepoints_tbl[3,2]) &  (referencepoints_tbl[3,2]!=0) ))) {

if (!is.na(referencepoints_tbl[3,2])) {
   F_Fref <-  F_ / referencepoints_tbl[3,2]
} else {
   F_Fref <-    F_ / referencepoints_tbl[6,2]
}

if (!is.na(referencepoints_tbl[3,4])) {
  SSBref    <-  referencepoints_tbl[3,4]*referencepoints_tbl[3,6]
} else {
  SSBref    <- referencepoints_tbl[6,4]*referencepoints_tbl[6,6]
}

      SSB_SSBref <-   SSB_ / SSBref


# read F and B ratios
#Ratios_bio = read.table(path_biological,sep=";",header=T)
#Ratios_bio <- Ratios_bio[as.character(Ratios_bio$Year) != "ALL", ]
#
#Ratios_press = read.table(path_pressure,sep=";",header=T)
#Ratios_press <- Ratios_press[as.character(Ratios_press$Year) != "ALL", ]
#


nb_species <- length(BMT_SPECIES)
vec_spe <- BMT_SPECIES

# Preparation plot before plotting species ratios
# Ylim_max=max(Ratios$F_Fref)+1
Ylim_max=max(F_Fref, na.rm=T)+1
 Ylim_max <- ifelse(Ylim_max < 1,2, Ylim_max) 
Ylim_min=min(F_Fref, na.rm=T)
 Ylim_min <- ifelse(Ylim_min< -1,Ylim_min, -1) 
Ylim =c(Ylim_min,Ylim_max)    

Xlim_max = max(SSB_SSBref, na.rm=T) 
Xlim_max <- ifelse(Xlim_max > 2, Xlim_max, 2)
#Xlim_min = min(as.numeric(as.character(Ratios_bio$Value[as.character(Ratios_bio$Variable) == "SSB_SSBref"])))
Xlim =c(0 ,Xlim_max) 


# read population table
CI_Population <- suppressWarnings(try(read.table(paste(POPULATION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";") ))



if (class(CI_Population) != "try-error") {
     plot_title <- paste("RISK EVALUATION plot -", BMT_SPECIES)
#  } else {
#  plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/", casestudy_name, " - KOBE plot ", harvest_rule_id,".jpg", sep="")
#  }
}


 perc_SSB_greater_than_ref_allyears <- data.frame(matrix(0, nrow=length(all_the_years), ncol=length(BMT_SPECIES)+1))
colnames(perc_SSB_greater_than_ref_allyears) <- c("year", BMT_SPECIES)
perc_SSB_greater_than_ref_allyears$year <- all_the_years

CI_Population <- try(read.csv(paste(POPULATION_table_CI, " ", INP$nruns, " runs.csv", sep=""), sep=";") )

if (class(CI_Population) != "try-error") {
# plot_title <- "RISK EVALUATION plot" 
annualSSB <- data.frame(matrix(0, nrow=INP$nruns, ncol=length(all_the_years)))
 
 for (runi in c(1:INP$nruns)) {
 annualSSB[runi,] <- CI_Population$SSB_exploited_pop[CI_Population$run==runi]
  }
} else {
CI_Population <- NULL
}

  
if (!is.null(CI_Population)) {                                                                          

  annualSSB <- annualSSB/SSBref
  annualSSB_TF <- data.frame(matrix(0, nrow=INP$nruns, ncol=length(all_the_years)))
  annualSSB_TF[] <- ifelse(annualSSB[] < 1, 1, 0)
  annualSSB_TF_year <- colSums(annualSSB_TF)
  annualSSB_TF_year_percent <- annualSSB_TF_year/INP$nruns *100
  perc_SSB_greater_than_ref_allyears[,2]  <- annualSSB_TF_year_percent 
  last_year <- annualSSB_TF_year_percent[length(annualSSB_TF_year_percent)]
# perc_SSB_greater_than_ref[i] <- last_year
                                                                          
  legend_string <- paste(" SSB < SSBref ", round(perc_SSB_greater_than_ref_allyears[nrow(perc_SSB_greater_than_ref_allyears), 2],2) , "%", sep="")

 }  else {
  legend_string <- ""
 }



#windows()
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(5, 5, 6, 10.5), xpd=T)                                                                                                                                       #  , sub=plot_title_sub
plot(1,0,xlim=Xlim,ylim=Ylim,type="n",xlab="SSB/SSBref",ylab="F/Fref", cex.axis=1.5, cex.lab=1.5, cex.main=1.8, main=plot_title, axes=F)     # empty plot      , main=plot_title

rect(0, 1, 1, Ylim_max, col="red")      #xleft, ybottom, xright, ytop
rect(1, Ylim_min, Xlim_max, 1, col="green") 
rect(0, Ylim_min, 1, 1, col="yellow")   #overfished (bottom-left)
rect(1, 1, Xlim_max, Ylim_max, col="orange")   #overfishing (top-right)   
axis(1,pos=Ylim_min)
axis(2,pos=0)
#abline(v=1,lwd=1.5)
#abline(h=0,lwd=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)
#box()
 
for (i in 1:nb_species) {
lines(SSB_SSBref, F_Fref,col=col_kobe[i],lwd=2.8)
points(SSB_SSBref, F_Fref,col=col_kobe[i], pch=symb_kobe[i], cex=1.3)

x_values <- SSB_SSBref
y_values <- F_Fref
str_start <- as.character(all_the_years[1]) #substring(as.character(all_the_years[1]), 3,4)
str_end <- as.character(all_the_years[length(all_the_years)]) #substring(as.character(all_the_years[length(all_the_years)]), 3,4)

text(x_values[1],y_values[1], labels=str_start,col=col_kobe[i], cex=0.8, font=2, pos=4)
text(x_values[length(all_the_years)],y_values[length(all_the_years)], labels=str_end,col=col_kobe[i], cex=0.8, font=2, pos=4)
}

#text(0.3,-1,"overfished",cex=1.2)
#text(1.3,-1,"healthy",cex=1.2)                  
#text(0.5,Ylim_max-0.2,"risky",cex=1.2) 
#text(1.5,Ylim_max-0.2,"overfishing",cex=1.2)                                                                                                       #,
# legend("topright", substring(BMT_SPECIES,1,17), col=col_kobe[1:length(BMT_SPECIES)], cex=1.3, pch=symb_kobe, bg="white", horiz=F, inset=c(-0.4,0),  bty="n", y.intersp=1.1)
 legend("top", legend_string, col=col_kobe[1:length(BMT_SPECIES)],             
cex=1.1, xpd = TRUE, bg="white", horiz=F,  bty="n", inset=c(0,-0.045*length(BMT_SPECIES)))  #, y.intersp=1.1   

dev.off()

  if (!is.null(CI_Population)) {                                                                          
write.table(perc_SSB_greater_than_ref_allyears, file=Uncertainty_table, sep=";", row.names=F)
 }
   
}                              
}

# saveKobePlot(years, name_bioind, name_pressind)
                        