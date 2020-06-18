# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





bmtsaveKobePlot <- function(all_the_years, path_biological, path_pressure) {

if (FALSE){
   all_the_years <- c(years, years.forecast)
   path_biological <- name_bioind
   path_pressure <- name_pressind
}

symb_kobe <- c(15:20)
col_kobe <- c("blue", "black", "grey", "dark green", "purple", "blue", "black", "grey", "dark green", "purple")
 plot_title <- "KOBE plot"
if (phase =="SIMULATION") {                                               
  plot_path <- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - KOBE plot.jpg", sep="")
  plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/", casestudy_name, " - KOBE plot ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
  plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/", casestudy_name, " - KOBE plot ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

# read F and B ratios
Ratios_bio = read.table(path_biological,sep=";",header=T)
Ratios_bio <- Ratios_bio[as.character(Ratios_bio$Year) != "ALL", ]

Ratios_press = read.table(path_pressure,sep=";",header=T)
Ratios_press <- Ratios_press[as.character(Ratios_press$Year) != "ALL", ]

nb_species <- length(BMT_SPECIES)
vec_spe <- BMT_SPECIES

# Preparation plot before plotting species ratios
# Ylim_max=max(Ratios$F_Fref)+1
Ylim_max=max(as.numeric(as.character(Ratios_press$Value[as.character(Ratios_press$Variable) == "F_Fref"])))+1
 Ylim_max <- ifelse(Ylim_max < 1,2, Ylim_max) 
Ylim_min=min(as.numeric(as.character(Ratios_press$Value[as.character(Ratios_press$Variable) == "F_Fref"])))
 Ylim_min <- ifelse(Ylim_min< -1,Ylim_min, -1) 
Ylim =c(Ylim_min,Ylim_max)    

Xlim_max = max(as.numeric(as.character(Ratios_bio$Value[as.character(Ratios_bio$Variable) == "SSB_SSBref"]))) 
Xlim_max <- ifelse(Xlim_max > 2, Xlim_max, 2)
#Xlim_min = min(as.numeric(as.character(Ratios_bio$Value[as.character(Ratios_bio$Variable) == "SSB_SSBref"])))
Xlim =c(0 ,Xlim_max) 


if (phase == "FORECAST" & !MEY_CALCULATION) {


CI_Population <- suppressWarnings(try(read.csv(name_bioind_CI, sep=";"))) 

if (class(CI_Population) != "try-error") {
  
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/", casestudy_name, " - RISK EVALUATION plot ", harvest_rule_id,".jpg", sep="")
  plot_title <-  "RISK EVALUATION plot" 
#  } else {
#  plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/", casestudy_name, " - KOBE plot ", harvest_rule_id,".jpg", sep="")
#  }
}

}

#windows()
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(5, 5, 3*length(BMT_SPECIES), 5), xpd=T)                                                                                                                                       #  , sub=plot_title_sub
plot(1,0,xlim=Xlim,ylim=Ylim,type="n",xlab="SSB/SSBref",ylab="F/Fref", cex.axis=1.5, cex.lab=1.5, cex.main=1.8, main=plot_title, axes=F)     # empty plot      , main=plot_title
 mtext( BMT_sw_version,side=4,outer=FALSE)
 
rect(0, 1, 1, Ylim_max, col="red")      #xleft, ybottom, xright, ytop
rect(1, Ylim_min, Xlim_max, 1, col="green") 
rect(0, Ylim_min, 1, 1, col="yellow")   #overfished (bottom-left)
rect(1, 1, Xlim_max, Ylim_max, col="orange")   #overfishing (top-right)   
axis(1,pos=Ylim_min)
axis(2,pos=0)
#abline(v=1,lwd=1.5)
#abline(h=0,lwd=1.5)
mtext(plot_title_sub, 3, line=4, cex=1.3)
#box()
 
 
# perc_SSB_greater_than_ref <- vector(mode="numeric", length=nb_species)
 legend_strings <- vector(mode="character", length=nb_species)

 perc_SSB_greater_than_ref_allyears <- data.frame(matrix(0, nrow=length(all_the_years), ncol=nb_species+1))
colnames(perc_SSB_greater_than_ref_allyears) <- c("year", BMT_SPECIES)
perc_SSB_greater_than_ref_allyears$year <- all_the_years
 
for (i in 1:nb_species) {

if (phase == "FORECAST" & !MEY_CALCULATION) {
     loca_INP <- get(paste("INP_", i, sep=""))                           # ADRIATIC - Population C.I. FORE HR5-SQ_2910_1 10 runs
     print( loca_INP$nruns )
     
     
     ALADYM_spe <<- i
source(paste(ALADYM_home, "/src/paths.r", sep=""))

CI_Population <- try(read.csv(paste(POPULATION_table_CI, " ", loca_INP$nruns, " runs.csv", sep=""), sep=";") )

if (class(CI_Population) != "try-error") {
# plot_title <- "RISK EVALUATION plot" 
annualSSB <- data.frame(matrix(0, nrow=loca_INP$nruns, ncol=length(all_the_years)))
 
 for (runi in c(1:loca_INP$nruns)) {
 annualSSB[runi,] <- CI_Population$SSB_exploited_pop[CI_Population$run==runi]
  }
} else {
CI_Population <- NULL
}
} else {
CI_Population <- NULL
} # IF phase

# selection of data of the stock i 
Ratios_temp_bio = Ratios_bio[as.character(Ratios_bio$Stock)==vec_spe[i] & as.numeric(as.character(Ratios_bio$Year)) %in% all_the_years,]
 Ratios_temp_press = Ratios_press[as.character(Ratios_press$Stock)==vec_spe[i] & as.numeric(as.character(Ratios_press$Year)) %in% all_the_years,]
#lines(Ratios_temp$B_Bref,Ratios_temp$F_Fref,col=7+i,lwd=3)
#points(Ratios_temp$B_Bref,Ratios_temp$F_Fref,col=7+i)
Ratios_bio_allyears = read.table(path_biological,sep=";",header=T)
  
if (!is.null(CI_Population)) {                                                                          
  SSB_ref <-  Ratios_bio_allyears$Value[as.character(Ratios_bio_allyears$Stock)==vec_spe[i] & as.character(Ratios_bio_allyears$Variable) == "SSBref"]
  annualSSB <- annualSSB/SSB_ref
  annualSSB_TF <- data.frame(matrix(0, nrow=loca_INP$nruns, ncol=length(all_the_years)))
  annualSSB_TF[] <- ifelse(annualSSB[] < 1, 1, 0)
  annualSSB_TF_year <- colSums(annualSSB_TF)
  annualSSB_TF_year_percent <- annualSSB_TF_year/loca_INP$nruns *100
  perc_SSB_greater_than_ref_allyears[,i+1]  <- annualSSB_TF_year_percent
  last_year <- annualSSB_TF_year_percent[length(annualSSB_TF_year_percent)]
# perc_SSB_greater_than_ref[i] <- last_year
 }
 
 
lines(as.numeric(as.character(Ratios_temp_bio$Value[as.character(Ratios_temp_bio$Variable) == "SSB_SSBref"])), as.numeric(as.character(Ratios_temp_press$Value[as.character(Ratios_temp_press$Variable) == "F_Fref"])),col=col_kobe[i],lwd=2.8)
points(as.numeric(as.character(Ratios_temp_bio$Value[as.character(Ratios_temp_bio$Variable) == "SSB_SSBref"])), as.numeric(as.character(Ratios_temp_press$Value[as.character(Ratios_temp_press$Variable) == "F_Fref"])),col=col_kobe[i], pch=symb_kobe[i], cex=1.3)

x_values <- as.numeric(as.character(Ratios_temp_bio$Value[as.character(Ratios_temp_bio$Variable) == "SSB_SSBref"]))
y_values <- as.numeric(as.character(Ratios_temp_press$Value[as.character(Ratios_temp_press$Variable) == "F_Fref"]))
str_start <- as.character(all_the_years[1])#substring(as.character(all_the_years[1]), 3,4)
str_end <- as.character(all_the_years[length(all_the_years)])#substring(as.character(all_the_years[length(all_the_years)]), 3,4)

text(x_values[1],y_values[1], labels=str_start,col=col_kobe[i], cex=0.8, font=2, pos=4)
text(x_values[length(all_the_years)],y_values[length(all_the_years)], labels=str_end,col=col_kobe[i], cex=0.8, font=2, pos=4)

  if (!is.null(CI_Population)) {                                                                          
  legend_strings[i] <- paste(" SSB < SSBref ", round(perc_SSB_greater_than_ref_allyears[nrow(perc_SSB_greater_than_ref_allyears), i+1],2) , "%", sep="")
  } else {
legend_strings[i] <- ""
}

}

#text(0.3,-1,"overfished",cex=1.2)
#text(1.3,-1,"healthy",cex=1.2)                  
#text(0.5,Ylim_max-0.2,"risky",cex=1.2) 
#text(1.5,Ylim_max-0.2,"overfishing",cex=1.2) 

legend_strings[] <- paste(BMT_SPECIES, legend_strings)                                                                                                     #,
legend("top", legend_strings, col=col_kobe[1:length(BMT_SPECIES)],             
cex=1.1, xpd = TRUE, pch=symb_kobe, bg="white", horiz=F,  bty="n", inset=c(0,-0.045*length(BMT_SPECIES)))  #, y.intersp=1.1   
          # xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
# tells R that I want a horizontal legend inset = c(x,y) tells R how to move
# the legend relative to the 'bottom' location bty = 'n' means that 'no' box
# will be drawn around it
dev.off()                                 

  if (!is.null(CI_Population)) {                                                                          
write.table(perc_SSB_greater_than_ref_allyears, file=paste(casestudy_path, "/",harvest_rule_id,"/", casestudy_name, " - Uncertainty ", harvest_rule_id,".csv", sep=""), sep=";", row.names=F)
 }
}

# saveKobePlot(years, name_bioind, name_pressind)
                        