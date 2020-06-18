# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#---------------------------------
# Short term forecast
#---------------------------------
# BEMTOOL STF settings:

# BEMTOOL MTF settings:
#dir_temp <- getwd()
#setwd(STF_home)
#dir.create("Results_STF")
#setwd(paste(STF_home, "/Results_STF", sep=""))
#dir.create(BMT_SPECIES[STF_spe])
#setwd(dir_temp)
#
#STF_home_species_res <<- paste(STF_home, "/Results_STF/", BMT_SPECIES[STF_spe], sep="")   

year_start <- casestudy.startsimulation     # BMT setting
year_end <- casestudy.endsimulation        # BMT setting

#nb_fleets <- length(BMT_FLEETSEGMENTS)
avg_y <-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO,".S", MTF_spe, sep=""),5]))      # casestudy.HR4.S1
#SRR <- as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO,".S", MTF_spe, sep=""),4])      # casestudy.HR4.S1  
# if SRR="N" the "geomean" model (Constant Recruitment equal to the historical geometric mean recruitment) will be used 
#steepness = 0.75   
#model <-  as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO,".S", MTF_spe, sep=""),2])      # casestudy.HR4.S1         
# bevholt, ricker,  shepherd, segreg,geomean
Fsqsetting <-  as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO,".S", MTF_spe, sep=""),3])      # casestudy.HR4.S1  
# "last"   # if "rescaled" is selected, the F by age of the last year will be calculated averaging the last avg_y years and rescaled to the last year. The reduction scenarios will be applied to this new F by age.
Ref_point  <-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO, sep=""),2]))      # casestudy.HR4  
# = 0.5
#ref_year <-   years.forecast[as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR", BMT_SCENARIO, sep=""),1])) ]     # casestudy.HR4  
# = 2020
#end_fore <- years.forecast[foreperiod]

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching STF for species", BMT_SPECIES[STF_spe]), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

#Fsqsetting = "last"      # can be "last" of "rescaled". If "last" the Fsq is the F of the last year, if "rescaled" will calulted an average of F on the last avg_Y years, rescaled to the last year. That estimates of F will be used for forecasts.
#Ref_point = 0.5          # Ref point
#ages <- 0:3              # age classes
#minfbar   <- 0            # age range for fbar
#maxfbar   <- 2
#----------------------------------

#source("scripts/STF_BEMTOOL.r")
STFresult <- STF(XSAinfo[[m_spe]]$results, year_start,year_end,Ref_point,avg_y,Fsqsetting)
