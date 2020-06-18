# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#---------------------------------------- 
# REFERENCE POINTS
#---------------------------------------- 
# Congelamento di tutti gli ambienti

ptm_RPS_TIME <- proc.time()  

SRO_temp <- new.env()
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_temp)
}
INP_temp <- new.env()
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_temp)
}

RND_temp <- new.env()
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_temp)
}

BAS_temp <- new.env()
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_temp)
}
GLO_temp <- new.env()
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_temp)
}



# vartiabili locali
# No Fertility
loca_Fertility                     <- 1
# Base value of natural mortality to calculate FMax
loca_min_BAS.MM                     <- mean(BAS$MM)
loca_min_BAS.FM                     <- mean(BAS$FM)


# costruzione e sovrascrittura delle variabili secondo l'ultimo anno

x <- 6  # cicli di life span per calcolo reference points
loca_Nsimulation2 <- max(INP$FGrowth_tend,INP$MGrowth_tend) * 12 * x  +1

if (as.numeric(INP$OPT_F_TYPE)==1) {                                                         # aggiunto-entrata con F
BAS$MZ_estimated                    <- vector(mode="numeric", length=(loca_Nsimulation2))
BAS$FZ_estimated                    <- vector(mode="numeric", length=(loca_Nsimulation2))
}
# inizializzazione matrice di output
F_lev <- seq(0,2,0.1)
if (nb_gears==1) {
Ref_points_tab = data.frame(matrix(nrow=length(F_lev),ncol=(13)))
} else {
Ref_points_tab = data.frame(matrix(nrow=length(F_lev),ncol=(13+nb_gears)))
}
Ref_points_tab[,1] = F_lev

num_iter_RPs <- 0

# start the loops for lifespan * 2


for (f in seq(0,2,0.1))  {
 num_iter_RPs <- num_iter_RPs + 1
if (as.numeric(INP$OPT_F_TYPE)==2) { 
#Fm = read.table(name_F_by_gear,sep=";",header=TRUE)      # AGGIUNTO
nb_gears=ncol(INP$Fm)-3
#
INP$Fmales <-  INP$Fm[as.character(INP$Fm[,ncol(INP$Fm)])=="M",]               # spostato nell'IF (MT)
INP$Fmales <- INP$Fmales[,1:(ncol(INP$Fmales)-1)]
INP$Ffemales <- INP$Fm[as.character(INP$Fm[,ncol(INP$Fm)])=="F",]
INP$Ffemales <-INP$Ffemales[,1:(ncol(INP$Ffemales)-1)]

}

source(paste(ALADYM_home, "/src/settings_ref_points.r", sep=""))

#if (as.numeric(INP$OPT_F_TYPE)==2) {
#Fmales = Fmales[,-c(1,2)]
#Ffemales = Ffemales[,-c(1,2)]
#}
# costruzione e sovrascrittura di Z secondo il livello di F
if (as.numeric(INP$OPT_F_TYPE)==1) {
BAS$MZ_estimated []    <-  (mean(INP$MZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]) - loca_min_BAS.MM)*f +loca_min_BAS.MM           
BAS$FZ_estimated  []   <- (mean(INP$FZ_estimated[(forecast-INP$Average_forecast*12+1):forecast])  -loca_min_BAS.FM)*f +loca_min_BAS.FM
}
  if (f==0) {
  INP$Fishing_efforts[,] <- 0
 INP$Esc_Surv_rate_sim[]="N"
  
  }   

load_SIMULATION_UNEXPLOITED(loca_Fertility,1,(loca_Nsimulation2-1)) #questo deve stare prima dell'exploited! 
if (as.numeric(INP$OPT_F_TYPE)==1) {  
load_SIMULATION_EXPLOITED(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM, 1, (loca_Nsimulation2-1))
} else {
load_SIMULATION_EXPLOITED_entrataF(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM, 1, (loca_Nsimulation2-1))
}

# esportazione dei risultati
#source("src/export.r")
load_Annual_Z_Sinclair((loca_Nsimulation2-1))
load_Annual_F_weighted((loca_Nsimulation2-1))
load_Annual_Z(loca_Nsimulation2-1)
load_Annual_F(loca_Nsimulation2-1)

#Annual_F_per_refpoints((loca_Nsimulation2-1))  
export_tables(loca_Nsimulation2-1)

# lettura tabelle prodotte
#-----------------------------------
 mortality = read.table(MORTALITY_table,header=TRUE,sep=";")
 population = read.table(POPULATION_table,header=TRUE,sep=";")
 production = read.table(PRODUCTION_table,header=TRUE,sep=";")
 
 gears= as.character(t(FLEETSEGMENTS_names))
  
  if (nb_gears!=1){
colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",FLEETSEGMENTS_names),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",FLEETSEGMENTS_names,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",FLEETSEGMENTS_names,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",FLEETSEGMENTS_names,sep=""))
}  else {
colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
}


colnames (population) = c("Year","Total_biomass_exploited_pop","Total_biomass_unexploited_pop","SS_NUMBERS_exploited_pop","SS_NUMBERS_unexploited_pop","SSB_exploited_pop","SSB_unexploited_pop","ESSBratioUSSB","Mean_length_of_exploited_pop","Mean_length_of_unexploited_pop","Mean_length_SS_of_exploited_pop","Mean_length_SS_of_unexploited_pop","Mean_age_of_exploited_pop","Mean_age_of_unexploited_pop","Mean_age_SS_of_exploited_pop","Mean_age_SS_of_unexploited_pop","NUMBERS_exploited_pop","NUMBERS_unexploited_pop")

if(nb_gears==1) {
colnames (production) =  c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard", "Discard_ratio", "AnnualLandObl")
}  else {
colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names, sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names, sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names, sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""), "Discard_ratio",paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""), paste("LandingObligation_",FLEETSEGMENTS_names,sep=""))
}
# aggiornamento matrice di output
r = f/0.1+1
last= (loca_Nsimulation2-1)/12
Ref_points_tab[r,2] = mortality$Annual_F_estimated [last]  # Annual F on age range selected by the user   (males + females)
Ref_points_tab[r,3] = production$Biological_Production[last]   # Biological Production                  
Ref_points_tab[r,4] = production$Total_Yield[last]             # Total Yield
Ref_points_tab[r,5] = production$Mean_length_in_catch[last]   # Mean length in catch
Ref_points_tab[r,6] = population$Total_biomass_exploited_pop[last]    # Total_biomass_exploited_pop
Ref_points_tab[r,7] = population$Mean_length_of_exploited_pop[last]   # Mean_length_of_exploited_pop
Ref_points_tab[r,8] = population$Mean_length_SS_of_exploited_pop[last]    # Mean_length_SS_of_exploited_pop
Ref_points_tab[r,9] = mortality$Annual_Z_estimated [last] # Annual Z estimated age range
Ref_points_tab[r,10] = mortality$Annual_Z_estimated_life_span [last]                      # Annual Z life span
Ref_points_tab[r,11] = population$SSB_exploited_pop[last] # SSB
Ref_points_tab[r,12] = population$SSB_unexploited_pop[last] 
Ref_points_tab[r,13] = population$SSB_exploited_pop[last]/population$SSB_unexploited_pop[last]
if (nb_gears!=1){
  for (gear in 1:nb_gears) {
  Ref_points_tab[r,13+gear] = mortality[last,ncol(mortality)-nb_gears+gear]
  }
}

if (nb_gears!=1){
  for (gear in 1:nb_gears) {
  Ref_points_tab[r,13+nb_gears+gear] = production[last,4+gear]
  }
}

if (nb_gears!=1){
  for (gear in 1:nb_gears) {
  Ref_points_tab[r,13+2*nb_gears+gear] = production[last,4+(nb_gears+1)*3+gear]
  }
}


# e poi sovrascrivi le variabili con i temp congelati     
#-----------------------------
SRO <- new.env()
for (obj_name in ls(SRO_temp)) {
     assign(obj_name, get(obj_name, envir = SRO_temp), envir=SRO)
}
INP <- new.env()
for (obj_name in ls(INP_temp)) {
     assign(obj_name, get(obj_name, envir = INP_temp), envir=INP)
}

RND <- new.env()
for (obj_name in ls(RND_temp)) {
     assign(obj_name, get(obj_name, envir = RND_temp), envir=RND)
}

BAS <- new.env()
for (obj_name in ls(BAS_temp)) {
     assign(obj_name, get(obj_name, envir = BAS_temp), envir=BAS)
}
GLO <- new.env()
for (obj_name in ls(GLO_temp)) {
     assign(obj_name, get(obj_name, envir = GLO_temp), envir=GLO)
}

#---------------------
print(paste("F factor = ",f,sep=""),quote=FALSE)

#if (IN_BEMTOOL) {
#  save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\Tables\\RPs.log", sep="")
#} else {
#    save_path <- paste(ALADYM_home, "\\Tables\\RPs.log", sep="")
#}


 RP_log <- data.frame(matrix(paste("F factor = ",f,sep=""), ncol=1, nrow=1))
 write.table(RP_log, file = RPs_logfile, sep=";", col.names=F, row.names=F)

} 

rm(num_iter_RPs)

Ref_points_tab [1,2]=0
Ref_points_tab [1,(ncol(Ref_points_tab)-2*nb_gears+1):ncol(Ref_points_tab)]=0


if (nb_gears==1){
colnames(Ref_points_tab) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR")
} else {
colnames(Ref_points_tab) = c("F factor","Annual_F_estimated","Biological_production", "Total_Yield","Mean_length_in_catch", "Total_biomass_exploited_pop" ,"Mean_length_of_exploited_pop","Mean_length_SS_of_exploited_pop","Annual_Z_estimated","Annual_Z_estimated_life_span","SSB_exploited_pop","SSB_unexploited_pop","SPR",paste("Annual_F_estimated_",gears,sep=""),paste("Yield_",gears,sep=""),paste("Landing_",gears,sep=""))
}

write.table(Ref_points_tab,file=YpR_table,sep=";",row.names=FALSE)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
#save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\Tables\\[", casestudy_name, "] YpR_Results.csv", sep="")
#write.table(Ref_points_tab, save_path,row.names=FALSE, sep=";")
## ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

if (INP$FRLt==4)  {
source(paste(ALADYM_home, "/src/Refpoints_interpolazione_poli5grado_no_SRR.r", sep=""))
} else {
source(paste(ALADYM_home, "/src/Refpoints_interpolazione_poli2grado_SRR.r", sep=""))
}
#source("src/Refpoints_interpolazione.r")
#source("src/Refpoints.r")

# sovrascrivi le variabili con i temp congelati e continua col forecast
#rm(SRO,INP,RND,BAS,GLO)
#-----------------------------

SRO <- new.env()
for (obj_name in ls(SRO_temp)) {
     assign(obj_name, get(obj_name, envir = SRO_temp), envir=SRO)
}
INP <- new.env()
for (obj_name in ls(INP_temp)) {
     assign(obj_name, get(obj_name, envir = INP_temp), envir=INP)
}

RND <- new.env()
for (obj_name in ls(RND_temp)) {
     assign(obj_name, get(obj_name, envir = RND_temp), envir=RND)
}

BAS <- new.env()
for (obj_name in ls(BAS_temp)) {
     assign(obj_name, get(obj_name, envir = BAS_temp), envir=BAS)
}
GLO <- new.env()
for (obj_name in ls(GLO_temp)) {
     assign(obj_name, get(obj_name, envir = GLO_temp), envir=GLO)
 }

print(proc.time() - ptm_RPS_TIME, quote=F )
#---------------------   FINE REFERENCE POINTS
