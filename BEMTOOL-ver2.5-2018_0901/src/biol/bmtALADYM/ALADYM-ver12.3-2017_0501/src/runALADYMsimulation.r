# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 if (!IN_BEMTOOL | (IN_BEMTOOL & !SKIP_spe)) {
wnd_sim <- showMessage("SIMULATION in progress...")
gtkWidgetSetSensitive(main_window, FALSE)
} else {
wnd_sim <- showMessage("LOADING in progress...")
gtkWidgetSetSensitive(main_window, FALSE)
}

if (!SKIP_spe) {

 if (IN_BEMTOOL) {
cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching BEMTOOL biological simulation [ALADYM] for species", BMT_SPECIES[ALADYM_spe]), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")
  } else {
cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching ALADYM simulation for species", new_aldPopulation@scientific_name), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")
  }

# cat("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
# rm(list = ls())
# Part to be changed by the user
# -------------------------------------------------------------------------------
# select the name of the table where are stored the data per gear:
# Gears_data = "Gears_data_PAPE_GSA18.csv" 
# prod_data_name = "Production_data.csv"   # ""            #AGGIUNTO
# eff_data_name = "" #"Effort_data.csv"                #AGGIUNTO
# ----------------------------------------------------------------------------
#
# Creating the environments

graphics.off()

suppressWarnings(rm(INP))
suppressWarnings(rm(RND) )
suppressWarnings(rm(BAS)  )
suppressWarnings(rm(SRO)  )
suppressWarnings(rm(GLO)   )

INP <- new.env()
RND <- new.env()
BAS <- new.env()
SRO <- new.env()
GLO <- new.env()

GLO$ThisIsVersion = "ALADYM-ver10.1.3-2015"

source(paste(ALADYM_home, "/src/print_info.r", sep=""))

GLO$starting_time <- proc.time()

print(paste("Writing input data file...", sep=""), quote=F)
source(paste(ALADYM_home, "/src/DataIn.r", sep=""))

if (!IN_BEMTOOL) {
  BMT_SPECIES <<- as.character(new_aldPopulation@scientific_name)
}

suppressWarnings(source(paste(ALADYM_home, "/src/paths.r", sep=""))  )

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration

INP$Year_simulation  <- length(years) # + length(years_forecast)
#GLO$L_number  <- INP$Time_slice *  (length(years)  + length(years_forecast) )


# GLO$L_number  <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
# FLEETSEGMENTS_names <-unique(gear_data$Gear)               # aladym stand alone              # istruzione spostata nel loadfunction della gui

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


#if (as.numeric(INP$OPT_F_TYPE)==2) { 
#Fm = read.table(name_F_by_gear,sep=";",header=TRUE)      # AGGIUNTO              # cambiare qui nel caso di entrata con F
#}

print("Data read.", quote=FALSE)

                         

if (gtkToggleButtonGetActive(radio_data)) {
prod_data <- get_production_data()
if (as.numeric(INP$OPT_F_TYPE)==1) {      # il P_production lo calcola solo se c'è entrata con Z                                                           
p_production_mat <- P_production_calc(prod_data,forecast)
    for (i in 1:forecast){
    INP$p_Production[i,] = p_production_mat[i,]
    }
}
} else {
INP$p_Production[i,]  <- get_p_production()
}


if (gtkToggleButtonGetActive(radio_effortdata) ) {
eff_data <- get_effort_data()
fact_mat <- fact_calc (eff_data,"N")    
for (i in 1:forecast){
        for (gear in 1:nb_gears) {
        INP$Fishing_efforts[i,gear] = fact_mat[i,gear]
        }
    }
} else {
        INP$Fishing_efforts <- get_fishing_coefficient()
}

flush.console()

#
# Per il controllo
#
if(GLO$Nrun < 2) {
  GLO$Nrun <- 100
}


if ((as.character(Cal)=="Y") ) {

source(paste(ALADYM_home, "/src/RunCalibration.r", sep=""))


} else {
#---------------------
print("Simulation in progress...",quote=FALSE)
# Start the clock!

RunModel_ptm <- proc.time()  


   if (exists("f")) { 
    rm("f")
   }
RunModel(1,(forecast-1))
# Stop the clock


# RunModel_ptm <- proc.time() 
proc_ <- proc.time()
print(paste("ALADYM [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-RunModel_ptm[3]), 2), "sec" ), quote=F )    
#print(proc.time() - RunModel_ptm, quote=F ) 
#rm(RunModel_ptm)

print("Simulation executed",quote=FALSE)
}

#------
# REFERENCE POINTS
if (as.character(Calc_ref_point)=="Y"){
print("Reference points calculation in progress...",quote=FALSE)
source(paste(ALADYM_home, "/src/RunRPs.r", sep=""))
print("Calculation reference points executed",quote=F)
#PlotRPs()
#saveKobePlot(years)
}
#------

# esportazione dei risultati
#

GLO$L_number  <- INP$Time_slice * INP$Year_simulation  

RUN_CI_FORE <<- F
load_Annual_Z_Sinclair(forecast-1)
load_Annual_F_weighted(forecast-1)
#} 
load_Annual_Z(forecast-1)  
#Annual_F_by_gear(forecast-1)               
load_Annual_F(forecast-1)               
#Export(forecast-1)

export_tables(forecast-1)
if (length(years) > 1) {
export_tab_mort(forecast-1)
}
indicators(forecast-1)

#catches_by_age(forecast-1)         #richiamato in Annual_F_by_gear 


if (IN_BEMTOOL) {
INP$filename <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - ", BMT_SPECIES[ALADYM_spe], sep="")
 } else {
INP$filename <- paste(tablesDIR, "/", new_aldPopulation@scientific_name, sep="")
 }
 

print("Saving ALADYM plots...", quote=F)
PlotRecruitment()
cat(" [PlotRecruitment] ") 

PlotInput()
cat(" [PlotInput] ") 

if (!IN_BEMTOOL) {

PlotYear()
cat(" [PlotYear] ") 
Plot_yield()
cat(" [Plot_yield] ") 
Plot_length() 
cat(" [Plot_length] ")   
Plot_F()             
cat(" [Plot_F] ")

} else {

PlotYear_allFleets()
cat(" [PlotYear] ")
#Plot_yield_allFleets() 
#cat(" [Plot_yield] ") 
Plot_length_allFleets() 
cat(" [Plot_length] ") 
#Plot_F_allFleets()              
#cat(" [Plot_F] ")
  
}      


if (!IN_BEMTOOL) {
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), TRUE)
gtkNotebookSetCurrentPage(notebook, 5)
gtkWidgetSetSensitive(btn_runScenario, TRUE)
}

landing_comparison <- saveObservedvsSimulatedPlot_landing()
discard_comparison <- saveObservedvsSimulatedPlot_discard()


catch_comparison <- landing_comparison

# simulated 
sim_disc <- data.frame(discard_comparison[3:nrow(discard_comparison), 2:(length(FLEETSEGMENTS_names)+2)]  )
sim_disc[is.na(  sim_disc)] <- 0
sim_land <-  data.frame(landing_comparison[3:nrow(landing_comparison), 2:(length(FLEETSEGMENTS_names)+2)] )
sim_catch <- sim_land 
for (nr in 1:nrow(sim_land)) {
 sim_catch[nr,] <- as.numeric(as.character(sim_land[nr,]))  + as.numeric(as.character(sim_disc[nr,]  ))
}

# observed 
obs_disc <- data.frame(discard_comparison[3:nrow(discard_comparison), (length(FLEETSEGMENTS_names)+3):(length(FLEETSEGMENTS_names)*2+3)]  )
obs_disc[is.na(  obs_disc)] <- 0
obs_land <-  data.frame(landing_comparison[3:nrow(landing_comparison), (length(FLEETSEGMENTS_names)+3):(length(FLEETSEGMENTS_names)*2+3)] )
obs_catch <- obs_land 
for (nr in 1:nrow(obs_land)) {
 obs_catch[nr,] <- as.numeric(as.character(obs_land[nr,]))  + as.numeric(as.character(obs_disc[nr,]  ))
}
diff_catch_table <- obs_catch
for (nr in 1:nrow(obs_catch)) {
 diff_catch_table[nr,] <-  round((as.numeric(as.character(sim_catch[nr,])) - as.numeric(as.character(obs_catch[nr,])))/as.numeric(as.character(obs_catch[nr, ])) * 100, 2)  
}
to_write <- data.frame(cbind(years, cbind(sim_catch, cbind(obs_catch, diff_catch_table ))) )
colnames(to_write)  <-  c(c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total"))
to_write <- data.frame(rbind(c("Year", c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total")), to_write) )
to_write <- data.frame(rbind(c("", "Simulated Catch [tons]", rep("", length(FLEETSEGMENTS_names)), "Observed Catch [tons]", rep("", length(FLEETSEGMENTS_names)), "Relative difference [%]",  rep("", length(FLEETSEGMENTS_names)) ), to_write)  )


#to_write <- data.frame( cbind(to_write, landing_comparison[, ((ncol(landing_comparison)-1):ncol(landing_comparison))])   )
 # write.table(to_write, SIMULATEDVSOBSERVED_CATCH_table, col.names=FALSE, row.names=FALSE, sep=";")

for (nc in c(1:14)) {
to_write <-  cbind(to_write, rep("", (length(years)+2)))
 }
 
colnames(landing_comparison)  <- c(1:ncol(landing_comparison))
#landing_comparison[, (ncol(landing_comparison)-13):(ncol(landing_comparison))] <- ""
for (nc in c(1:14)) {
discard_comparison <-  cbind(discard_comparison, rep("", (length(years)+2)))
 }
 
colnames(discard_comparison)  <- c(1:ncol(landing_comparison))
colnames(to_write)  <- c(1:ncol(landing_comparison))

summary_table <- data.frame(rbind(to_write, rbind(landing_comparison, discard_comparison)))
write.table(summary_table, SIMULATEDVSOBSERVED_SUMMARY_table, col.names=FALSE, row.names=FALSE, sep=";")


if (IN_BEMTOOL) {
mat_path <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - ", BMT_SPECIES[ALADYM_spe], " mat-len-wei M.csv", sep="")
towr_M <-  data.frame(BAS$MMaturity)
towr_M <- data.frame(cbind(towr_M, BAS$MLength) )
towr_M <- data.frame(cbind(towr_M, BAS$MWeight) )
towr_M <- data.frame(cbind(c(INP$tr:(nrow(towr_M)+INP$tr-1)), towr_M) )
colnames(towr_M) <- c("Months", "Maturity", "Length", "Weight")
write.table(file=mat_path, towr_M,row.names=FALSE,  sep=";")

mat_path <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - ", BMT_SPECIES[ALADYM_spe], " mat-len-wei F.csv", sep="")
towr_F <-  data.frame(BAS$FMaturity)
towr_F <- data.frame(cbind(towr_F, BAS$FLength) )
towr_F <- data.frame(cbind(towr_F, BAS$FWeight) )
towr_F <- data.frame(cbind(c(INP$tr:(nrow(towr_F)+INP$tr-1)), towr_F) )
colnames(towr_F) <- c("Months", "Maturity", "Length", "Weight")
write.table(file=mat_path, towr_F,row.names=FALSE,  sep=";")
 } 



if (!IN_BEMTOOL) {
referencepoints_tbl <- suppressWarnings(try(read.csv(REFERENCEPOINTS_table, sep=";"), silent = TRUE))  
if (class(referencepoints_tbl) !=  "try-error" ) {
PlotRPs()
saveKobePlot(years)
}
}

 # set the status quo for the forecast fleet segments
for (fl_se in 1:length(FLEETSEGMENTS_names)) {  
FleetList_forecast[[fl_se]] <- setFleetsegmentStatusQuo(FleetList_simulation[[fl_se]], FleetList_forecast[[fl_se]])
}

  print("StatusQuo saved in the forecast objects!", quote=F) 


if (IN_BEMTOOL) {
fleet_list_fore <- list(species=ALADYM_spe, aldFleets=FleetList_forecast) 

#if (!exists("ALADYM_GUI_fleets_fore") ) {
print(paste("********************* adding to fleets_fore! species:", BMT_SPECIES[ALADYM_spe]), quote=F)

   .GlobalEnv$ ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast
 path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle_fore.Rdata", sep="")
save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  

}


if (!IN_BEMTOOL) {
# load the first segment
gtkComboBoxSetActive(combo_fleetsegments_fore, 0 ) 

last_year_recruitment <- round(meanWequals(INP$Recruits,(length(years)*12+1), INP$Time_slice)[length(years)], 0)

gtkEntrySetText(entry_costant_recr_forecast, last_year_recruitment )
gtkEntrySetText(entry_costant_recr_forecast_UN, last_year_recruitment )
 
gtkEntrySetText(entry_costant_Linf_f, BAS$F_mean_Linf )
gtkEntrySetText(entry_costant_Linf_m, BAS$M_mean_Linf )
gtkEntrySetText(entry_costant_k_f, BAS$F_mean_k )
gtkEntrySetText(entry_costant_k_m, BAS$M_mean_k )
gtkEntrySetText(entry_costant_t0_f, BAS$F_mean_t0 )
gtkEntrySetText(entry_costant_t0_m, BAS$M_mean_t0 )

gtkEntrySetText(entry_costant_males_L50, BAS$M_mean_L50 )
gtkEntrySetText(entry_costant_females_L50, BAS$F_mean_L50 )
gtkEntrySetText(entry_costant_males_MR, BAS$M_mean_MR )
gtkEntrySetText(entry_costant_females_MR, BAS$F_mean_MR )

} else {


print(paste("Saving", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("SRO_", ALADYM_spe, sep=""), new.env())
SRO_simulation <- get(paste("SRO_", ALADYM_spe, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_simulation)     
}
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/SRO_", ALADYM_spe,".Rdata", sep="")
save(list = ls(SRO), envir = SRO, file= path_to_save)

print(paste("Saving", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_simulation)
}
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/INP_", ALADYM_spe, ".Rdata",sep="")
save(list = ls(INP), envir = INP, file= path_to_save)

print(paste("Saving", paste("RND_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("RND_", ALADYM_spe, sep=""), new.env())
RND_simulation <- get(paste("RND_", ALADYM_spe, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_simulation)
}
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/RND_", ALADYM_spe,".Rdata", sep="")
save(list = ls(RND), envir = RND, file= path_to_save)


print(paste("Saving", paste("BAS_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("BAS_", ALADYM_spe, sep=""), new.env())
BAS_simulation <- get(paste("BAS_", ALADYM_spe, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_simulation)
}
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/BAS_", ALADYM_spe,".Rdata", sep="")
save(list = ls(BAS), envir = BAS, file= path_to_save)


print(paste("Saving", paste("GLO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("GLO_", ALADYM_spe, sep=""), new.env())
GLO_simulation <- get(paste("GLO_", ALADYM_spe, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_simulation)
}
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GLO_", ALADYM_spe,".Rdata", sep="")
save(list = ls(GLO), envir = GLO, file= path_to_save)

print(paste("Saving", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)

  
  
}

if (!IN_BEMTOOL) {
assign("Global_temp", new.env())
for (obj_name in ls()) {
  if (!("RGtkObject" %in% class(get(obj_name))) & !("function" %in% class(get(obj_name)))   ) {
     assign(obj_name, get(obj_name), envir=Global_temp)
     }
}
Rworking_env <- paste(workingfilesDIR,"/", new_aldPopulation@scientific_name, ".Rdata", sep="")
save(list = ls(Global_temp), envir = Global_temp, file= Rworking_env)
wnd <- showMessageOK("SIMULATION completed!")
} else {
Rworking_env <- paste(workingfilesDIR,"/", new_aldPopulation@scientific_name, ".Rdata", sep="")
save.image(file=Rworking_env)
wnd <- showMessage("SIMULATION completed!")
wnd$destroy()
}
# gtkComboBoxSetActive(combo_fleetsegments_fore, 0)

gtkWidgetSetSensitive(main_window, TRUE)
wnd_sim$destroy()

} else {        # skip species ************************************************************************************************************************************
# skip the species

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Reading last saved BEMTOOL biological simulation [ALADYM] for species", BMT_SPECIES[ALADYM_spe]), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")


INP <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/INP_", ALADYM_spe, ".Rdata", sep="")
no_error <- try( load(file= path_to_save , INP), silent=TRUE)
 
 if (class(no_error) !=  "try-error") {

print(paste("Reading", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_simulation)     
}

RND <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/RND_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , RND)

print(paste("Reading", paste("RND_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("RND_", ALADYM_spe, sep=""), new.env())
RND_simulation <- get(paste("RND_", ALADYM_spe, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_simulation)     
}

BAS <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/BAS_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , BAS)

print(paste("Reading", paste("BAS_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("BAS_", ALADYM_spe, sep=""), new.env())
BAS_simulation <- get(paste("BAS_", ALADYM_spe, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_simulation)     
}


SRO <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/SRO_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , SRO)

print(paste("Reading", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("SRO_", ALADYM_spe, sep=""), new.env())
SRO_simulation <- get(paste("SRO_", ALADYM_spe, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_simulation)     
}

GLO <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GLO_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , GLO)

print(paste("Reading", paste("GLO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("GLO_", ALADYM_spe, sep=""), new.env())
GLO_simulation <- get(paste("GLO_", ALADYM_spe, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_simulation)     
}


path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle_fore.Rdata", sep="")
load(path_to_save)  



#for (fl_se in 1:length(FLEETSEGMENTS_names)) {  
#FleetList_forecast[[fl_se]] <- setFleetsegmentStatusQuo(FleetList_simulation[[fl_se]], FleetList_forecast[[fl_se]])
#}
#
#  print("StatusQuo saved in the forecast objects!", quote=F) 
#
#
#if (IN_BEMTOOL) {
#fleet_list_fore <- list(species=ALADYM_spe, aldFleets=FleetList_forecast) 
#
##if (!exists("ALADYM_GUI_fleets_fore") ) {
#print(paste("********************* adding to fleets_fore! species:", BMT_SPECIES[ALADYM_spe]), quote=F)
#
#   .GlobalEnv$ ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast
# path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/GUIfle_fore.Rdata", sep="")
#save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  
#
#}

                                    
FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#if (is.null(FleetList_forecast)) {
#      print(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!"), quote=F)
#      print(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!"), quote=F)
#      showError(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!")) 
#      ALADYM_stop <<- TRUE
#      SKIP_spe <<- FALSE
#}


} else {
      print(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!"), quote=F)
      print(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!"), quote=F)
      showError(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!")) 
      ALADYM_stop <<- TRUE
      SKIP_spe <<- FALSE
}



gtkWidgetSetSensitive(main_window, TRUE)
wnd_sim$destroy()

#wnd <- showMessage("LOADING completed!")
#wnd$destroy()

} 

