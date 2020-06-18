# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


BAS$MMaturity_or= BAS$MMaturity
BAS$FMaturity_or=BAS$FMaturity

BAS$FM_or= BAS$FM
BAS$MM_or=BAS$MM

 wnd_fore <- showMessage("FORECAST in progress...")

showCompTime <<- F

source( paste(ALADYM_home, "/src/paths.r", sep="") )


if (!IN_BEMTOOL) {
cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching ALADYM forecast for species", new_aldPopulation@scientific_name), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

  simperiod <- length(years)
  foreperiod <- length(years_forecast)
  all_years <<- c(years,years_forecast )
  bmt_average_forecast <<-  as.numeric(as.character(new_aldForecast@no_years_average))
	BMT_HR_CHANGE_FISHMORTALITY <- 3
	BMT_SCENARIO <- 0 
	gears <<- FLEETSEGMENTS_names 
	MEY_CALCULATION <<- FALSE
	
} else {
cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching BEMTOOL biological forecast [ALADYM] for species", BMT_SPECIES[ALADYM_spe]), quote=FALSE)


print("***************************************************************************", quote=FALSE)
cat("\n\n")
}

if (!MEY_CALCULATION) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd_fore <- showMessage("FORECAST in progress...")
} else {
 if (MEY_LEVEL == 1) {
  gtkWidgetSetSensitive(main_window, FALSE)
wnd_fore <- showMessage("FORECAST in progress...")
 }
}
 
#---------------------------------------- 
# FORECAST PHASE
#---------------------------------------- 

if (showCompTime)  {
runALADYMforecast_ptm <- proc.time()  
}


READ_ENV_OK <<- T

if (IN_BEMTOOL) {     

source(paste(ALADYM_home, "/src/runALADYMforecast_bemtool_case_load_envs.r", sep=""))

} else {        


# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같END in BEMTOOL

if (length(new_aldForecast@target_F) == 0)  {     # ----------------------------------------------- different from REDUCTION OF F scenario
    Ref_point <-  NA     # Reference point per short e medium term forecast            
    Ref_month <-  NA   
    Ref_year <-  NA 
} else {     # ----------------------------------------------------------------------------------------------------- different from REDUCTION OF F scenario	
        # read the forecast reduction input by the user
        bmt_forecast_reduction <-  as.numeric(as.character(new_aldForecast@target_F)) #as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",2]))
        # set the time span in which get the reduction input by the user
        bmt_time_span <- as.numeric(as.character(new_aldForecast@target_year)) - years[length(years)]  #as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",1]))
        # set the reference month 
        Ref_month <- (simperiod + bmt_time_span) * 12  
        Ref_point <-  new_aldForecast@target_F
      
      INP$Forecast_reduction <- data.frame(matrix(NA, nrow=1, ncol= nb_gears))
        
        for (n_int in 1:nb_gears) {        
            INP$Forecast_reduction[1, n_int] <- FleetList_forecast[[n_int]]@scenario.reduction
           
         #  Forecast_reduction[1,fl_ord] <- as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])  
         # INP$Forecast_reduction[1,fl_ord] <- FleetList_forecast[[fl_ord]]@scenario.reduction # as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])          
        }     
            
# Forecast_reduction <- INP$Forecast_reduction

}	

}

if (READ_ENV_OK)  {

INP$Year_simulation             <- simperiod + foreperiod
GLO$L_number  <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
forecast <-  simperiod * 12 + 1
   

INP$tr <- as.numeric(as.character(new_aldSimulation@Tr ))
loca_Nsimulation <- (GLO$L_number + 1)    #numero di mesi da simulare +1
nb_gears = length(FleetList_simulation)


RUN_CI_FORE <<- new_aldForecast@CI_calculation 

  if ( RUN_CI_FORE ) {
  
  if ( new_aldForecast@CI_recruitment ) {
     if (new_aldForecast@CI_recruitment_constant_or_SRR == 1) {
  if (new_aldForecast@CI_recruitment_1_file_or_distribution == 2 ) {    #external file (1) /distribution (2)                       ex: CI_error_source
  #  INP$R_type_sim           <- which(DISTRIBUTION == data.frame(new_aldForecast@recruitment.noise)$distribution[1])
    
    INP$R_type_for <- which(DISTRIBUTION == data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$distribution[1])
    INP$R_pam1_for   <- as.numeric(data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$A[1])
    INP$R_pam2_for  <- as.numeric(data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$B[1])
    
    } else {
    
    INP$R_type_sim  <-  4
    INP$R_pam1_sim   <- 1
    INP$R_pam2_sim  <- 1    
    
   # da verificare
  INP$R_type_for <- INP$R_type_sim
  INP$R_pam1_for <-  INP$R_pam1_sim
  INP$R_pam2_for <-  INP$R_pam2_sim
    }
    }
    }
    
    }


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

 source(paste(ALADYM_home, "/src/DataIn_forecast.r", sep=""))
  
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################




if ((forecast < GLO$L_number) & !is.na(Ref_point) & !is.na(Ref_month) & !is.na(INP$Average_forecast))  {
#--------------------------------------
# calcolo moltiplicatori per attrezzo
#--------------------------------------
forecast_simulation =  GLO$L_number - forecast +1
multipliers = matrix(nrow=forecast_simulation, ncol=nb_gears)    


 Forecast_reduction <- data.frame(matrix(NA, nrow=1, ncol= nb_gears))

   Forecast_reduction[1,] <- toupper(INP$Forecast_reduction)
   INP$Forecast_reduction <- Forecast_reduction  
        #for (n_int in 1:length(FLEETSEGMENTS_names)) {
#           Forecast_reduction[1,n_int] <- toupper(FleetList_forecast[[n_int]]@scenario.reduction)  
#        }      
#    INP$Forecast_reduction <- Forecast_reduction  
          
  for (g in 1:nb_gears) {
  multipliers[,g] = REDUCTION(INP$Forecast_reduction[1,g],g)
  }

  
#  print("Multipliers:", quote=F)
#print(multipliers[(forecast+1):(GLO$L_number+1)], quote=F )
  
# caso in cui sto usando le riduzioni forzate, i fishing coefficient sono uguali a 1
INP$Fishing_efforts[(forecast):nrow(INP$Fishing_efforts),] <- 1  

# si innesta il codice dei CI *********************************************************************** 같같같같같같같같같같같같같같같같같?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


if (!RUN_CI_FORE) {

loca_Fertility <- 1
 loca_min_BAS.MM  <- mean(BAS$MM)
loca_min_BAS.FM <- mean(BAS$FM)
load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number)     #


  if (as.numeric(INP$OPT_F_TYPE)==1) { 
  load_FORECAST(forecast) 
	} else {
  load_FORECAST_entrataF(forecast)
  }

  source(paste(ALADYM_home, "/src/output.r" ,sep="") )
    
load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
#Annual_F_by_gear(GLO$L_number) 
load_Annual_F(GLO$L_number) 
              
#Export(GLO$L_number)
export_tables(GLO$L_number)
export_tab_mort(GLO$L_number)
indicators(GLO$L_number)

} else {
source(paste(ALADYM_home, "/src/runCI.r" ,sep="") )
}

#if (Unc_growth!="Y") {
#source(paste(ALADYM_home, "/src/runCI.r" ,sep="") )
#} else if (RUN_CI_FORE & (Unc_growth=="Y") ) {
# source(paste(ALADYM_home, "/src/runCI_growth.r" ,sep="") ) 
#} else if (RUN_CI_FORE & (Unc_maturity=="Y"))  {
#  print("riga 1222_______________________________Entrato nell'if maturity_____________________________________")   # qui ?contemplate solo il caso di spunta solo su Maturity
#  source(paste(ALADYM_home, "/src/runCI_maturity.r" ,sep="") )
# }



 } else if ((forecast < GLO$L_number) & !is.na(INP$Average_forecast))  {
          # scenario non forzato, lavorano solo i fishing coefficient               

 forecast_simulation =  GLO$L_number - forecast +1
 multipliers = matrix(nrow=forecast_simulation,ncol=nb_gears)
  for (g in 1:nb_gears){
  multipliers[,g] = 1                 
  }

if (!IN_BEMTOOL) {
if (gtkToggleButtonGetActive(radio_effortdata)) {
fishing_eff_result <- fact_calc(eff_data,"Y") 
   for (gear in 1:nb_gears) {
    for (i in (forecast+1):(GLO$L_number+1)){
    INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]
    }
    }  
}
}  else {
fishing_eff_result <- fact_calc(eff_data,"Y") 
   for (gear in 1:nb_gears) {
    for (i in (forecast+1):(GLO$L_number+1)){
    INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]
    }

    } 
}



# si innesta il codice dei CI *********************************************************************** 같같같같같같같같같같같같같같같같같?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


if (!RUN_CI_FORE) {
loca_Fertility <- 1
 loca_min_BAS.MM  <- mean(BAS$MM)
loca_min_BAS.FM <- mean(BAS$FM)
load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number)     #


  if (as.numeric(INP$OPT_F_TYPE)==1) { 
  load_FORECAST(forecast) 
	} else {
  load_FORECAST_entrataF(forecast)
  }

  source(paste(ALADYM_home, "/src/output.r" ,sep="") )

  
load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
#Annual_F_by_gear(GLO$L_number) 
load_Annual_F(GLO$L_number) 
              
#Export(GLO$L_number)
export_tables(GLO$L_number)
export_tab_mort(GLO$L_number)
indicators(GLO$L_number)
  
 } else {
  INP$Recruits_original <- INP$Recruits  
   source(paste(ALADYM_home, "/src/runCI.r" ,sep="") )
 } 
 
# if (RUN_CI_FORE & Unc_growth!="Y" & Unc_maturity !="Y") {
#source(paste(ALADYM_home, "/src/runCI.r" ,sep="") )
#  } else if (RUN_CI_FORE & Unc_growth=="Y" ) {
# source(paste(ALADYM_home, "/src/runCI_growth.r" ,sep="") ) 
#  }  else if (RUN_CI_FORE & Unc_maturity=="Y"){
#  source(paste(ALADYM_home, "/src/runCI_maturity.r" ,sep="") )
# }
 

  
}   # fine if scenario
 

#source(paste(ALADYM_home, "/src/output.r" ,sep="") )
 
print("Forecast ended.", quote=FALSE)
flush.console()



#catches_by_age(GLO$L_number)  

print("Data exported.", quote=FALSE)
flush.console()

      # save_path <- paste(workingfilesDIR,"/", prefix_outfiles, "Selectivity params", suffix_outfiles,".csv", sep="")     
#        save_path_fc <- paste(workingfilesDIR,"/", prefix_outfiles, "Fishing coefficients", suffix_outfiles,".csv", sep="")     
       write.table(cbind(INP$param1,cbind(INP$param2, cbind(INP$param3, cbind(INP$param4, INP$param5)))), file=SEL_working_tabl, sep=";", row.names=F)      
          write.table(INP$Fishing_efforts, file=FC_working_tabl, sep=";", row.names=F)

if (!MEY_CALCULATION) {
print("Saving ALADYM plots...", quote=F)
PlotRecruitment()
cat(" [PlotRecruitment] ") 
#PlotInput()
#cat(" [PlotInput] ") 

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
             
}             

print("Program end.", quote=FALSE)
flush.console()

# save.image(file="simulation.RData")

#print("Timing:", quote=FALSE)
#print("Usr  Sys  Total", quote=FALSE)
#print(proc.time() - GLO$starting_time)
flush.console()


if (IN_BEMTOOL) {
INP$filename <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/", casestudy_name, " - ", BMT_SPECIES[ALADYM_spe], sep="")
 } else {
INP$filename <- paste(ALADYM_home, "/Tables/",new_aldPopulation@scientific_name, sep="")
 }
   


if (IN_BEMTOOL) {


print(paste("Saving", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("SRO_", ALADYM_spe, sep=""), new.env())
SRO_simulation <- get(paste("SRO_", ALADYM_spe, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_simulation)
}



print(paste("Saving", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_simulation)
}

print(paste("Saving", paste("RND_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("RND_", ALADYM_spe, sep=""), new.env())
RND_simulation <- get(paste("RND_", ALADYM_spe, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_simulation)
}

print(paste("Saving", paste("BAS_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("BAS_", ALADYM_spe, sep=""), new.env())
BAS_simulation <- get(paste("BAS_", ALADYM_spe, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_simulation)
}

print(paste("Saving", paste("GLO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("GLO_", ALADYM_spe, sep=""), new.env())
GLO_simulation <- get(paste("GLO_", ALADYM_spe, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_simulation)
}

}

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("ALADYM forecast completed!", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

if (!IN_BEMTOOL) {
referencepoints_tbl <- suppressWarnings(try(read.csv(REFERENCEPOINTS_table, sep=";"), silent = TRUE) )
if (class(referencepoints_tbl) !=  "try-error" ) {
saveKobePlot(c(years, years_forecast))
}
}


if (!MEY_CALCULATION) {
wnd_fore$destroy()


} else {
 if (MEY_LEVEL == 1) {
wnd_fore$destroy()
 }
}



if (!IN_BEMTOOL) {
wnd_fore_ok <- showMessageOK("FORECAST completed!")   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), TRUE)
gtkWidgetSetSensitive(main_window, TRUE) 
 gtkWidgetSetSensitive(button_saveall_selectivity_fore, TRUE)    
 gtkNotebookSetCurrentPage(notebook, 5)
} else {
if (!MEY_CALCULATION) {
wnd_fore_ok <- showMessage(paste("FORECAST completed for ",BMT_SPECIES[ALADYM_spe],"!", sep=""))
wnd_fore_ok$destroy()
main_window$destroy() 
} else {
if (MEY_LEVEL ==1) {
wnd_fore_ok <- showMessage(paste("FORECAST completed for ",BMT_SPECIES[ALADYM_spe],"!", sep=""))
wnd_fore_ok$destroy()
main_window$destroy() 
}
}
}

} else {
wnd_fore$destroy()
wnd_fore_ok <- showMessageOK("FORECAST not completed!")   
gtkWidgetSetSensitive(main_window, TRUE) 

}

