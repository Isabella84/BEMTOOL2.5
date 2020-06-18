# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






if (BMT_STATE == "START")  {
 source(suppressWarnings(paste(getwd(), "/src/utils/ini.r", sep="")))
create_folders()
      # #############################################################################################
      # BASE YEARS (Initialization)
      # #############################################################################################
      
      Populations <<- list()
      Stocks <- list()
      FleetStockInteractions <- list()
      Interactionsyear <<- list()
      Fleetyear <<- list()     # to track the evolution in the time: every year a list of fleet segment and attributes is saved 
      AllSegments <- list()
      
      phase <<- "SIMULATION" 
      source(suppressWarnings(paste(getwd(), "/src/ini/simulation_fleetcreation.r", sep="")))
      
      phase <<- "FORECAST"
      source(suppressWarnings(paste(getwd(), "/src/ini/forecast_fleetcreation.r", sep="")))

      phase <<- "SIMULATION"
      # ---------------------------------------------------------------------------------------------------
      # 2 STEP: read economic data and update all Fleetsegment in all the time of simulation 
      # ---------------------------------------------------------------------------------------------------
      # source(suppressWarnings(paste(getwd(), "/src/econ/readEcondata.r", sep="")))
      Fleetyear <<- setFleet(Fleetyear)
      
      Fleetyear <<- updateDerivedEffortVars.nonint(Fleetyear)
      
      # #############################################################################################
      # POPULATION INITIALIZATION
      # #############################################################################################
      
      
      for (m in 1:length(BMT_SPECIES)) {  
      ALADYM_spe <<- m
      # create the objects Populations
      # print(paste("creating bmtPopulation [", BMT_SPECIES[m], "]...", sep=""),  quote=FALSE)      
      source(suppressWarnings(paste(getwd(), "/src/ini/population.ini.r", sep="")))
      if (m==1) {
      Populations <<- list(new_bmtPopulation)
      } else {
      Populations <<- c(Populations, new_bmtPopulation) 
      }
      }
      
      
      source(suppressWarnings(paste(getwd(), "/src/ini/simulation_interactioncreation.r", sep="")))
      
      phase <<- "FORECAST"
      source(suppressWarnings(paste(getwd(), "/src/ini/forecast_interactioncreation.r", sep="")))
      
      phase <<- "SIMULATION"  
      # #############################################################################################
      # READ BIOLOGICAL DATA FROM STOCK ASSESSMENT
      # #############################################################################################
      source(paste(getwd(), "/src/biol/readBioldata.r", sep=""))
      
       # commented on version 2.5.5
     # source(paste(getwd(), "/src/econ/readEcondata_sim.r", sep=""))
      
      # #############################################################################################
      # run Biological model
      # #############################################################################################
      
      GO_ON_WITH_DIAGNOSIS <<- TRUE 
      
      for (m_spe in 1:m_stock) {
         if (GO_ON_WITH_DIAGNOSIS) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          SAtool <<- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),1])
          GO_ON_WITH_DIAGNOSIS <<- ALADYM_flag | (SAtool != "NONE") 
       }           
      }
    
      if (GO_ON_WITH_DIAGNOSIS) {
      ALADYM_reference_points_calc <<- c()
      
			ALADYM_GUI_fleets_fore <<- vector(mode="list", length=length(BMT_SPECIES))
			ALADYM_GUI_fleets <<-  vector(mode="list", length=length(BMT_SPECIES))
			ALADYM_GUI_populations <<- vector(mode="list", length=length(BMT_SPECIES))
			ALADYM_GUI_simulations <<-  vector(mode="list", length=length(BMT_SPECIES))
      
      source(paste(getwd(), "/src/biol/runBiol.r", sep="")) 
      } else {
        
showError("Impossible to continue because one or more species have nor assessed neither simulated input data!") 
print("Impossible to continue because one or more species have nor assessed neither simulated input data!")
# 	error <- data.frame(matrix("Impossible to continue because one or more species have nor assessed neither simulated input data!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

      
      }
 } 
 
 if (BMT_STATE == "DIAGNOSIS" & GO_ON_WITH_DIAGNOSIS) {
 
  suppressWarnings(rm(INP))
  rm(GLO)
   rm(SRO)
    rm(BAS)
    rm(RND)
 
   print(paste("Updating FLEET objects from INTERACTION objects...", sep=""), quote=FALSE )
 #  simperiod <- length(BMT_YEARS_SIMULATION)
   
  for (yy in 1:simperiod) {
         Fleetyear <<- updateFleetfromInteraction(Fleetyear, yy)
           #  source(paste(getwd(), "/src/biol/updateFleetfromInteraction.r", sep=""))
      }  
      
      # #############################################################################################
      # run ECONOMIC estimation 
      # #############################################################################################
      # STIMA DEGLI INIDCATORI E VARIABILI DERIVATE
      print(paste("Economic indicators calculation...", sep=""), quote=FALSE )
      Fleetyear <<- runEcon(Fleetyear, simperiod, length(BMT_FLEETSEGMENTS), length(BMT_SPECIES))

      # average.daysatsea[3] = max by regulation
      # fishingcoefficient = fishingCoeff_df,
      # fishingcoefficient.annual = 0,
      # landing.number = landing.number_df,
name_bioind <<- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Biological indicators.csv", sep="")
name_pressind <<- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Pressure impact indicators.csv", sep="")
name_econind <<- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Economic output.csv", sep="")
    
print("DIAGNOSIS in progress [State indicators]...", quote=F)
write.table(getBiologicalTable(years), file=name_bioind, sep=";", row.names=F)    
print("DIAGNOSIS in progress [Pressure/Impact indicators]...", quote=F)
write.table(getPressureImpactTable(years), file=name_pressind, sep=";", row.names=F)       
print("DIAGNOSIS in progress [Economic indicators]...", quote=F)
write.table(getEconomicTable(years), file=name_econind, sep=";", row.names=F)

MEY_CALCULATION <<- FALSE

print("Saving Kobe plot...", quote=F)
bmtsaveKobePlot(years, name_bioind, name_pressind)
print("Saving biological indicators plot...", quote=F)
#saveSingleIndicatorPlot(years, name_bioind, name_pressind)
saveSingleIndicatorPlot(years, name_bioind, name_pressind, NULL, NULL)
saveSingleIndicatorPlot_econ(years, name_econind, NULL)
print("Saving F plot...", quote=F)
saveF_Plot(years, name_pressind, name_bioind)
print("Saving Yield barplot...", quote=F)
saveYield_barPlot(years, name_pressind)

print("Saving Revenues-Landings plot...", quote=F)
saveRevenues_Plot(years, name_econind)
print("Saving Revenues-Discard plot...", quote=F)
saveRevenues_discard_Plot(years, name_econind)
print("Saving Avg.salary-Employment plot...", quote=F)
saveSalary_Plot(years, name_econind)
print("Saving Net profit-R/BER plot...", quote=F)
saveProfit_Plot(years, name_econind)
print("Saving Profit-Salary-Capital barplot...", quote=F)
save_EconomicbarPlot(years, name_econind)

#print("Saving observed vs simulated yield...", quote=F)
#saveObservedvsSimulatedPlot(years, name_pressind)

Rworking_env <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - BEMTOOL simulation.Rdata", sep="")
Rworking_env_selection <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - BEMTOOL simulation-rev.Rdata", sep="")

save(file=Rworking_env_selection, list = c("casestudy_path","casestudy_name", "Populations", "FleetStockInteractions", "Interactionsyear" , "Fleetyear", "XSAinfo", "XSAinfo_empty", "VITinfo", "VITinfo_empty" , "SURBAinfo", "SURBAinfo_empty",  "ReportINFO", "ReportINFO_empty", "ALADYM_GUI_fleets",  "ALADYM_GUI_populations" , "ALADYM_GUI_simulations", "BMT_SPECIES", "BMT_FLEETSEGMENTS", "years", "years_forecast", "FleetList_forecast"))           #   "ALADYM_GUI_fleets_fore",

save.image(file=Rworking_env)
 BMT_STATE <<- "DO_SCENARIO"                                         

  suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/reload_diagnosis.r", sep="")))		

gtkLabelSetText(lbl_current_step, " Step 5 of 9 ")  
gtkNotebookSetCurrentPage(BMTnotebook, 4)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 

#bmt_wnd_sim$destroy()
#gtkWidgetSetSensitive(BMTmain_window, T)    

# if (BMT_R_GUI) {
#    gtkWidgetSetSensitive(main_window, TRUE)
#    wnd_sim$destroy()
# } else {
#    source(paste(getwd(), "/src/runBEMTOOLforecast.r", sep=""))   
# }

 } 

