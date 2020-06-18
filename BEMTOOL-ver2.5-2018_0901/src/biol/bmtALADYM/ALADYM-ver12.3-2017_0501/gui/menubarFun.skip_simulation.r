# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







#
#
#
#
#
# ---------------------- Run simulation action
skip_simulation <- function(widget, window, Populations, Interactionsyear) {      

BMT_STATE <<- "WAIT"
SKIP_spe <<- TRUE

ALADYM_stop <<- FALSE

path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIpop.Rdata", sep="")
load(file= path_to_save , .GlobalEnv)   

# popu_list <- list(species=ALADYM_spe, aldPopulation=new_aldPopulation) 

#if (!exists("ALADYM_GUI_populations") ) {
  new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]] 
#  } else {
#   ALADYM_GUI_populations <<- c(ALADYM_GUI_populations, list(popu_list))
#  }

path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIsim.Rdata", sep="")
load(file= path_to_save , .GlobalEnv)
  
# sim_list <- list(species=ALADYM_spe, aldSimulation=new_aldSimulation) 

#if (!exists("ALADYM_GUI_simulations") ) {
  new_aldSimulation <<- ALADYM_GUI_simulations[[ALADYM_spe]] 
#  } else {
#   ALADYM_GUI_simulations <<- c(ALADYM_GUI_simulations, list(sim_list))
#  } 

# fleet_list <- list(species=ALADYM_spe, aldFleets=FleetList_simulation) 

path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle.Rdata", sep="")
load(file= path_to_save , .GlobalEnv)

#if (!exists("ALADYM_GUI_fleets") ) {
  FleetList_simulation <<- ALADYM_GUI_fleets[[ALADYM_spe]] 
#  } else {
#   ALADYM_GUI_fleets <<- c(ALADYM_GUI_fleets, list(fleet_list))
#  } 

source(paste(ALADYM_home, "/src/runALADYMsimulation.r", sep="") )

if (!ALADYM_stop)  {

# source(paste(ALADYM_home, "/src/main_Simulation.r", sep="") )

              # ----------------------------------------------------------------------------
              # UPLOAD BMT OBJECTS from ALADYM after the simulation
              # ---------------------------------------------------------------------------- 
              print(paste("Updating Biological data from ALADYM for species [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=FALSE )
              biologicalUpdateResults <- updateBiologicalfromALADYM(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear, ALADYM_reference_points_calc)
              Populations <<- biologicalUpdateResults$popus
              Interactionsyear <<- biologicalUpdateResults$inters
              Fleetyear <<- biologicalUpdateResults$fleets
              ALADYM_reference_points_calc <<- biologicalUpdateResults$refpoints 
              # source(paste(getwd(), "/src/biol/bmtALADYM/updateBiologicalfromALADYM.r", sep=""))
              
              # ----------------------------------------------------------------------------
              # ----------------------------------------------------------------------------
              # ----------------------------------------------------------------------------
main_window$destroy()

ALADYM_spe <<- ALADYM_spe + 1

if (ALADYM_spe <= length(BMT_SPECIES) & all(ALADYM_reference_points_calc)) {
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])

  if (ALADYM_flag) { 
        # to launch ALADYM with the GUI
             #  m_spe=2                           
              forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
              source(paste(ALADYM_home, "/ALADYM.r", sep=""))      
        # to launch ALADYM without GUI
        # source(paste(ALADYM_home, "/src/runALADYMsimulation.r", sep=""))              
        }
      BMT_STATE <<- "WAIT"
   } else {
   if (!all(ALADYM_reference_points_calc)) {
      BMT_STATE <<- "WAIT"
   }  else {
      BMT_STATE <<- "DIAGNOSIS"
   }
   source(suppressWarnings(paste(getwd(), "/src/runBEMTOOLdiagnosis.r", sep="") ) ) 
   }

   }
   
  # reload_fleetsegment_fore_info()
    
} 
