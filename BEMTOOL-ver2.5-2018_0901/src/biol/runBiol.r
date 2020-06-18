# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# ---------------------------------------------------------------------------------------------------
# run ALADYM for the species m 
# --------------------------------------------------------------------------------------------------- 

  # ATTENTION: The next step depends on the user's setting from cfg table [casestudy.S1.AladymSimulation] 
  
  # for (m_spe in 1:m_stock) {

  
  ALADYM_flag <<- FALSE
  for (number_spe in 1:length(BMT_SPECIES)) {
  if (!ALADYM_flag) {     
  ALADYM_spe <<- number_spe
  ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])
  }
  } 
  #   ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])
      # ALADYM_input_path <<- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),2])
      # ALADYM_gears_path <<- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),3])
       # ALADYM_F_by_gear <<- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),4])
    
       
        if (ALADYM_flag) {
        
        # to launch ALADYM with the GUI
              forecast <<- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  +1
                    GLO <- new.env()
      GLO$L_number <- 12 *  (length(years)  + length(years.forecast) )
               source(paste(ALADYM_home, "/ALADYM.r", sep=""))
              BMT_STATE <<- "WAIT"             
        } else {
            BMT_STATE <<- "DIAGNOSIS"
        }
 
  # } 
