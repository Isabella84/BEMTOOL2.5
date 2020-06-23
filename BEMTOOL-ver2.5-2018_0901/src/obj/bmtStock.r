# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ------------------------------------------------------------------------------
# Definition of object types used in BEMTOOL code (S4 classes and methods)
# - Stock (for EXPLOITED and UNEXPLOITED)
# ------------------------------------------------------------------------------

# definition of object: bmtStock  
STOCK_TYPE <- c("EXPLOITED", "UNEXPLOITED")         
setClass(Class="bmtStock",
        representation=representation(
            Stype = "character",
            
            numbers = "list", # by month of age and sex
            annual.numbers = "numeric",
            annual.numbers.CI.perc = "data.frame",
            
            SB = "list", # by month of age and sex
            annual.SB = "numeric",
            annual.SB.CI.perc = "data.frame",
            
            SS.numbers = "data.frame",  #  # by month of simulation
            annual.SS.numbers = "numeric",
            annual.SS.numbers.CI.perc = "data.frame",
            
            SSB = "data.frame",   # by month of simulation
            annual.SSB = "numeric",
            annual.SSB.CI.perc = "data.frame",
            
            L95 = "numeric",
            L95.CI.perc = "data.frame",
            meanLength = "numeric",
            meanLength.CI.perc = "data.frame",
            meanWeight = "numeric",
            meanWeight.CI.perc = "data.frame", 
            criticalLength = "numeric",
						criticalLength.CI.perc = "data.frame",        
            harvestRate = "numeric",
						harvestRate.CI.perc = "data.frame",          # only for exploited stock
            exploitationRate = "numeric",
            exploitationRate.CI.perc = "data.frame"))    # only for exploited stock
