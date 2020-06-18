# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# #############################################################################################
# FLEET INITIALIZATION of FORECAST objects only for fleet 
# #############################################################################################

for (y in (simperiod+1):(simperiod+foreperiod) ) {
# ---------------------------------------------------------------------------------------------------
# 1st STEP: Initialization of fleetsegment and the fleet 
# ---------------------------------------------------------------------------------------------------
# read number of species from cfg and names and do a loop with the following actions:
for (n in 1:length(BMT_FLEETSEGMENTS)) {
    # create the objects Fleetsegments
    #print(paste("creating bmtFleetsegment [", BMT_FLEETSEGMENTS[n], "]...", sep=""), quote=FALSE)  
    source(suppressWarnings(paste(getwd(), "/src/ini/fleetsegment.ini.r", sep="")))
    if (n==1) {
    AllSegments <- list(new_bmtFleetsegment) 
    } else {
    AllSegments <- c(AllSegments, new_bmtFleetsegment)
    } 
}

# out of fleet segment loop define the whole fleet by mean of the created list of fleetsegments
# structure of import weight data frame
import.weight_df <- data.frame(matrix(-1, nrow=1, ncol=length(BMT_SPECIES))  )

colnames(import.weight_df) <- BMT_SPECIES 
rownames(import.weight_df) <- years.forecast[(y-simperiod)] 

# store the Fleetsegments at time t=1 
                  
Fleetyear <<- c(Fleetyear, new(Class= "bmtFleet",
                    country =	as.character(cfg[rownames(cfg) == "casestudy.country",1]),
                    GSA =	as.character(cfg[rownames(cfg) == "casestudy.GSA",1]),
                    fleetsegments =	AllSegments,
                    import.weight	= import.weight_df) )
}
