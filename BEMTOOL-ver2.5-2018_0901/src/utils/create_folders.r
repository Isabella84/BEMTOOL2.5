# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# save folders for the projects

 create_folders  <- function()  {

dir_temp <- getwd()
setwd(casestudy_path)
dir.create("Diagnosis")
setwd(paste(casestudy_path, "/Diagnosis", sep=""))
# dir.create("Biological indicators")
dir.create("Biological Pressure Impact")
dir.create("Economic indicators")
dir.create("working files")
#dir.create("ALADYM")
setwd(paste(casestudy_path, "/Diagnosis/Biological Pressure Impact", sep=""))

for (ss in 1:length(BMT_SPECIES)) {                                            # casestudy.S1.AladymSimulation
   ALADYM_simulation <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
if (ALADYM_simulation) {
    # setwd(paste(casestudy_path, "/Diagnosis/ALADYM", sep=""))
    setwd(paste(casestudy_path, "/Diagnosis/Biological Pressure Impact", sep=""))
    dir.create(paste("ALADYM - ", BMT_SPECIES[ss], sep="") )
    setwd(paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ss], "", sep=""))
    dir.create("Input")
    #dir.create("Graphs")
    dir.create("Sim vs Obs")
    #dir.create("working files")
#        setwd(paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ss],"/Graphs", sep=""))
#            dir.create("grouped")
    }
}

setwd(dir_temp)

  }