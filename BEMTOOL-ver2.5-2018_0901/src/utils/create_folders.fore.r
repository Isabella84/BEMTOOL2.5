# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# save folders for the projects

if (!MEY_CALCULATION ) {
dir_temp <- getwd()
setwd(casestudy_path)

harvest_rule_code <<-  as.character(cfg[rownames(cfg) == "casestudy.HR",1])
harvest_rule_level <<- as.character(cfg[rownames(cfg) == "casestudy.HR",2])
harvest_rule_id <<- paste("HR", harvest_rule_code, "-", harvest_rule_level, sep="")

dir.create(harvest_rule_id)
setwd(paste(casestudy_path, "/",harvest_rule_id, sep=""))
#dir.create("Biological indicators")
dir.create("Biological Pressure Impact")
dir.create("Economic indicators")
dir.create("working files")
#dir.create("Uncertainty")

if (BMT_SCENARIO != BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {
#dir.create("ALADYM")

for (ss in 1:length(BMT_SPECIES)) {
    setwd(paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact", sep=""))
    dir.create(paste("ALADYM - ", BMT_SPECIES[ss], sep=""))
    setwd(paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ss], sep=""))
    dir.create("Input")
    # dir.create("Graphs")
    # dir.create("working files")
    #setwd(paste(casestudy_path, "/", harvest_rule_id, "/ALADYM/", BMT_SPECIES[ss],"/Tables", sep=""))
    dir.create("quantiles")
#            setwd(paste(casestudy_path, "/", harvest_rule_id, "/ALADYM/", BMT_SPECIES[ss],"/Graphs", sep=""))
#            dir.create("grouped")
#    dir.create("Uncertainty")
}

} else {
for (ss in 1:length(BMT_SPECIES)) {
    setwd(paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact", sep=""))
    dir.create(paste("MSTF - ", BMT_SPECIES[ss], sep=""))
}

}

setwd(dir_temp)


} else {

dir_temp <- getwd()
setwd(casestudy_path)

dir.create("MEY calculation")
setwd(paste(casestudy_path, "/MEY calculation", sep=""))



dir.create(paste("MEY", harvest_rule_level))
setwd(paste(casestudy_path, "/MEY calculation/MEY ",harvest_rule_level, sep=""))
#dir.create("Biological indicators")
dir.create("Biological Pressure Impact")
dir.create("Economic indicators")
#dir.create("Uncertainty")
dir.create("working files")

for (ss in 1:length(BMT_SPECIES)) {
    setwd(paste(casestudy_path, "/MEY calculation/MEY ",harvest_rule_level, "/Biological Pressure Impact", sep=""))
    dir.create(paste("ALADYM - ", BMT_SPECIES[ss], sep="") )
    setwd(paste(casestudy_path, "/MEY calculation/MEY ",harvest_rule_level, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ss], sep=""))
    dir.create("Input")
#    dir.create("Graphs")
#    dir.create("working files")
   # setwd(paste(casestudy_path, "/MEY calculation/MEY ",harvest_rule_level, "/ALADYM - ", BMT_SPECIES[ss], sep=""))
    dir.create("quantiles")
#                setwd(paste(casestudy_path, "/MEY calculation/MEY ",harvest_rule_level, "/ALADYM/", BMT_SPECIES[ss],"/Graphs", sep=""))
#            dir.create("grouped")
   # dir.create("Uncertainty")
}

setwd(dir_temp)

}
