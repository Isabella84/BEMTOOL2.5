# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





readVIT<-function(year, m_spe, n_fleets) {

if (FALSE) {
 year=y_int
 m_spe= m_int
 n_fleets=n_fleet_for_species
}
# [VIT.sex]	[VIT.sexratio]	[VIT.analysis.length]	[VIT.analysis.discard]	[VIT.file.female]	[VIT.female.ages]	[VIT.file.males]	[VIT.male.ages]	[VIT.file.combined]	[VIT.male.ages]

VIT.sex <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),2])
#VIT.sexratio <-	    as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),3]))
VIT.analysis.length <-	as.logical(cfg[rownames(cfg) == paste("casestudy.S",m_spe, ".StockAssessmentTool", sep=""),3])
VIT.analysis.discard <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),4])

# ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),5])) 
# ages_M <-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAssessmentTool", sep=""),6]))

if (VIT.sex) {
    filePath_F <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileF", sep=""),year] )
    filePath_M <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileM", sep=""),year] )
    table_F <-  read.csv(filePath_F, sep=";")
    ages_F <- which(as.character(table_F$Class) == "Catch in Numbers") - 2   
    table_M <-  read.csv(filePath_M, sep=";") 
    ages_M <- which(as.character(table_M$Class) == "Catch in Numbers") - 2   
} else {
    filePath_C <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileC", sep=""),year] )
    table_C <-  read.csv(filePath_C, sep=";")
    ages_C <- which(as.character(table_C$Class) == "Catch in Numbers") - 2   
}

if (VIT.sex) {
    VITfemale <- getVIToutput(filePath_F, ages_F, n_fleets)
    VITmale <- getVIToutput(filePath_M, ages_M, n_fleets)
    return_matrix <- list(VITfemale, VITmale)
} else {
    VITcombined <- getVIToutput(filePath_C, ages_C, n_fleets)
    return_matrix <- list(VITcombined)
}

return(return_matrix)
}