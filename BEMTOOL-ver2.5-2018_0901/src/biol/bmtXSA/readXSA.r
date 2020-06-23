# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





readXSA <- function(m_spe, n_fleets) {
# [VIT.sex]	[VIT.sexratio]	[VIT.analysis.length]	[VIT.analysis.discard]	[VIT.file.female]	[VIT.female.ages]	[VIT.file.males]	[VIT.male.ages]	[VIT.file.combined]	[VIT.male.ages]

#ages_C <- as.numeric(Populations[[m_spe]]@lifespan[2,1]) + 1

    filePath_results <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileC", sep=""),1] )
    filePath_catchesbyfleet <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileC", sep=""),2] )
    filePath_Fbyfleet <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileC", sep=""),3] )
    filePath_RPs <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".StockAss.fileC", sep=""),4] )
    XSAout <- dget(filePath_results)
    XSAcatches <- read.csv(filePath_catchesbyfleet, sep=";")
    XSA_F <- read.csv(filePath_Fbyfleet, sep=";")
    RPs <- read.csv(filePath_RPs, sep=";") 
    outputs <- list(results=XSAout, catchesbyf=XSAcatches, Fbyf=XSA_F, referencepoints = RPs)

return(outputs)
}
