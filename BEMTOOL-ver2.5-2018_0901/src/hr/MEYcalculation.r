# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





if (!exists("MEY_LEVEL")) {
     MEY_EFFORT_VAR <<-  as.character(cfg[rownames(cfg) == "casestudy.MEY",3])
     MEY_FLEETSEGMENT <<-  as.character(cfg[rownames(cfg) == "casestudy.MEY",2])
  
     levs <<- seq(0.1,2,0.1)
     levs <<- c(0.000001,levs)
      
MEY_LEVEL <<- 1 
} else {
 MEY_LEVEL <<- MEY_LEVEL +1 
}
            
     # for (lev in levs) {
     if (MEY_LEVEL <= length(levs)) {
      # #############################################################################################
      # run Harvest Rules
      # #############################################################################################
      phase <<- "FORECAST"                                     
      lev <<- levs[MEY_LEVEL]
      BMT_SCENARIO <<- 2
           
      harvest_rule_code <<- "MEY"
      harvest_rule_level <<- paste("lev", lev)
      harvest_rule_id <<- paste(harvest_rule_code, " ", harvest_rule_level, sep="")
      
      source(suppressWarnings(paste(getwd(), "/src/utils/create_folders.fore.r", sep="")))
      source(paste(getwd(), "/src/hr/runHR.r", sep=""))
      
  #  if (FALSE) {  
      # save economic results for level
  

} else {


MEY_levels_table <- data.frame(matrix(nrow=length(levs), ncol=8))
colnames(MEY_levels_table) <- c("Case study name", "Effort level", "Effort variable", "Fleet segment", "Number of years", "Gross value added", "Profit", "ROI")
MEY_levels_table[,1] <- casestudy_name
MEY_levels_table[,2] <- levs
MEY_levels_table[,3] <- MEY_EFFORT_VAR
MEY_levels_table[,4] <- MEY_FLEETSEGMENT
MEY_levels_table[,5] <- length(years.forecast)

for (n_lev in 1:length(levs)) {
      # #############################################################################################
     # run Harvest Rules
      # #############################################################################################   
      harvest_rule_code <<- "MEY"
      harvest_rule_level <<- paste("lev", levs[n_lev])
      harvest_rule_id <<- paste(harvest_rule_code, " ", harvest_rule_level, sep="")
      
     name_econind <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Economic output ", harvest_rule_id,".csv", sep="")
                    # HR12-MEY - Economic output FORE MEY lev 2.csv
                    
      ECOindicators <- read.csv(name_econind,sep=";",header=T)

ECOindicators_gross <- ECOindicators[as.character(ECOindicators$Variable) == "gross.value.added" & as.numeric(as.character(ECOindicators$Year)) == years.forecast[foreperiod],]
ECOindicators_profit <- ECOindicators[as.character(ECOindicators$Variable) == "profit" & as.numeric(as.character(ECOindicators$Year)) == years.forecast[foreperiod],]
ECOindicators_ROI <- ECOindicators[as.character(ECOindicators$Variable) == "Economic.indicators[ROI]" & as.numeric(as.character(ECOindicators$Year)) == years.forecast[foreperiod],]   

MEY_levels_table[n_lev, 6] <- sum(as.numeric(as.character(ECOindicators_gross$Value)))
MEY_levels_table[n_lev, 7] <- sum(as.numeric(as.character(ECOindicators_profit$Value)))
MEY_levels_table[n_lev, 8] <- mean(as.numeric(as.character(ECOindicators_ROI$Value)))
}

name_MEY_levels_file <<- paste(casestudy_path, "/MEY calculation/", casestudy_name, " - MEY by effort levels.csv", sep="")
write.table(MEY_levels_table, file = name_MEY_levels_file, row.names=F, sep=";")

MEYs <- c("MEY.gross.value.added", "MEY.profit", "MEY.ROI")
MEY_header <- c("Case_study",	"Scenario",	"ID_scenario", "Fleet_segment",	"Species", "Year", "Variable", "Value", "Unit")
MEY_results <- data.frame(matrix(nrow=3, ncol=length(MEY_header)))
colnames(MEY_results)  <- MEY_header

MEY_results$Case_study <- casestudy_name
MEY_results$Scenario <- "MEY"
MEY_results$ID_scenario <- "MEY"
MEY_results$Fleet_segment <- MEY_FLEETSEGMENT
MEY_results$Species <- "ALL"
MEY_results$Year <- "ALL"
MEY_results$Variable <- MEYs
MEY_results$Value <- c(max(MEY_levels_table[, 6]), max(MEY_levels_table[, 7]), max(MEY_levels_table[, 8]))
MEY_results$Unit <- c("euro", "euro", "")

name_MEY_file <<- paste(casestudy_path, "/MEY calculation/", casestudy_name, " - MEY.csv", sep="")
write.table(MEY_results, file = name_MEY_file, row.names=F, sep=";")

saveMEY_Plot()

showMessageOK("MEY calculation completed!")
          BMT_STATE <<- "FINISH" 
}

	#	error <- data.frame(matrix("MEY process successfully terminated!", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\info.err", sep=""), sep=";", col.names=F, row.names=F)

