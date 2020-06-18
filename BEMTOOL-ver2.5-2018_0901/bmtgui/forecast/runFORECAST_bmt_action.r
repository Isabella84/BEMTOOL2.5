# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



runFORECAST_bmt_action <- function(w) {

FINAL_BMTCFG <- setScenarioOptions()

      # inserted_name <-  gtkEntryGetText(bmt_entry_scenario_name)
   
   casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <<- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)
scenarios_dirs <<- scenarios_dirs[str_detect(scenarios_dirs[], "HR")  ]

scenario_names <<- c()
if (length(scenarios_dirs)> 0) {
for (len in 1:length(scenarios_dirs)) {
vect_ <- str_split(as.character(scenarios_dirs[len]), "/")
vect_ <- vect_[[1]]
scenario_names <<- c(scenario_names, vect_[length(vect_)])
}
}

name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")

if (name_this_scenario %in% scenario_names) {
showError("A scenario with the inserted name already exists! Insert a different name.")
} else {
       

#bmt_wnd_sim <<- showMessage("SIMULATION in progress...")
#gtkWidgetSetSensitive(BMTmain_window, F)

FINAL_BMTCFG_economic <- saveBMTCFG_scenario()

max_num_col <- max(ncol(FINAL_BMTCFG), ncol(FINAL_BMTCFG_economic) )

if (max_num_col > ncol(FINAL_BMTCFG)) {
    finalist_scenario <- data.frame(cbind(FINAL_BMTCFG), matrix("", ncol=max_num_col-ncol(FINAL_BMTCFG), nrow=nrow(FINAL_BMTCFG)), stringsAsFactors=F)
    finalist_eco <- FINAL_BMTCFG_economic 
} else if (max_num_col > ncol(FINAL_BMTCFG_economic)) {
    finalist_eco <- data.frame(cbind(FINAL_BMTCFG_economic), matrix("", ncol=max_num_col-ncol(FINAL_BMTCFG_economic), nrow=nrow(FINAL_BMTCFG_economic)), stringsAsFactors=F)
    finalist_scenario <- FINAL_BMTCFG
} else {
   finalist_scenario <- FINAL_BMTCFG
   finalist_eco <- FINAL_BMTCFG_economic 
}

colnames(finalist_eco) <- paste("col", c(1:ncol(finalist_eco)), sep="")
colnames(finalist_scenario) <- paste("col", c(1:ncol(finalist_scenario)), sep="")

FINAL_BMTCFG <- data.frame(rbind(finalist_scenario, finalist_eco) , stringsAsFactors=F)

      name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")
#write.table(FINAL_BMTCFG, paste(getwd(), "/bmtconfig.csv", sep=""), sep=";", row.names=F, col.names=F)
   
 null_row_names <- c( which( as.character(FINAL_BMTCFG[,1]) == ""), which( !is.na(as.numeric(as.character(FINAL_BMTCFG[,1]) ))) , which(is.na(FINAL_BMTCFG[,1])) )
 
 levels(FINAL_BMTCFG[,1]) <- factor(c(levels(FINAL_BMTCFG[,1]),  c(1:length(null_row_names))))  
 
    FINAL_BMTCFG[null_row_names,1]  <- c(1:length(null_row_names))
 
  write.table(FINAL_BMTCFG, paste(getwd(), "/bmtconfig scenario.csv", sep=""), sep=";", row.names=F, col.names=F, na="")

  BMT_STATE <<- "DO_SCENARIO"

              suppressWarnings(source(paste(getwd(), "/src/runBEMTOOLforecast.r", sep="") ) ) 

#path_of_vessel <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on number ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="")
#path_of_days  <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on seadays ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="") 
#
#                     
#  write.table(VESSELS_final_fore, file=path_of_vessel, sep=";", row.names=F, col.names=F)
#    write.table(DAYS_final_fore, file=path_of_days, sep=";", row.names=F, col.names=F)





#source(paste(getwd(), "/src/runBEMTOOL.r", sep=""))
  # CAMBIARE PERCORSOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
}

}