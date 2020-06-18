# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


      
      if (!LOADED_CASESTUDY) { 
     go_quest <- T  
    if (go_quest) {   go_quest <- setCSname() }    
   # if (go_quest) {   go_quest <- assign_casestudypath() }
    if (go_quest) {   go_quest <- setPeriod() }

    if (go_quest) {  
    
    mat_cfg_species_settings <<- data.frame(matrix( nrow=length(BMT_SPECIES), ncol=20) )
 colnames(mat_cfg_species_settings) <- c("", "[female.lifespan]", "[male.lifespan]", "[sexratio]", "[a.female]", "[b.female]", "[a.male]", "[b.male]", "[t0.female]", "[k.female]", "[linf.female]", "[t0.male]", "[k.male]", "[linf.male]", "[l50.female]", "[matrange.female]", "[l50.male]", "[matrange.male]", "[natural mortality constant]", "[stock recruitment relationship]")
  mat_cfg_species_settings[,1] <- paste("casestudy.S", 1:length(BMT_SPECIES), ".params", sep="")

mat_cfg_ALADYM_sim  <<- data.frame(matrix( nrow=length(BMT_SPECIES), ncol=3))
colnames(mat_cfg_ALADYM_sim) <- c("", "[ALADYM simulation]",	"[Average years for RP and forecast]")
mat_cfg_ALADYM_sim[,1] <- paste("casestudy.S",  1:length(BMT_SPECIES), ".AladymSimulation", sep="") 

mat_cfg_REF_points  <<- data.frame(matrix( nrow=length(BMT_SPECIES), ncol=4))
colnames(mat_cfg_REF_points) <- c("","[RP ALADYM calculation]",	"[RP ALADYM use]", "[external table RPs]")
mat_cfg_REF_points[,1] <- paste("casestudy.referencepoints.S", 1:length(BMT_SPECIES), sep="") 

first_y <- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.startsimulation" , 2]))
last_y <- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.endsimulation" , 2]))    
        
BMT_YEARS_SIMULATION <<- c(first_y:last_y)

#gSignalConnect(combo_species, "changed", reload_species_info)
for (choice in BMT_YEARS_SIMULATION) { combo_years_VIT$appendText(choice) }

mat_cfg_XSA[3,] <- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""),	paste("[input catches ", first_y, "-", last_y, "]", sep=""),	paste("[output F  ", first_y, "-", last_y, "]", sep=""),	"[reference points]", "")


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_XSA_list <<-  c(mat_cfg_XSA_list, list(mat_cfg_XSA))
}

if ((ncol(mat_cfg_VIT)-1) >= (last_y-first_y +1)) {
   mat_cfg_VIT[3,] <- c(	"", paste("[file-",BMT_YEARS_SIMULATION,"]", sep =""), rep("", ((ncol(mat_cfg_VIT)-1) - (last_y-first_y +1) ))	)
} else {
   yers <- c(first_y:last_y)
    to_add <- data.frame(matrix( ncol=((last_y-first_y +1) - (ncol(mat_cfg_VIT)-1)), nrow=6))
    mat_cfg_VIT <<- cbind(mat_cfg_VIT, to_add)
     mat_cfg_VIT[3,] <- c(	"", paste("[file-",BMT_YEARS_SIMULATION,"]", sep =""))
     mat_cfg_VIT[is.na(mat_cfg_VIT[])] <- ""
}

VITpathss <<- mat_cfg_VIT[4:6,]
colnames(VITpathss) <- mat_cfg_VIT[3,]


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_VIT_list <<-  c(mat_cfg_VIT_list, list(mat_cfg_VIT))
}


mat_cfg_externalReport[3,] <- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""),	 "",  "", "", "")


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_externalReport_list <<-  c(mat_cfg_externalReport_list, list(mat_cfg_externalReport))
}

mat_cfg_SURBA[3,] <- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""), "")


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_SURBA_list <<-  c(mat_cfg_SURBA_list, list(mat_cfg_SURBA))
}

mat_cfg_assessment_tools <<-list()
 for (sp in 1:length(BMT_SPECIES)) {
mat_cfg_assessment_tools <<- c(mat_cfg_assessment_tools,list(NULL)) 
}

   
      for (choice in BMT_SPECIES) { 
      gtkComboBoxRemoveText(combo_species, 0)
    }
    
    for (choice in BMT_SPECIES) { 
    combo_species$appendText(choice)   
    } 
  
        for (choice in BMT_FLEETSEGMENTS) { 
          gtkComboBoxRemoveText(bmt_combo_fleetsegments, 0)
           gtkComboBoxRemoveText(bmt_economicdata_fleet_combo, 0)
#         gtkComboBoxRemoveText(bmt_combo_fleetsegments_effort_r4, 0)
    }
      
   for (choice in BMT_FLEETSEGMENTS) { 
        bmt_combo_fleetsegments$appendText(choice)
        bmt_economicdata_fleet_combo$appendText(choice)
#        bmt_combo_fleetsegments_effort_r2$appendText(choice)        
    }
        
# GENERIC_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
#   colnames(GENERIC_matrix) <- c("year",MONTHS)
#     GENERIC_matrix$year <- years
  GENERIC_matrix <- NULL

EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))

EFFORT_NUMBER_list <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
EFFORT_DAY_list <<- vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
EFFORT_GT_list <<- vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
EFFORT_KW_list <<- vector(mode = "list", length = length(BMT_FLEETSEGMENTS))

for (flfl in 1:length(BMT_FLEETSEGMENTS)) {
      spe_list <<- c()
      for (sp in 1:length(BMT_SPECIES)) {
          spe_list <<- c(spe_list, list(GENERIC_matrix))      
      }
LANDING_list_all <<- c(LANDING_list_all, list(spe_list)) 
}


mat_cfg_LandingData  <<- data.frame(matrix( nrow=2, ncol=(length(BMT_SPECIES)+1)))
mat_cfg_LandingData[1,] <- c("", paste("[S", c(1:length(BMT_SPECIES)), " production file]", sep=""))
mat_cfg_LandingData[2,] <- c("casestudy.TimeSeries.productionData", rep("", length(BMT_SPECIES)))

mat_cfg_LandingData[is.na(mat_cfg_LandingData)] <- ""
 

 }
 gtkComboBoxSetActive(combo_species, 0 ) 
 gtkComboBoxSetActive(combo_assessment_tool, 0 ) 
  gtkComboBoxSetActive(bmt_economicdata_fleet_combo, 0 )   
    } else {
     go_quest <- T   
}