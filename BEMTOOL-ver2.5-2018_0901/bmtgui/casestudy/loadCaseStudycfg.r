# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#
#
#
loadCaseStudycfg <- function(path) {

bmt_wnd_sim <- showMessage("Loading CASE STUDY...")
gtkWidgetSetSensitive(BMTmain_window, F)

# path <- "C:\\Case18\\Case18_bmtconfig_noALADYM.csv"

 all_CFG_mat <- data.frame(read.csv(path, sep=";", header=F))
     
     mat_cfg_general <<- data.frame(all_CFG_mat[2:6,1:3])
     colnames(mat_cfg_general)  <<-  c("", "[case name]", "[store path]")
     
first_y <- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.startsimulation" , 2]))
last_y <- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.endsimulation" , 2]))    
        
BMT_YEARS_SIMULATION <<- c(first_y:last_y)

casestudy_path  <<- as.character(mat_cfg_general[1,3] )
casestudy_name <<-  as.character(mat_cfg_general[1,2] )


     
     all_CFG_mat <- all_CFG_mat[7:nrow(all_CFG_mat), ]
     
     mat_cfg_F <<-  data.frame(all_CFG_mat[which(!is.na(str_locate(all_CFG_mat[,1], "casestudy.F"))[,1]), 1:2]  )
     colnames(mat_cfg_F) <<- c("", "[code]")   
     
       BMT_FLEETSEGMENTS <<- as.character(mat_cfg_F[,2])
     
     all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_F)+2):nrow(all_CFG_mat), ]
     
     end_species <- which(!is.na(str_locate(all_CFG_mat[,1], "associatedFleetsegment")))[1]

      mat_cfg_S <<-  data.frame(all_CFG_mat[2:(end_species-2), 1:2] ) 
     colnames(mat_cfg_S) <<- c("", "[species]")
          
     BMT_SPECIES <<- as.character(mat_cfg_S[,2])

 GENERIC_matrix <- NULL

for (flfl in 1:length(BMT_FLEETSEGMENTS)) {
EFFORT_NUMBER_list <<-  c(EFFORT_NUMBER_list, list(GENERIC_matrix))
EFFORT_DAY_list <<- c(EFFORT_DAY_list, list(GENERIC_matrix))
EFFORT_GT_list <<- c(EFFORT_GT_list, list(GENERIC_matrix))
EFFORT_KW_list <<- c(EFFORT_KW_list, list(GENERIC_matrix))

      spe_list <<- c()
      for (sp in 1:length(BMT_SPECIES)) {
          spe_list <<- c(spe_list, list(GENERIC_matrix))
      
      }
LANDING_list_all <<- c(LANDING_list_all, list(spe_list)) 
}
     
      all_CFG_mat <- all_CFG_mat[(end_species-1):nrow(all_CFG_mat), ]
     
     mat_cfg_SF <<- data.frame(all_CFG_mat[2:(nrow(mat_cfg_S)+1), 1:(nrow(mat_cfg_F)+1)]  )
     colnames(mat_cfg_SF) <<- c("", paste("[F",1:nrow(mat_cfg_F),"]", sep=""))


     for (n_spe in 1:length(BMT_SPECIES)) {
     #print(paste("Interaction spe:", BMT_SPECIES[n_spe]))
          for (n_segm in 2:(length(BMT_FLEETSEGMENTS)+1) ) {
                if ( as.character(mat_cfg_SF[n_spe, n_segm]) != "" & as.character(mat_cfg_SF[n_spe, n_segm]) != "-" ) {
                    # print(paste("Interaction gear:", BMT_FLEETSEGMENTS[n_segm]))
                        to_add <- data.frame(matrix(c(BMT_SPECIES[n_spe], as.character(mat_cfg_SF[n_spe, n_segm]) ), nrow=1, ncol=2))  
                        colnames(to_add) <- colnames(interactions)
                        interactions <<- rbind(interactions, to_add)
                }      
          }
     }
     
     all_CFG_mat <- all_CFG_mat[(end_species-1):nrow(all_CFG_mat), ]
     
     mat_cfg_species_settings <<-  data.frame(all_CFG_mat[2:(nrow(mat_cfg_S)+1), 1:20]  )
 colnames(mat_cfg_species_settings) <<- c("", "[female.lifespan]", "[male.lifespan]", "[sexratio]", "[a.female]", "[b.female]", "[a.male]", "[b.male]", "[t0.female]", "[k.female]", "[linf.female]", "[t0.male]", "[k.male]", "[linf.male]", "[l50.female]", "[matrange.female]", "[l50.male]", "[matrange.male]", "[natural mortality constant]", "[stock recruitment relationship]")

      all_CFG_mat <- all_CFG_mat[(end_species-1):nrow(all_CFG_mat), ]   
  
# ****************************************************************** SETTING ASSESSMENT LISTS
  
  for (choice in BMT_YEARS_SIMULATION) { combo_years_VIT$appendText(choice) }
 
mat_cfg_XSA[3,] <<- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""),	paste("[input catches ", first_y, "-", last_y, "]", sep=""),	paste("[output F  ", first_y, "-", last_y, "]", sep=""),	"[reference points]", "")
 

for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_XSA_list <<-  c(mat_cfg_XSA_list, list(mat_cfg_XSA))
}
     
if ((ncol(mat_cfg_VIT)-1) >= (last_y-first_y +1)) {
   mat_cfg_VIT[3,] <<- c(	"", paste("[file-",BMT_YEARS_SIMULATION,"]", sep =""), rep("", ((ncol(mat_cfg_VIT)-1) - (last_y-first_y +1) ))	)
} else {
   yers <- c(first_y:last_y)
    to_add <- data.frame(matrix( ncol=((last_y-first_y +1) - (ncol(mat_cfg_VIT)-1)), nrow=6))
    mat_cfg_VIT <<- cbind(mat_cfg_VIT, to_add)
     mat_cfg_VIT[3,] <<- c(	"", paste("[file-",BMT_YEARS_SIMULATION,"]", sep =""))
     mat_cfg_VIT[is.na(mat_cfg_VIT[])] <<- ""
} 

VITpathss <<- mat_cfg_VIT[4:6,]
colnames(VITpathss) <<- mat_cfg_VIT[3,]


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_VIT_list <<-  c(mat_cfg_VIT_list, list(mat_cfg_VIT))
}

 


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_none_list <<-  c(mat_cfg_none_list, list(mat_cfg_none))
}
 
    
mat_cfg_externalReport[3,] <<- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""),	 "",  "", "", "")


for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_externalReport_list <<-  c(mat_cfg_externalReport_list, list(mat_cfg_externalReport))
}
  
mat_cfg_SURBA[3,] <<- c(	"", paste("[file ", first_y, "-", last_y, "]", sep=""), "")
 

for (sp in 1:length(BMT_SPECIES)) {
 mat_cfg_SURBA_list <<-  c(mat_cfg_SURBA_list, list(mat_cfg_SURBA))
}

mat_cfg_assessment_tools <<-list()
 for (sp in 1:length(BMT_SPECIES)) {
mat_cfg_assessment_tools <<- c(mat_cfg_assessment_tools,list(NULL)) 
}

    for (choice in BMT_SPECIES) { 
    combo_species$appendText(choice)   
#     bmt_eco_landing_species$appendText(choice)  
    } 
    
       for (choice in BMT_FLEETSEGMENTS) { 
    bmt_combo_fleetsegments$appendText(choice)    
    } 
    
            for (choice in BMT_FLEETSEGMENTS) { 
    bmt_economicdata_fleet_combo$appendText(choice)   
    } 

 # ************************************************************************ END SETTING ASSESSMENT LISTS 

    
     sa_too <- which(!is.na(str_locate(all_CFG_mat[,1], ".StockAssessmentTool")))
     end_satool <-  which(!is.na(str_locate(all_CFG_mat[,1], ".AladymSimulation")))
     
     for (num_spe in 1:length(BMT_SPECIES) ) {
     if (num_spe < length(BMT_SPECIES) ) {
             temp_ass <- data.frame(all_CFG_mat[(sa_too[num_spe]-1):(sa_too[num_spe+1]-2), ])     
              } else {
             temp_ass <- data.frame(all_CFG_mat[(sa_too[num_spe]-1):(end_satool[1]-2), ]) 
              }
              
             if (temp_ass[2,2] == "XSA") {  
                   mat_cfg_XSA_list[[num_spe]] <<-  temp_ass[,1:6]   
                   mat_cfg_assessment_tools[[num_spe]] <<- mat_cfg_XSA_list[[num_spe]]
              
              } else if (temp_ass[2,2] == "VIT") {
                      maxAge_pos <- which(temp_ass[1,] == "[maxAge]") 
                      if (maxAge_pos >= (length(BMT_YEARS_SIMULATION) +1)) {
                            mat_cfg_VIT_list[[num_spe]] <<-  temp_ass[,1:maxAge_pos]
                          } else {
                          mat_cfg_VIT_list[[num_spe]] <<-  temp_ass[,1:(length(BMT_YEARS_SIMULATION) +1)]
                        }
                         mat_cfg_assessment_tools[[num_spe]] <<-   mat_cfg_VIT_list[[num_spe]]
              } else if (temp_ass[2,2] == "SURBA") {
                     mat_cfg_SURBA_list[[num_spe]] <<-  temp_ass[,1:3]
                     mat_cfg_assessment_tools[[num_spe]] <<-   mat_cfg_SURBA_list[[num_spe]]  
              } else if (temp_ass[2,2] == "from Report") {
                      mat_cfg_externalReport_list[[num_spe]] <<-  temp_ass[,1:6]
                       mat_cfg_assessment_tools[[num_spe]] <<-   mat_cfg_externalReport_list[[num_spe]]
              } else if (temp_ass[2,2] == "none") {
                      mat_cfg_none_list[[num_spe]] <<-  temp_ass[,1:2]
                      mat_cfg_assessment_tools[[num_spe]] <<-   mat_cfg_none_list[[num_spe]]
              } 
   
     }
  
        all_CFG_mat <- all_CFG_mat[(end_satool[1]-1):nrow(all_CFG_mat), ]   
  
         mat_cfg_ALADYM_sim <<- all_CFG_mat[2:(length(BMT_SPECIES)+1), 1:3]
         colnames(mat_cfg_ALADYM_sim) <<- c("", "[ALADYM simulation]",	"[Average years for RP and forecast]")
     
        all_CFG_mat <- all_CFG_mat[(length(BMT_SPECIES)+2):nrow(all_CFG_mat),]


     rp_cfg <- which(!is.na(str_locate(all_CFG_mat[,1], "casestudy.referencepoints.S")[,1]))

     mat_cfg_REF_points <<- all_CFG_mat[rp_cfg,1:4]
     colnames(mat_cfg_REF_points) <<-  c("","[RP ALADYM calculation]",	"[RP ALADYM use]", "[external table RPs]")
     
     mat_cfg_EffortData <<- all_CFG_mat[1:4, 1:5]

  all_CFG_mat <- all_CFG_mat[5:nrow(all_CFG_mat),]
     mat_cfg_LandingData <<- all_CFG_mat[1:2, 1:(length(BMT_SPECIES)+1)]
      all_CFG_mat <- all_CFG_mat[3:nrow(all_CFG_mat),]
      
      all_CFG_mat <- all_CFG_mat[as.character(all_CFG_mat[,4]) != "[external table RPs]",] 
      
   all_CFG_mat <- all_CFG_mat[-which(!is.na(str_locate(as.character(all_CFG_mat[,1]), "casestudy.referencepoints.S")[,1])), ]
    
    

   mat_cfg_general_fore <<- data.frame(all_CFG_mat[1:2,1:2])
       all_CFG_mat <- all_CFG_mat[3:nrow(all_CFG_mat),]
       
       BMT_YEARS_FORECAST <<- c(as.numeric(as.character(mat_cfg_general_fore[1,2])):as.numeric(as.character(mat_cfg_general_fore[2,2])) )
             
     set_economic_data_lists()
     
     set_effort_data_lists()
     
     set_landing_data_lists() 

#		 print(all_CFG_mat)
#
#   print("prendere correction factor")   
 
 # commented on version 2.5.5
  #  mat_cfg_EconomicIndicator_simu <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+1), 1:2] )


   
  # ***************************************************************+ set the graphical items

 gtkEntrySetText(entry_CaseStudy_name,  mat_cfg_general[mat_cfg_general[,1] == "casestudy.name",2])
  gtkEntrySetText(lbl_casestudy, mat_cfg_general[mat_cfg_general[,1] == "casestudy.name",3] ) 
 gtkEntrySetText(entry_casestudy_StartYear_simulation,  mat_cfg_general[mat_cfg_general[,1] == "casestudy.startsimulation",2]) 
 gtkEntrySetText(entry_casestudy_EndYear_simulation, mat_cfg_general[mat_cfg_general[,1] == "casestudy.endsimulation",2])
gtkEntrySetText(entry_casestudy_EndYear_forecast, mat_cfg_general_fore[mat_cfg_general_fore[,1] == "casestudy.endforecast",2]) 
#   mat_cfg_general_fore[mat_cfg_general_fore[,1] == "casestudy.startforecast",2] <<- en_y +1
#    <<- en_y_fore
 
    reload_interaction_table()
  
  nostockss <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostockss) <<- "Stocks"

reload_nostocks_table() 

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <<- "Fleet segment"

reload_nofleetsegments_table() 

gtkEntrySetText(entry_casestudy_NOFLEETSEGMENT, length(BMT_FLEETSEGMENTS))
gtkEntrySetText(entry_casestudy_NOSTOCK, length(BMT_SPECIES) )

gtkComboBoxSetActive(combo_species, 0)
gtkComboBoxSetActive(bmt_economicdata_fleet_combo, 0)


#    mat_cfg_EconomicIndicator_simu <<- data.frame(all_CFG_mat[, 1:2] )


#suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/reload_diagnosis.r", sep="")))					

bmt_wnd_sim$destroy()
wnd <- showMessageOK("        Case study loaded!        ")
gtkWidgetSetSensitive(BMTmain_window, T)
#print("Case study loaded!")

}