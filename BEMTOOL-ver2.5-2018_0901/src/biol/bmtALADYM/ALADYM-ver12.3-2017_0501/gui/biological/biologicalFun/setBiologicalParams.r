# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






setBiologicalParams<-function(BIOmatr) {

if (showCompTime)  {
setBiologicalParams_ptm <- proc.time()  
}

 if (!IN_BEMTOOL) {
suppressWarnings(source(paste(ALADYM_home, "/gui/ini.r", sep="") )	)	
}			

errorsVector <- c() 

#  BIOmatr <-          BIOparameters_table

if (!IN_BEMTOOL) {
geas_df <- BIOmatr[BIOmatr$Parameters == "gears", 2:ncol(BIOmatr)]
geas_df <- geas_df[!is.na(geas_df) ]
geas_df <- geas_df[geas_df != ""] 
FLEETSEGMENTS_names <<- c() 
} else {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
geas_df <- FLEETSEGMENTS_names 
}

if (!IN_BEMTOOL) {
Effort_TYPE <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Effort type", 2])   # E or FC
Production_TYPE <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Production type", 2])  # P or PP
} else {
Effort_TYPE <- "E"   # E or FC
Production_TYPE <- "P"  # P or PP
}

Mortality_TYPE <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Mortality type", 2])  # Z or F
F_TYPE <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "F type", 2])  # F or O
F_splitting_TYPE <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "F splitting type", 2])  # F or O

data_FR_comp <- data.frame(matrix("", ncol=length(geas_df), nrow=1), stringsAsFactors=F)

colnames(data_FR_comp) <- paste("F", 1:length(geas_df), sep="")

Selectivity_TYPE <- data_FR_comp

Discard_COMPONENT <- data_FR_comp     # Y, 0, NA
Discard_TYPE <- data_FR_comp    # V or RO
 
Discard_survivability_COMPONENT <- data_FR_comp    # Y, N
Discard_survivability_TYPE <- data_FR_comp          # C, DOS

Escape_survivability_COMPONENT <- data_FR_comp   # Y, N
Escape_survivability_TYPE <- data_FR_comp             # C, DOS
Escape_survivability_DOS_TYPE <- data_FR_comp    # O, V

EffortF_RELATIONSHIP <- data_FR_comp    # Y, N
EffortF_RELATIONSHIP_a <-  data_FR_comp     # Y, N
EffortF_RELATIONSHIP_b <- data_FR_comp   # Y, N

for (ge in 1:length(geas_df)) {
Discard_COMPONENT[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Discard component", ge+1] )     # Y, 0, NA
Discard_TYPE[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Discard type", ge+1] )   # V or RO
 
Discard_survivability_COMPONENT[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Discard survivability component", ge+1] )   # Y, N
Discard_survivability_TYPE[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Discard survivability type", ge+1] )             # C, DOS

Escape_survivability_COMPONENT[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Escape survivability component",  ge+1] )  # Y, N
Escape_survivability_TYPE[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Escape survivability type", ge+1] )             # C, DOS
Escape_survivability_DOS_TYPE[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Escape survivability DOS type",  ge+1] )      # O, V

EffortF_RELATIONSHIP[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Effort - F relationship", ge+1] )    # Y, N
EffortF_RELATIONSHIP_a[1,ge] <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Effort - F relationship a", ge+1] )     # Y, N
EffortF_RELATIONSHIP_b[1,ge] <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Effort - F relationship b", ge+1] )   # Y, N

if (Mortality_TYPE == "Z") {
   Selectivity_TYPE[1,ge] <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Selectivity type", ge+1])  # P parameters, A age, L  length
}

}




# ------------------------------------------------------------------- Mortality_TYPE
if ( Mortality_TYPE == "Z" ) {   
new_aldSimulation@enteringMortality <<- "Z"
gtkToggleButtonSetActive(radio_Zentry, T)
} else if ( Mortality_TYPE == "F") {
new_aldSimulation@enteringMortality <<- "F"
gtkToggleButtonSetActive(radio_Fentry, T)
} else {
new_aldSimulation@enteringMortality <<- "Z"
gtkToggleButtonSetActive(radio_Zentry, T)
Mortality_TYPE <- "Z"
}

if ( Mortality_TYPE == "F") {
if ( F_TYPE == "F" ) {   
new_aldSimulation@Ftype <<- "F"
gtkToggleButtonSetActive(radio_f_by_fleet, T)
} else if ( F_TYPE == "O") {
new_aldSimulation@Ftype <<- "O"
gtkToggleButtonSetActive(radio_f_overall, T)

if (F_splitting_TYPE == "CAA")  {
 new_aldSimulation@Fsplittingtype <<- "CAA"
gtkToggleButtonSetActive(radio_catch_by_age_splitting, T)
} else if (F_splitting_TYPE == "P") {
 new_aldSimulation@Fsplittingtype <<- "P"
gtkToggleButtonSetActive(radio_production_splitting, T)
}

} else {
new_aldSimulation@Ftype <<- "F"
gtkToggleButtonSetActive(radio_f_by_fleet, T)
F_TYPE <- "F"
}
}


if (!(Production_TYPE %in% c("P", "PP"))) {
     Production_TYPE <- "P"
}
if (!(Effort_TYPE %in% c("E", "FC"))) {
     Effort_TYPE <- "E"
}
#if (!(Discard_COMPONENT %in% c("Y", "0", "NA"))) {
#     Discard_COMPONENT <- "NA"
#}

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# READING FILES.....
# -------------------------------------------------------------------
# -------------------------------------------------------------------

up_file_Recr <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Recruitment file", 2])
up_file_M_males <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "M file - Male", 2])
up_file_M_females <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "M file - Female", 2])
up_file_Z <-   as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Z file", 2])
up_file_F <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "F file", 2])
up_file_F_overall <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "F overall file", 2])
up_file_catch_for_splitting <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Catch by age for splitting file", 2])
up_file_sel <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Selectivity file", 2])
up_file_sel_age <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Selectivity file by age", 2])
up_file_sel_length <- as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Selectivity file by length", 2])
up_file_P <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Production file", 2])
up_file_D <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Discard file", 2])
up_file_PP <-   as.character(BIOmatr[as.character(BIOmatr$Parameters) == "P production file", 2])
up_file_eff <-   as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Effort data file", 2])
up_file_fc <-    as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Fishing coefficient file", 2])
up_file_dis <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Reverse-ogive Discard file", 2])
up_file_dis_external <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "External Discard vector file", 2])
up_file_landing_obl <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "Landing obligation file", 2])
up_file_escape_surv_ext_vect <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "DOS External Escape survivability vector file", 2])

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# READING FILES..... END
# -------------------------------------------------------------------
# -------------------------------------------------------------------


   if (!IN_BEMTOOL) {
gtkEntrySetText(entry_StartYear_simulation, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "start - end simulation - end forecast"])))
gtkEntrySetText(entry_EndYear_simulation, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "start - end simulation - end forecast"])))
gtkEntrySetText(entry_EndYear_forecast, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "start - end simulation - end forecast"])))

gtkEntrySetText(entryVBF_M_lifespan, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Lifespan - Male"])) )
gtkEntrySetText(entryVBF_F_lifespan, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Lifespan - Female"])) ) 

change_startend_year()
all_years <<- c( as.numeric(as.character(gtkEntryGetText(entry_StartYear_simulation))):(as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation)))))  
GLO$L_number <- length(all_years) *12
}

gtkComboBoxSetActive(combo_L50dis_M, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L50 - Male"]))-1 )
gtkEntrySetText(entryOGIVEL50_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Maturity Ogive L50 - Male"])) )
gtkEntrySetText(entryOGIVEL50_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Maturity Ogive L50 - Male"])) )
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L50 - Male"])) != 3) {  
gtkEntrySetText(entryOGIVEL50_M_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Maturity Ogive L50 - Male"])) )   
gtkEntrySetText(entryOGIVEL50_M_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters == "Maturity Ogive L50 - Male"])) )
}

gtkComboBoxSetActive(combo_L75L25dis_M, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"]))-1 )
gtkEntrySetText(entryOGIVEL75L25_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"])) )
gtkEntrySetText(entryOGIVEL75L25_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"])) )
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"])) != 3) {  
gtkEntrySetText(entryOGIVEL75L25_M_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"])) )
gtkEntrySetText(entryOGIVEL75L25_M_b, as.numeric(as.character( BIOmatr$B[BIOmatr$Parameters == "Maturity Ogive L75L25 - Male"])) )
}

gtkComboBoxSetActive(combo_L50dis_F, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L50 - Female"]))-1 )
gtkEntrySetText(entryOGIVEL50_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Maturity Ogive L50 - Female"])) )
gtkEntrySetText(entryOGIVEL50_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Maturity Ogive L50 - Female"])) )
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L50 - Female"])) != 3) {  
gtkEntrySetText(entryOGIVEL50_F_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Maturity Ogive L50 - Female"])) )
gtkEntrySetText(entryOGIVEL50_F_b,  as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters == "Maturity Ogive L50 - Female"])) )
}

gtkComboBoxSetActive(combo_L75L25dis_F, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"]))-1 )
gtkEntrySetText(entryOGIVEL75L25_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"])))
gtkEntrySetText(entryOGIVEL75L25_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"]))) 
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"])) != 3) {
gtkEntrySetText(entryOGIVEL75L25_F_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"])))
gtkEntrySetText(entryOGIVEL75L25_F_b,  as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters == "Maturity Ogive L75L25 - Female"])))
}

# set the growth parameters 

gtkComboBoxSetActive(combo_t0dis_M,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Growth T0 - Male"]))-1)
gtkEntrySetText(entryVBFtzero_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Growth T0 - Male"])))
gtkEntrySetText(entryVBFtzero_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Growth T0 - Male"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Growth T0 - Male"])) != 3) {
gtkEntrySetText(entryVBFt0_M_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Growth T0 - Male"])))
gtkEntrySetText(entryVBFt0_M_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters == "Growth T0 - Male"])))
}
                                                                                                           
gtkComboBoxSetActive(combo_Kdis_M, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Growth K - Male"]))-1)
gtkEntrySetText(entryVBFK_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters == "Growth K - Male"])))
gtkEntrySetText(entryVBFK_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters == "Growth K - Male"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Growth K - Male"])) != 3) {
gtkEntrySetText(entryVBFK_M_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters == "Growth K - Male"])))
gtkEntrySetText(entryVBFK_M_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters == "Growth K - Male"])))
}

gtkComboBoxSetActive(combo_Linfdis_M,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth Linf - Male"]))-1)
gtkEntrySetText(entryVBFLinf_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Growth Linf - Male"])))
gtkEntrySetText(entryVBFLinf_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Growth Linf - Male"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth Linf - Male"])) != 3) {
gtkEntrySetText(entryVBFLinf_M_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="Growth Linf - Male"])))
gtkEntrySetText(entryVBFLinf_M_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="Growth Linf - Male"])))
}

gtkComboBoxSetActive(combo_t0dis_F,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth T0 - Female"]))-1)
gtkEntrySetText(entryVBFtzero_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Growth T0 - Female"])))
gtkEntrySetText(entryVBFtzero_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Growth T0 - Female"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth T0 - Female"])) != 3) {
gtkEntrySetText(entryVBFt0_F_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="Growth T0 - Female"])))
gtkEntrySetText(entryVBFt0_F_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="Growth T0 - Female"])))
}


gtkComboBoxSetActive(combo_Kdis_F,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth K - Female"]))-1)
gtkEntrySetText(entryVBFK_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Growth K - Female"])))
gtkEntrySetText(entryVBFK_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Growth K - Female"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth K - Female"])) != 3) {
gtkEntrySetText(entryVBFK_F_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="Growth K - Female"])))
gtkEntrySetText(entryVBFK_F_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="Growth K - Female"])))
}

gtkComboBoxSetActive(combo_Linfdis_F,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth Linf - Female"]))-1)
gtkEntrySetText(entryVBFLinf_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Growth Linf - Female"])))
gtkEntrySetText(entryVBFLinf_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Growth Linf - Female"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Growth Linf - Female"])) != 3) {
gtkEntrySetText(entryVBFLinf_F_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="Growth Linf - Female"])))
gtkEntrySetText(entryVBFLinf_F_b, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="Growth Linf - Female"])))
}

gtkComboBoxSetActive(combo_OFFSPRING_rand,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Offspring R"]))-1)
gtkEntrySetText(entryOFFSPRING_rand_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Offspring R"])))
gtkEntrySetText(entryOFFSPRING_rand_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Offspring R"])))
if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Offspring R"])) != 3) {
gtkEntrySetText(entryOFFSPRING_rand_a, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="Offspring R"])))
gtkEntrySetText(entryOFFSPRING_rand_b,as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="Offspring R"])))
}


gtkToggleButtonSetActive(chkCalibration, as.logical(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Recruitment calibration"])) )
if (as.logical(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Recruitment calibration"])) ) {
  gtkEntrySetText(entry_minrec, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Recruitment calibration"])))
  gtkEntrySetText(entry_maxrec, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Recruitment calibration"])))
  gtkWidgetSetSensitive(entry_maxrec, TRUE)
  gtkWidgetSetSensitive(entry_minrec, TRUE)
}  else {
  gtkWidgetSetSensitive(entry_maxrec, FALSE)
  gtkWidgetSetSensitive(entry_minrec, FALSE)
}

gtkComboBoxSetActive(combo_SS, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Spawners"]))-1 )
gtkEntrySetText(entry_delaySS, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Spawners"])) )

if (!IN_BEMTOOL) {
BMT_SPECIES <<-  as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Scientific name"])
 suppressWarnings(source(paste(ALADYM_home, "/src/paths.r", sep=""))  )
gtkEntrySetText(entrySpecies, as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Scientific name"]) )  
gtkEntrySetText(entryGSA, as.character(BIOmatr$Distribution[BIOmatr$Parameters =="GSA"]) )    
gtkEntrySetText(entryYearsForAverage, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Number of years for average"])) )  
gtkEntrySetText(entry_SR_value, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Sex ratio"])) )

gtkEntrySetText(entryAB_A_M, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="L-W parameters - Male"])) )
gtkEntrySetText(entryAB_B_M, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="L-W parameters - Male"])) )
gtkEntrySetText(entryAB_A_F, as.numeric(as.character(BIOmatr$A[BIOmatr$Parameters =="L-W parameters - Female"])) )
gtkEntrySetText(entryAB_B_F, as.numeric(as.character(BIOmatr$B[BIOmatr$Parameters =="L-W parameters - Female"])) ) 

}

if (!IN_BEMTOOL | (IN_BEMTOOL & SAtool == "none") ) {
gtkEntrySetText(entry_agerange_M_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Age range of F calculated - Male"])) )
gtkEntrySetText(entry_agerange_M_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Age range of F calculated - Male"])) )
gtkEntrySetText(entry_agerange_F_min, as.numeric(as.character(BIOmatr$Min[BIOmatr$Parameters =="Age range of F calculated - Female"])) )
gtkEntrySetText(entry_agerange_F_max, as.numeric(as.character(BIOmatr$Max[BIOmatr$Parameters =="Age range of F calculated - Female"])) )
}



gtkEntrySetText(entrySpeciesCommonName, as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Common name"]) )
gtkEntrySetText(entryYearsToBePreSimulated, as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Years to be presimulated"])) )
tr_value <- as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "Tr"]))

gtkEntrySetText(entryOFFSPRING_tr, tr_value)

# BIOmatr[,2] <- as.numeric(as.character(BIOmatr[,2]))

offspring_prop_df <<- BIOmatr[as.character(BIOmatr$Parameters) == "Offspring proportion",c(2:13)]
 colnames(offspring_prop_df) <<- c(MONTHS)
 reload_monthlyOffsprings()


monthly.survivability_df <<- BIOmatr[as.character(BIOmatr$Parameters) == "Survivability proportion",c(2:13)]
 colnames(monthly.survivability_df) <<- c(MONTHS)
reload_monthlySurvivability() 
 

# -------------------------------------------------------------------------------------------------------------------
# loading recruitment from file

if (!IN_BEMTOOL | (IN_BEMTOOL & SAtool == "none") ) {

stockrecruitment.SRvector <<- try(read.csv(up_file_Recr, sep=";"))

if (class(stockrecruitment.SRvector) != "try-error") {

stockrecruitment.SRvector.seed <<- stockrecruitment.SRvector$seed[1] 
stockrecruitment.SRvector <<- stockrecruitment.SRvector[,colnames(stockrecruitment.SRvector) != "seed"] 

check_ <- check_input("RECRUITMENT_VECTOR", stockrecruitment.SRvector)

if (check_$result == "KO") {
    errorsVector <- c(errorsVector, "Impossible to load recruitment data.")
    #showError("Impossible to load recruitment data! Inconsistent settings and data.")
} else { 
#------------------------------------------ load the file

gtkEntrySetText(entry_OFFSPRING_seedvalue, stockrecruitment.SRvector.seed)
recruitments <<- list()
recruitmentIndex <<- 0
add.recruitments()
  recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments)) {
    iter <-  recruitments.model$append()$iter
     recruitments.model$set(iter,0, recruitments[[i]]$year)
    #print(paste("in model:", as.character(recruitments[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments.model$set(iter, e, as.double(recruitments[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
       #print(paste("in model:", recruitments[[i]][e]) )
    }
     recruitments.model$set(iter,13,TRUE)
  } 

   recruitments.treeview$destroy()
 recruitments.treeview <<- gtkTreeViewNewWithModel( recruitments.model)
 recruitments.treeview$setRulesHint(TRUE)
 recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns( recruitments.treeview) 
recruitment.sw$add(recruitments.treeview)

}
} else {
print(paste("--------------------- Error in reading RECRUITMENT file [path: \"", up_file_Recr, "\"]", sep =""), quote=F)

}

}



# -------------------------------------------------------------------------------------------------------------------
# loading gears from configuration file

if (!IN_BEMTOOL) {
geas <- geas_df

for (gg in 1:length(geas)) {

string_fleet <- geas[gg]
m <- regexec("[[:alnum:]+[:punct:]+[:blank:]+]+", string_fleet)

 #gtkEntrySetText(entryGearName, as.character(regmatches(string_fleet, m)))
 print(paste("--------------------- Adding fleet segment from configuration file:", geas[gg]) , quote=F)
 fleetsegmentName <- add_gear_from_config(as.character(regmatches(string_fleet, m)))                                                            # <----------------------------------  time consuming
# fleetsegmentName <- add_gear()                                                                                  # <----------------------------------  time consuming
 FLEETSEGMENTS_names <<- c(FLEETSEGMENTS_names, fleetsegmentName)
 gtkComboBoxInsertText(combo_fleetsegments, (length(FLEETSEGMENTS_names)-1), FLEETSEGMENTS_names[length(FLEETSEGMENTS_names)])
 gtkComboBoxInsertText(combo_fleetsegments_fore, (length(FLEETSEGMENTS_names)-1), FLEETSEGMENTS_names[length(FLEETSEGMENTS_names)])
}
}
#

if (!IN_BEMTOOL | (IN_BEMTOOL & SAtool == "none") ) {
# -------------------------------------------------------------------------------------------------------------------
# loading natural mortality for males
mortality.Mvector.males <<- try(read.csv(up_file_M_males, sep=";", na.strings = "") )

if (class(mortality.Mvector.males) != "try-error") { 

check_ <- check_input("NATURAL_MORTALITY_VECTOR_M", mortality.Mvector.males)

if (check_$result == "KO") {
    errorsVector <- c(errorsVector, "Impossible to load natural mortality data (males).")
    # showError("Impossible to load natural mortality data (males)! Inconsistent settings and data.")
} else { 
#------------------------------------------ load the file

Mvector_M <<- list()
Mvector_MIndex <<- 0
add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter, 0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter, 2,TRUE)
  } 
  Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)
Mvector_M.sw$add(Mvector_M.treeview)

}
} else {
print(paste("--------------------- Error in reading NATURAL MORTALITY file [path: \"", up_file_M_males, "\"]", sep =""), quote=F)

}




# -------------------------------------------------------------------------------------------------------------------
# loading natural mortality for females

mortality.Mvector.females <<- try(read.csv(up_file_M_females, sep=";", na.strings = "") )

if (class(mortality.Mvector.females  ) != "try-error") {

check_ <- check_input("NATURAL_MORTALITY_VECTOR_F", mortality.Mvector.females)

if (check_$result == "KO") {
    errorsVector <- c(errorsVector, "Impossible to load natural mortality data (females).")
    #showError("Impossible to load natural mortality data (females)! Inconsistent settings and data.")
} else { 
#------------------------------------------ load the file
Mvector_F <<- list()
Mvector_FIndex <<- 0
add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 
 Mvector_F.treeview$destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)

}

}  else {
print(paste("--------------------- Error in reading NATURAL MORTALITY file [path: \"", up_file_M_females, "\"]", sep =""), quote=F)

}

}

# -------------------------------------------------------------------------------------------------------------------
# loading total mortality
if (!IN_BEMTOOL | (IN_BEMTOOL & SAtool == "none") ) {
if (Mortality_TYPE == "Z") {   

mortality.Zvector <<- try(read.csv(up_file_Z, sep=";", na.strings = ""))

if (class(mortality.Zvector  ) != "try-error") {
mortality.Zvector.males <<- mortality.Zvector[mortality.Zvector$sex=="M",colnames(mortality.Zvector) != "sex"]
mortality.Zvector.females <<- mortality.Zvector[mortality.Zvector$sex=="F",colnames(mortality.Zvector) != "sex"]

mortality.Zvector.males.seed <<- mortality.Zvector.males$seed[1] 
mortality.Zvector.males <<- mortality.Zvector.males[,colnames(mortality.Zvector.males) != "seed"] 

mortality.Zvector.females.seed <<- mortality.Zvector.females$seed[1] 
mortality.Zvector.females <<- mortality.Zvector.females[,colnames(mortality.Zvector.females) != "seed"] 

 check_f <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.females )
 
  check_m <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.males)

if (check_f$result == "KO") {
        errorsVector <- c(errorsVector, "Impossible to load total mortality data (females).")
    #showError("Impossible to load total mortality data! Inconsistent settings and data.")
} else if (check_m$result == "KO") {
    errorsVector <- c(errorsVector, "Impossible to load total mortality data (males).")
    #showError("Impossible to load total mortality data! Inconsistent settings and data.")
} else {  
#------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_M, mortality.Zvector.males.seed)
Zvector_M <<- list()
Zvector_MIndex <<- 0
add.Zvector_M()
  Zvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_M)) {
    iter <-  Zvector_M.model$append()$iter
     Zvector_M.model$set(iter,0, Zvector_M[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_M.model$set(iter, e, as.double(Zvector_M[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     Zvector_M.model$set(iter,13,TRUE)
  } 

  Zvector_M.treeview$destroy()
 Zvector_M.treeview <<- gtkTreeViewNewWithModel( Zvector_M.model)
 Zvector_M.treeview$setRulesHint(TRUE)
 Zvector_M.treeview$getSelection()$setMode("single")
Zvector_M.add_columns( Zvector_M.treeview)

Zvector_M.sw$add(Zvector_M.treeview)


#------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_F, mortality.Zvector.females.seed)
Zvector_F <<- list()
Zvector_FIndex <<- 0
add.Zvector_F()
  Zvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_F)) {
    iter <-  Zvector_F.model$append()$iter
     Zvector_F.model$set(iter,0, Zvector_F[[i]]$year)
    #print(paste("in model:", as.character(Zvector_F[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_F.model$set(iter, e, as.double(Zvector_F[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
     #  print(paste("in model:", Zvector_F[[i]][e]) )
    }
     Zvector_F.model$set(iter,13,TRUE)
  } 

  Zvector_F.treeview$destroy()
 Zvector_F.treeview <<- gtkTreeViewNewWithModel( Zvector_F.model)
 Zvector_F.treeview$setRulesHint(TRUE)
 Zvector_F.treeview$getSelection()$setMode("single")
Zvector_F.add_columns( Zvector_F.treeview)
Zvector_F.sw$add(Zvector_F.treeview)

}

} else {
print(paste("--------------------- Error in reading TOTAL MORTALITY file [path: \"", up_file_Z, "\"]", sep =""), quote=F)

}

} else {
    loaded_Fmortality <- try(read.csv(up_file_F, sep=";", na.strings = "") )  
    print("--------------------- Loading fishing mortality configuration", quote=F )

    loaded_Fmortality_overall <- try(read.csv(up_file_F_overall, sep=";", na.strings = "") )  
    print("--------------------- Loading fishing mortality overall configuration", quote=F )



}

}         


n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))  

first_age_mal <- 0
first_age_fem <- 0

   n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

 l_inf_F <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max)) 
  l_inf_M <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))   
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))
l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))

# inizialize all the matrices to be imported
if (Mortality_TYPE == "Z") {
if (nchar(up_file_sel ) > 1 ) {
   if (!is.na(up_file_sel)) {
loaded_selectivity <- try(read.csv( up_file_sel, sep=";", na.strings = "")  )
print("--------------------- Loading selectivity configuration", quote=F)
   }
}

if (nchar(up_file_sel_age ) > 1) {
   if (!is.na(up_file_sel_age)) {
loaded_selectivity_age <- try(read.csv( up_file_sel_age, sep=";", na.strings = "")  )
print("--------------------- Loading selectivity by age configuration", quote=F)
}
}

if (nchar(up_file_sel_length ) > 1) {
    if (!is.na(up_file_sel_length)) {
loaded_selectivity_length <- try(read.csv( up_file_sel_length, sep=";", na.strings = "")  )
print("--------------------- Loading selectivity by length configuration", quote=F)
}
}

} else {
    if (F_TYPE == "O" ) {
    
        loaded_CatchatAGE <- try(read.csv( up_file_catch_for_splitting, sep=";", na.strings = "")  )
    
         FF_overall_mat <- try(read.csv( up_file_F_overall, sep=";", na.strings = "")  )

         if (class(FF_overall_mat) != "try-error")  {
         FF_overall_mat <- FF_overall_mat[FF_overall_mat$Sex == "F", 1: (ncol(FF_overall_mat) -1)]
         FF_overall_mat <- t(FF_overall_mat)
         FF_overall_mat <- FF_overall_mat[2:nrow(FF_overall_mat), ] 
         FF_overall_mat <- data.frame(cbind(years, data.frame(FF_overall_mat)))
         colnames(FF_overall_mat) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") ) 
         new_aldSimulation@fishingmortality.overall.F <<- FF_overall_mat
         FF_overall_matrix  <<- FF_overall_mat
         reload_fishingmortalityF_overall() 
         } else {
          reload_EMPTY_fishingmortalityF_overall()
         }
       
         FM_overall_mat <- try(read.csv( up_file_F_overall, sep=";", na.strings = "")  )  
          if (class(FM_overall_mat) != "try-error")  {
         FM_overall_mat <- FM_overall_mat[FM_overall_mat$Sex == "M", 1: (ncol(FM_overall_mat) -1)]
         FM_overall_mat <- t(FM_overall_mat)
         FM_overall_mat <- FM_overall_mat[2:nrow(FM_overall_mat), ] 
         FM_overall_mat <- data.frame(cbind(years, data.frame(FM_overall_mat)))
         colnames(FM_overall_mat) <- c("year", paste("age",  c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") ) 
         new_aldSimulation@fishingmortality.overall.M <<- FM_overall_mat
            FM_overall_matrix  <<- FM_overall_mat
         reload_fishingmortalityM_overall() 
          } else {
          reload_EMPTY_fishingmortalityM_overall()
         }
    
    }


}

if (IN_BEMTOOL) {
  Effort_TYPE <- "E"
  Production_TYPE <- "P"
}

    if (!IN_BEMTOOL) {
if (Effort_TYPE == "E") {
loaded_effortvariables <- try(read.csv( up_file_eff, sep=";", na.strings = "")  )
print("--------------------- Loading effort data configuration", quote=F)
} else {
loaded_fishcoeff <- try(read.csv( up_file_fc, sep=";", na.strings = "") ) 
print("--------------------- Loading fishing coefficient configuration", quote=F)
}
}


if (Production_TYPE == "P") {
    if (!IN_BEMTOOL) {
loaded_production <- try(read.csv(up_file_P, sep=";", na.strings = "") ) 
print("--------------------- Loading production configuration", quote=F )
}
loaded_monthly_discard <- try(read.csv(up_file_D, sep=";", na.strings = "") ) 
print("--------------------- Loading discard configuration", quote=F )
} else {
    if (!IN_BEMTOOL) {
loaded_PP <- try(read.csv( up_file_PP, sep=";", na.strings = "") ) 
print("--------------------- Loading p production configuration", quote=F )
}
} 

 
#} 

if (any(Discard_COMPONENT[!is.na(Discard_COMPONENT)] == "Y")) {
   Discard_TYPE_notNA <- Discard_TYPE[which(!is.na(Discard_COMPONENT) & Discard_COMPONENT != "N" & Discard_COMPONENT != "0")]
  
  if (length(Discard_TYPE_notNA[!is.na(Discard_TYPE_notNA)])> 0) { 
if (any(Discard_TYPE_notNA[!is.na(Discard_TYPE_notNA)] == "RO")) {
loaded_discard <- try(read.csv(up_file_dis, sep=";", na.strings = "") ) 
print("--------------------- Loading discard reverse ogive configuration", quote=F )
} 

if (any(Discard_TYPE_notNA == "V")) {
loaded_discard_extvector <- try(read.csv(up_file_dis_external, sep=";", na.strings = "") ) 
print("--------------------- Loading discard external vector configuration", quote=F )
}

}

loaded_landing_obligation <- try(data.frame(read.csv(up_file_landing_obl, sep=";", na.strings = "")) ) 
print("--------------------- Loading landing obligation configuration", quote=F )

}


if (any(Escape_survivability_COMPONENT[!is.na(Escape_survivability_COMPONENT)] == "Y")) {
     Escape_survivability_DOS_TYPE_notNA <- Escape_survivability_DOS_TYPE[which(!is.na(Escape_survivability_COMPONENT) & Escape_survivability_COMPONENT != "N")]
if (length(Escape_survivability_DOS_TYPE_notNA[!is.na(Escape_survivability_DOS_TYPE_notNA)])> 0) {
if (any(Escape_survivability_DOS_TYPE_notNA[!is.na(Escape_survivability_DOS_TYPE_notNA)] == "V")) {
  loaded_escape_surv_external_vector <- try(data.frame(read.csv(up_file_escape_surv_ext_vect, sep=";", na.strings = "")) ) 
print("--------------------- Loading escape survivability configuration", quote=F )
}
}

}

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?hence FLEET SEGMENTS



    
    
 if (!IN_BEMTOOL) {  # if not in BEMTOOL the code reconstruct all the fleet segment from the GUI

for (fs in 1:length(FLEETSEGMENTS_names) ) {

effF <- data.frame(matrix("", nrow=1, ncol=3))
colnames(effF) <- c("relationship_type", "a", "b")
effF$relationship_type <- as.character(EffortF_RELATIONSHIP[1,fs])
effF$a <- as.numeric(as.character(EffortF_RELATIONSHIP_a[1,fs] ))
effF$b <- as.numeric(as.character(EffortF_RELATIONSHIP_b[1,fs] ))



FleetList_simulation[[fs]]@EffortF.relationship <<- effF
FleetList_forecast[[fs]]@EffortF.relationship <<- effF

# when Production is available the P production is calculated
print(paste("Loading", FLEETSEGMENTS_names[fs], "..."), quote=F)

if (Production_TYPE == "P") {
FleetList_simulation[[fs]]@production.datatype <<- "Production data"
FleetList_forecast[[fs]]@production.datatype <<- "Production data"
} else {
FleetList_simulation[[fs]]@production.datatype <<- "P production"
FleetList_forecast[[fs]]@production.datatype <<- "P production"
}

 if (Effort_TYPE == "E") {
FleetList_simulation[[fs]]@effort.datatype <<- "Effort data"
FleetList_forecast[[fs]]@effort.datatype <<- "Effort data"  
 } else {
FleetList_simulation[[fs]]@effort.datatype <<- "Fishing coefficient"
FleetList_forecast[[fs]]@effort.datatype <<- "Fishing coefficient"
 }
 
 if (Production_TYPE == "P" ) {
if (class(loaded_production ) != "try-error") {
prod_fs <- loaded_production[as.character(loaded_production$fleet_segment) == FLEETSEGMENTS_names[fs],]
#print(prod_fs)
prodMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
produ <- prod_fs$PRODUCTION[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) { prodMatr <- rbind(prodMatr, c(years[yea], prod_fs$PRODUCTION[1], produ))  
} else { prodMatr <- rbind(prodMatr, c(years[yea],"", produ)) }
}
colnames(prodMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@production.vector <<- prodMatr 

prodMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
# print(vess)
 prodMatr_fore <- rbind(prodMatr_fore, c(years_forecast[yea], produ))  
}
colnames(prodMatr_fore) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@production.vector <<- prodMatr_fore 

}

}



# when Effort data are available the Fishing coefficient is calculated
if (Effort_TYPE == "E" ) {
if (class(loaded_effortvariables ) != "try-error") {
# -------------------------------------------------------------------------------------------------------------------  loading effort data from file
# VESSELSSSSSSSSSSSSSSSSSSSSSS
data_fs <- loaded_effortvariables[as.character(loaded_effortvariables$fleet_segment) == FLEETSEGMENTS_names[fs],]
vesselsMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
vesselsMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
# DAYSSSSSSSSSSSSSSSSSSSSSSSSS
daysMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
daysMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
# GTSSSSSSSSSSSSSSSSSSSSSSS
gtMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
gtMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))

for (yea in 1:length(years) ) {

vess <- data_fs$VESSELS[(2+(yea-1)*12):(1+(yea)*12)]
if (yea==1) { vesselsMatr <- rbind(vesselsMatr, c(years[yea], data_fs$VESSELS[1], vess)) 
} else { vesselsMatr <- rbind(vesselsMatr, c(years[yea],"", vess)) }

dayss <- data_fs$DAYS[(2+(yea-1)*12):(1+(yea)*12)]
if (yea==1) { daysMatr <- rbind(daysMatr, c(years[yea], data_fs$DAYS[1], dayss)) 
} else { daysMatr <- rbind(daysMatr, c(years[yea],"", dayss)) }

gts <- data_fs$GT[(2+(yea-1)*12):(1+(yea)*12)]
if (yea==1) { gtMatr <- rbind(gtMatr, c(years[yea], data_fs$GT[1], gts)) 
} else { gtMatr <- rbind(gtMatr, c(years[yea],"", gts)) }

}
colnames(vesselsMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@vessels.vector <<- vesselsMatr 

colnames(daysMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@days.vector <<- daysMatr 

colnames(gtMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@gt.vector <<- gtMatr 


for (yea in 1:length(years_forecast) ) {
# print(vess)
 vesselsMatr_fore <- rbind(vesselsMatr_fore, c(years_forecast[yea], vess))
  daysMatr_fore <- rbind(daysMatr_fore, c(years_forecast[yea], dayss))
   gtMatr_fore <- rbind(gtMatr_fore, c(years_forecast[yea], gts))  
}

colnames(vesselsMatr_fore) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@vessels.vector <<- vesselsMatr_fore 

colnames(daysMatr_fore) <- c("year",		MONTHS)
FleetList_forecast[[fs]]@days.vector <<- daysMatr_fore 

colnames(gtMatr_fore) <- c("year",		MONTHS)
FleetList_forecast[[fs]]@gt.vector <<- gtMatr_fore 

}
} 
}     # END IF
}   # end stand alone situation ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


# always run in BMT and ALADYM


if (Mortality_TYPE == "F") {
   
   if (F_TYPE == "O") {
    
    F_overall_matrix <<- try(read.csv( up_file_F_overall, sep=";", na.strings = "")  )  
     
     if (F_splitting_TYPE == "CAA") {         
   if ( class(F_overall_matrix) != "try-error" & class(loaded_CatchatAGE ) != "try-error" ) {
      loaded_Fmortality <- split_F(F_splitting_TYPE)
#   loaded_Fmortality  <- splitF_bymeansCATCHatAGE(F_overall_matrix, loaded_CatchatAGE)

   }
   } else {
    if ( class(F_overall_matrix) != "try-error" ) {
     loaded_Fmortality <- split_F(F_splitting_TYPE)
#   loaded_Fmortality  <- splitF_bymeansCATCHatAGE(F_overall_matrix, loaded_CatchatAGE)

   }
   
   }
   }
 
 }



for (fs in 1:length(FLEETSEGMENTS_names) ) {

if (Selectivity_TYPE[1,fs] == "P") {
FleetList_simulation[[fs]]@selectivity.mode <<- "params" 
FleetList_forecast[[fs]]@selectivity.mode <<- "params" 
} else if (Selectivity_TYPE[1,fs] == "A") {
FleetList_simulation[[fs]]@selectivity.mode <<- "age"
FleetList_forecast[[fs]]@selectivity.mode <<- "age"  
} else if (Selectivity_TYPE[1,fs] == "L") {
FleetList_simulation[[fs]]@selectivity.mode <<- "length" 
FleetList_forecast[[fs]]@selectivity.mode <<- "length" 
}

 if (as.character(Discard_COMPONENT[1,fs]) == "Y") {
FleetList_simulation[[fs]]@discard.calculation <<- "YES"
FleetList_forecast[[fs]]@discard.calculation <<- "YES"

 if (as.character(Discard_TYPE[1,fs]) == "V") {
FleetList_simulation[[fs]]@discard.datatype <<- "External vector" 
FleetList_forecast[[fs]]@discard.datatype <<- "External vector"
 } else {
FleetList_simulation[[fs]]@discard.datatype <<- "Reverse ogive"
FleetList_forecast[[fs]]@discard.datatype <<- "Reverse ogive"
 }
 
 } else if (as.character(Discard_COMPONENT[1,fs]) == "0") {
FleetList_simulation[[fs]]@discard.calculation <<- "0"
FleetList_forecast[[fs]]@discard.calculation <<- "0"

 } else {
FleetList_simulation[[fs]]@discard.calculation <<- "NA" 
FleetList_forecast[[fs]]@discard.calculation <<- "NA" 
 }
 
 
 
  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?set discard survivability component
 if (as.character(Discard_survivability_COMPONENT[1,fs]) == "Y") {
FleetList_simulation[[fs]]@discard.survivability.calculation <<- "Y" 
FleetList_forecast[[fs]]@discard.survivability.calculation <<- "Y" 

  if (as.character(Discard_survivability_TYPE[1,fs]) == "C") {
FleetList_simulation[[fs]]@discard.survivability.datatype <<- "C" 
FleetList_forecast[[fs]]@discard.survivability.datatype <<- "C" 
 } else if (as.character(Discard_survivability_TYPE[1,fs]) == "DOS") {
FleetList_simulation[[fs]]@discard.survivability.datatype <<- "DOS"
FleetList_forecast[[fs]]@discard.survivability.datatype <<- "DOS"  
 }

dat_fr <- data.frame(matrix("", nrow=1, ncol=2)) 
colnames(dat_fr) <- c("param1_or_M", "param2_or_F")
 dat_fr$param1_or_M <-  as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("Discard survivability parameters_F", fs, sep="") , 2]))
 dat_fr$param2_or_F <-  as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("Discard survivability parameters_F", fs, sep=""), 3]))
FleetList_simulation[[fs]]@discard.survivability.params <<- dat_fr  
FleetList_forecast[[fs]]@discard.survivability.params <<- dat_fr  
 
 } else {
FleetList_simulation[[fs]]@discard.survivability.calculation <<- "N" 
FleetList_forecast[[fs]]@discard.survivability.calculation <<- "N" 

 }
 
 
 
 
 
 
 
  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?set escape survivability component
 if (as.character(Escape_survivability_COMPONENT[1,fs]) == "Y") {
FleetList_simulation[[fs]]@escape.survivability.calculation <<- "Y" 
FleetList_forecast[[fs]]@escape.survivability.calculation <<- "Y" 

  if (as.character(Escape_survivability_TYPE[1,fs]) == "C") {
FleetList_simulation[[fs]]@escape.survivability.datatype <<- "C" 
FleetList_forecast[[fs]]@escape.survivability.datatype <<- "C" 

dat_fr <- data.frame(matrix("", nrow=1, ncol=2)) 
colnames(dat_fr) <- c("males", "females")
 dat_fr$males <- as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("Escape survivability costant parameters_F", fs, sep="") , 2]))
 dat_fr$females <- as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("Escape survivability costant parameters_F", fs, sep=""), 3]))
FleetList_simulation[[fs]]@escape.survivability.constant <<- dat_fr
FleetList_forecast[[fs]]@escape.survivability.constant <<- dat_fr    

 } else if (as.character(Escape_survivability_TYPE[1,fs]) == "DOS") {
 
FleetList_simulation[[fs]]@escape.survivability.datatype <<- "DOS"  
FleetList_forecast[[fs]]@escape.survivability.datatype <<- "DOS"  

 if ( as.character(Escape_survivability_DOS_TYPE[1,fs]) == "O") {
FleetList_simulation[[fs]]@escape.survivability.DOS.datatype <<- "O"
FleetList_forecast[[fs]]@escape.survivability.DOS.datatype <<- "O"
    
  dat_fr <- data.frame(matrix("", nrow=1, ncol=2)) 
colnames(dat_fr) <- c("param1", "param2")
 dat_fr$param1 <- as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("DOS Escape survivability Ogive parameters_F", fs, sep=""), 2]))
 dat_fr$param2 <- as.numeric(as.character(BIOmatr[BIOmatr$Parameters == paste("DOS Escape survivability Ogive parameters_F", fs, sep=""), 3]))
FleetList_simulation[[fs]]@escape.survivability.DOS.ogiveparams <<- dat_fr   
FleetList_forecast[[fs]]@escape.survivability.DOS.ogiveparams <<- dat_fr   
    
 } else if (as.character(Escape_survivability_DOS_TYPE[1,fs]) == "V") {
    FleetList_simulation[[fs]]@escape.survivability.DOS.datatype <<- "V"
       FleetList_forecast[[fs]]@escape.survivability.DOS.datatype <<- "V"
 
 if (class(loaded_escape_surv_external_vector ) != "try-error") {
 # load external discard vector from configuration file
 escape_surv_extvector_fs <- loaded_escape_surv_external_vector[,c(1, (fs+1), (length(FLEETSEGMENTS_names)+2))]

 dataframe_escapesurv_extvector_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M)))
 colnames(dataframe_escapesurv_extvector_M) <-  c(paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_escapesurv_extvector_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F)))
  colnames(dataframe_escapesurv_extvector_F) <-  c( paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
       to_add <- data.frame(matrix(  escape_surv_extvector_fs[escape_surv_extvector_fs[ncol(escape_surv_extvector_fs)]=="M",2], nrow=1)) 
       colnames(to_add) <- c( paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_escapesurv_extvector_M <- rbind(dataframe_escapesurv_extvector_M, to_add)
       to_add <- data.frame(matrix(  escape_surv_extvector_fs[escape_surv_extvector_fs[ncol(escape_surv_extvector_fs)]=="F",2], nrow=1)) 
       colnames(to_add) <- c( paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_escapesurv_extvector_F <- rbind(dataframe_escapesurv_extvector_F, to_add) 

 FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.M <<- dataframe_escapesurv_extvector_M
 FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.F <<- dataframe_escapesurv_extvector_F
 
  FleetList_forecast[[fs]]@escape.survivability.DOS.ext_vect.M <<- dataframe_escapesurv_extvector_M
 FleetList_forecast[[fs]]@escape.survivability.DOS.ext_vect.F <<- dataframe_escapesurv_extvector_F

}
} 

 } 

 } else if (as.character(Escape_survivability_COMPONENT[1,fs]) == "N") {
FleetList_simulation[[fs]]@escape.survivability.calculation <<- "N"
FleetList_forecast[[fs]]@escape.survivability.calculation <<- "N"  
 }
 
 
 
 
 
if (any(Discard_COMPONENT[!is.na(Discard_COMPONENT)] == "Y")) {
if (class(loaded_monthly_discard ) != "try-error") {
month_discard_fs <- loaded_monthly_discard[as.character(loaded_monthly_discard$fleet_segment) == FLEETSEGMENTS_names[fs],]
month_discard_Matr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
month_disc <- month_discard_fs$DISCARD[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) { month_discard_Matr <- rbind(month_discard_Matr, c(years[yea], month_discard_fs$DISCARD[1], month_disc))  
} else { month_discard_Matr <- rbind(month_discard_Matr, c(years[yea],"", month_disc)) }
}
colnames(month_discard_Matr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@monthly.discard.vector <<- month_discard_Matr 

month_discard_Matr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
# print(vess)
 month_discard_Matr_fore <- rbind(month_discard_Matr, c(years_forecast[yea], month_disc))  
}
colnames(month_discard_Matr_fore) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@monthly.discard.vector <<- month_discard_Matr_fore 

} 
} else {
  month_discard_Matr <- data.frame(matrix(0, nrow=length(years), ncol=(length(MONTHS)+2)))
  colnames(month_discard_Matr) <- c("year",	"seed",	MONTHS)
  month_discard_Matr$year <- years
FleetList_simulation[[fs]]@monthly.discard.vector <<- month_discard_Matr 

  month_discard_Matr_fore <- data.frame(matrix(0, nrow=length(years_forecast), ncol=(length(MONTHS)+1)))
  colnames(month_discard_Matr_fore) <- c("year",	MONTHS)
  month_discard_Matr_fore$year <- years_forecast
FleetList_forecast[[fs]]@monthly.discard.vector <<- month_discard_Matr_fore 
}

}


# always run in BMT and ALADYM  
if (Production_TYPE == "P") {
prod_data <- get_production_data()
 if (any(Discard_COMPONENT[!is.na(Discard_COMPONENT)] == "Y")) {
disc_data <- get_discard_data()
disc_data$Discard <- as.numeric(as.character(disc_data$Discard))
disc_data$Discard[is.na(disc_data$Discard)] <- 0
prod_data$Production <- as.numeric(as.character(prod_data$Production))  +  as.numeric(as.character(disc_data$Discard))
}
p_production_mat <- P_production_calc.gui(prod_data,forecast)
}

if (Effort_TYPE == "E") {
eff_data <- get_effort_data()
#print(eff_data)
#print(forecast)
#print(all_years)
fact_mat <- fact_calc.gui(eff_data,"N", all_years, forecast)
}



for (fs in 1:length(FLEETSEGMENTS_names) ) {
# ------------------------------------------------------------------------------------------------------------------- load selectivity from configuration file
if (Mortality_TYPE == "Z") {
if ( as.character(Selectivity_TYPE[1,fs] ) == "P")  {
if (class(loaded_selectivity ) != "try-error") {
   selectivity_fs <- loaded_selectivity[as.character(loaded_selectivity$fleet_segment) == FLEETSEGMENTS_names[fs],]
   selectivity_fs[1,1] <- ""

fleet.selectivity <<- selectivity_fs[, 1:(ncol(selectivity_fs)-1)]
FleetList_simulation[[fs]]@selectivity.vector <<- fleet.selectivity  
} else {

sel_matrix <- data.frame(matrix(-1, nrow=((length(years)*12) +1), ncol=8))
 
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
   
fleet.selectivity <<- sel_matrix
FleetList_simulation[[fs]]@selectivity.vector <<- fleet.selectivity  
}

 #dataframe_sel_age_M <- data.frame(matrix(-1, nrow=1, ncol=(n_ages_M)))
# colnames(dataframe_sel_age_M) <-  c( paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
#  dataframe_sel_age_F <- data.frame(matrix(-1, nrow=1, ncol=(n_ages_F)))
#  colnames(dataframe_sel_age_F) <-  c( paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
#
#   FleetList_simulation[[fs]]@SelectivityAge.M.vector   <<- dataframe_sel_age_M
#   FleetList_simulation[[fs]]@SelectivityAge.F.vector   <<- dataframe_sel_age_F
#
#   FleetList_forecast[[fs]]@SelectivityAge.M.vector   <<- dataframe_sel_age_M
#   FleetList_forecast[[fs]]@SelectivityAge.F.vector    <<- dataframe_sel_age_F

} else {

sel_matrix <- data.frame(matrix(-1, nrow=((length(years)*12) +1), ncol=8))
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
   
fleet.selectivity <<- sel_matrix
FleetList_simulation[[fs]]@selectivity.vector <<- fleet.selectivity  

if ( as.character(Selectivity_TYPE[1,fs] ) == "A") {
 
 dataframe_sel_age_M <- data.frame(matrix(nrow=length(years), ncol=(n_ages_M + 1)))
 colnames(dataframe_sel_age_M) <-  c( "Year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_sel_age_F <- data.frame(matrix(nrow=length(years), ncol=(n_ages_F +1)))
  colnames(dataframe_sel_age_F) <-  c( "Year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") ) 
  
  for (ye in 1:length(years)) {
       dataframe_sel_age_M[ye, ] <-  c(years[ye], loaded_selectivity_age[loaded_selectivity_age$Sex == "M" & loaded_selectivity_age$Year == years[ye], (fs+2)] )
         dataframe_sel_age_F[ye, ] <- c(years[ye],  loaded_selectivity_age[loaded_selectivity_age$Sex == "F" & loaded_selectivity_age$Year == years[ye], (fs+2)] ) 
  }

   FleetList_simulation[[fs]]@SelectivityAge.M.vector   <<- dataframe_sel_age_M
   FleetList_simulation[[fs]]@SelectivityAge.F.vector   <<- dataframe_sel_age_F

   FleetList_forecast[[fs]]@SelectivityAge.M.vector   <<- dataframe_sel_age_M
   FleetList_forecast[[fs]]@SelectivityAge.F.vector    <<- dataframe_sel_age_F
  
   }  else {
   
   selectivity_len_fs <- loaded_selectivity_length[loaded_selectivity_length$Sex == "M", (fs+2)]

    dataframe_sel_len_M <- data.frame(matrix(nrow=length(l_inf_lens_M) , ncol=(length(years)+1)))
 colnames(dataframe_sel_len_M) <-  c( "Length", years )
  dataframe_sel_len_F <- data.frame(matrix(nrow=length(l_inf_lens_F) , ncol=(length(years)+1)))
  colnames(dataframe_sel_len_F) <-  c( "Length", years )

  dataframe_sel_len_M$Length <- l_inf_lens_M
    dataframe_sel_len_F$Length <- l_inf_lens_F

# dataframe_sel_len_M <- data.frame(matrix(nrow=length(years), ncol=(length(l_inf_lens_M) + 1)))
# colnames(dataframe_sel_len_M) <-  c( "Year", paste("len",l_inf_lens_M, sep="") )
#  dataframe_sel_len_F <- data.frame(matrix(nrow=length(years), ncol=(length(l_inf_lens_F) +1)))
#  colnames(dataframe_sel_len_F) <-  c( "Year",  paste("len", l_inf_lens_F, sep="") ) 
  
  for (ye in 1:length(years)) {
       dataframe_sel_len_M[,ye+1 ] <-  loaded_selectivity_length[loaded_selectivity_length$Sex == "M" & loaded_selectivity_length$Year == years[ye], (fs+2)] 
         dataframe_sel_len_F[,ye+1 ] <- loaded_selectivity_length[loaded_selectivity_length$Sex == "F" & loaded_selectivity_length$Year == years[ye], (fs+2)]  
  }

   FleetList_simulation[[fs]]@SelectivityLength.M.vector   <<- dataframe_sel_len_M
   FleetList_simulation[[fs]]@SelectivityLength.F.vector   <<- dataframe_sel_len_F

   FleetList_forecast[[fs]]@SelectivityLength.M.vector   <<- dataframe_sel_len_M
   FleetList_forecast[[fs]]@SelectivityLength.F.vector    <<- dataframe_sel_len_F
   
   }

}

} else {

sel_matrix <- data.frame(matrix(-1, nrow=((length(years)*12) +1), ncol=8))
 
   colnames(sel_matrix) <-    heading <- c("year","month", "param1", "param2", "param3", "param4", "param5", "sel_type" )
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   sel_matrix$year <- years_rep
   sel_matrix$month <- months_rep
   
fleet.selectivity <<- sel_matrix
FleetList_simulation[[fs]]@selectivity.vector <<- fleet.selectivity  
}


if (as.character(Discard_COMPONENT[1,fs]) == "Y") { 
if (class(loaded_landing_obligation ) != "try-error") {
  loaded_landing_obligation$landing_obligation <- as.character( loaded_landing_obligation$landing_obligation)
   land_obl_fs <- loaded_landing_obligation[as.character(loaded_landing_obligation$fleet_segment) == FLEETSEGMENTS_names[fs],]
#   land_obl_fs[1,1] <- ""
   land_obl_Matr <- data.frame(matrix("", nrow=length(years), ncol=(length(MONTHS)+1)), stringsAsFactors = F)
   colnames(land_obl_Matr) <- c("year",	MONTHS)
for (yea in 1:length(years) ) {
land_obl_Matr[yea, ]  <- as.character(land_obl_Matr[yea, ] )
fico <- as.character(land_obl_fs$landing_obligation[(1+(yea-1)*12):((yea)*12) ] )
land_obl_Matr [yea,1] <-  as.character(years[yea])
land_obl_Matr [yea,2:ncol(land_obl_Matr)] <- fico 
}

FleetList_simulation[[fs]]@landing.obligation.vector <<- land_obl_Matr

   land_obl_Matr_fore <- data.frame(matrix("", nrow=length(years_forecast), ncol=(length(MONTHS)+1)), stringsAsFactors = F)
   colnames(land_obl_Matr_fore) <- c("year",	MONTHS)
for (yea in 1:length(years_forecast) ) {
land_obl_Matr_fore [yea,1] <-  as.character(years_forecast[yea])
land_obl_Matr_fore [yea,2:ncol(land_obl_Matr_fore)] <- fico 
}

FleetList_forecast[[fs]]@landing.obligation.vector <<- land_obl_Matr_fore
 
}

}  else {

land_obl_Matr_sim <- data.frame(matrix("N", nrow=length(years), ncol=(length(MONTHS)+1)))
   colnames(land_obl_Matr_sim) <- c("year",	MONTHS)
   land_obl_Matr_sim$year <- years

FleetList_simulation[[fs]]@landing.obligation.vector <<- land_obl_Matr_sim

land_obl_Matr_fore <- data.frame(matrix("N", nrow=length(years_forecast), ncol=(length(MONTHS)+1)))
   colnames(land_obl_Matr_fore) <- c("year",	MONTHS)
   land_obl_Matr_fore$year <- years_forecast

FleetList_forecast[[fs]]@landing.obligation.vector <<- land_obl_Matr_fore
}


# -------------------------------------------------------------------------------------------------------------------  load fishing coefficient calculated
if (!IN_BEMTOOL) {
if (Effort_TYPE == "E" ) { 
if (class(loaded_effortvariables) != "try-error") {  # load fishing coefficient from calculated matrix (from Effort data in input)
fishcoeff_fs <- as.numeric(as.character(fact_mat[,fs] ))
fishcoeffMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
fico <- fishcoeff_fs[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) { fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea], fishcoeff_fs[1], fico))  
} else { fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea],"", fico)) }
}
colnames(fishcoeffMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@fishingeffort.vector <<- fishcoeffMatr 
# ------------------------------------------------------------------------------------------------------------------- load fishing coefficient from file in input
}
} else if (class(loaded_fishcoeff) != "try-error") {
  fishcoeff_fs <- loaded_fishcoeff[as.character(loaded_fishcoeff$fleet_segment) == FLEETSEGMENTS_names[fs],]

fishcoeffMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
fico <- fishcoeff_fs$FISHING_COEFFICIENT[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) {
    fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea], fishcoeff_fs$FISHING_COEFFICIENT[1], fico))  
} else {
    fishcoeffMatr <- rbind(fishcoeffMatr, c(years[yea],"", fico))  
}
}
colnames(fishcoeffMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@fishingeffort.vector <<- fishcoeffMatr 
}


fishcoeffMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
    fishcoeffMatr_fore <- rbind(fishcoeffMatr_fore, c(years_forecast[yea], fico))  
}
colnames(fishcoeffMatr_fore) <- c("year",	MONTHS)
FleetList_forecast[[fs]]@fishingeffort.vector <<- fishcoeffMatr_fore 

}

# ------------------------------------------------------------------------------------------------------------------- load p production calculated
if (!IN_BEMTOOL) {
if (Production_TYPE == "P" ) {
if (class(loaded_production ) != "try-error") {
  pp_fs <- as.numeric(as.character(p_production_mat[,fs]))
ppMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
pipro <- pp_fs[(2+(yea-1)*12):(1+(yea)*12)]
if (yea==1) { ppMatr <- rbind(ppMatr, c(years[yea], pp_fs[1], pipro))  
} else { ppMatr <- rbind(ppMatr, c(years[yea],"", as.numeric(as.character(pipro))) ) }
}
colnames(ppMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@pproduction.vector <<- ppMatr 

# aggiungi il p production
}
# -------------------------------------------------------------------------------------------------------------------  load p production from file
}  else if (class(loaded_pp) != "try-error") {
pp_fs <- loaded_pp[as.character(loaded_pp$fleet_segment) == FLEETSEGMENTS_names[fs],]
ppMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
pipro <- pp_fs$P_PRODUCTION[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) { ppMatr <- rbind(ppMatr, c(years[yea], pp_fs$P_PRODUCTION[1], pipro))  
} else { ppMatr <- rbind(ppMatr, c(years[yea],"", pipro)) }
}
colnames(ppMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@pproduction.vector <<- ppMatr 

}

} else {

pp_fs <- as.numeric(as.character(p_production_mat[,fs]))
ppMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
for (yea in 1:length(years) ) {
pipro <- pp_fs[(2+(yea-1)*12):(1+(yea)*12)]
# print(vess)
if (yea==1) { ppMatr <- rbind(ppMatr, c(years[yea], pp_fs[1], pipro))  
} else { ppMatr <- rbind(ppMatr, c(years[yea],"", pipro)) }
}
colnames(ppMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[fs]]@pproduction.vector <<- ppMatr 


}


ppMatr_fore <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea in 1:length(years_forecast) ) {
    ppMatr_fore <- rbind(ppMatr_fore, c(years_forecast[yea], pipro))  
}
colnames(ppMatr_fore) <- c("year", MONTHS)
FleetList_forecast[[fs]]@pproduction.vector <<- ppMatr_fore 


# -------------------------------------------------------------------------------------------------------------------  load discard from configuration file
 if (as.character(Discard_COMPONENT[1,fs]) == "Y" ) {
# FleetList_simulation[[fs]]@discard.calculation <- "YES"
if (as.character(Discard_TYPE[1,fs]) == "RO" ) {
if (class(loaded_discard) != "try-error") {
   discard_fs <- loaded_discard[as.character(loaded_discard$fleet_segment) == FLEETSEGMENTS_names[fs],]
   discard_fs[1,1] <- ""

fleet.discard <<- discard_fs[, 1:4]
FleetList_simulation[[fs]]@discard.vector <<- fleet.discard 

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! sistemare discard_fore (quando ?reverse ogive)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

} else if (class(loaded_discard_extvector ) != "try-error") {
fleet.discard <<- NULL
 # load external discard vector from configuration file
 discard_extvector_fs <- loaded_discard_extvector[,c(1:2, (fs+2), (length(FLEETSEGMENTS_names)+3))]

 dataframe_discard_extvector_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
 colnames(dataframe_discard_extvector_M) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_discard_extvector_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
  colnames(dataframe_discard_extvector_F) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
   for (yy in 1:length(years) ) {
       discard_extvector_fs_y <- discard_extvector_fs[discard_extvector_fs$Year == years[yy], ]
       to_add <- data.frame(matrix(  discard_extvector_fs_y[discard_extvector_fs_y[ncol(discard_extvector_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_discard_extvector_M <- rbind(dataframe_discard_extvector_M, to_add)
       to_add <- data.frame(matrix(  discard_extvector_fs_y[discard_extvector_fs_y[ncol(discard_extvector_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_discard_extvector_F <- rbind(dataframe_discard_extvector_F, to_add) 
   }     # .GlobalEnv$
 FleetList_simulation[[fs]]@discard_extvector.M.vector <<- dataframe_discard_extvector_M
 FleetList_simulation[[fs]]@discard_extvector.F.vector <<- dataframe_discard_extvector_F



  dataframe_discard_extvector_M_fore <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
 colnames(dataframe_discard_extvector_M_fore) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_discard_extvector_F_fore <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
  colnames(dataframe_discard_extvector_F_fore) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
   for (yy in 1:length(years_forecast) ) {
       to_add <- data.frame(matrix(  discard_extvector_fs_y[discard_extvector_fs_y[ncol(discard_extvector_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years_forecast[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_discard_extvector_M_fore <- rbind(dataframe_discard_extvector_M_fore, to_add)
       to_add <- data.frame(matrix(  discard_extvector_fs_y[discard_extvector_fs_y[ncol(discard_extvector_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years_forecast[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_discard_extvector_F_fore <- rbind(dataframe_discard_extvector_F_fore, to_add) 
   }     # .GlobalEnv$
 FleetList_forecast[[fs]]@discard_extvector.M.vector <<- dataframe_discard_extvector_M_fore
 FleetList_forecast[[fs]]@discard_extvector.F.vector <<- dataframe_discard_extvector_F_fore

}
} 


# ----------------------------------------------------------------------------------- load fishing mortality from configuration file
if (Mortality_TYPE == "F" ) {
#if (F_TYPE == "F")  {
  if ( class(loaded_Fmortality) != "try-error") {
 F_fs <- loaded_Fmortality[,c(1:2, (fs+2), (length(FLEETSEGMENTS_names)+3))]

 dataframe_F_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
 colnames(dataframe_F_M) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_F_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
  colnames(dataframe_F_F) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
   for (yy in 1:length(years) ) {
       F_fs_y <- F_fs[F_fs$Year == years[yy], ]
       to_add <- data.frame(matrix(  F_fs_y[F_fs_y[ncol(F_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_F_M <- rbind(dataframe_F_M, to_add)
       to_add <- data.frame(matrix(  F_fs_y[F_fs_y[ncol(F_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_F_F <- rbind(dataframe_F_F, to_add) 
   }     # .GlobalEnv$
 FleetList_simulation[[fs]]@fishingmortality.M.vector <<- dataframe_F_M
 FleetList_simulation[[fs]]@fishingmortality.F.vector <<- dataframe_F_F
 }

if (F_TYPE == "O" ) { 
 if ( F_splitting_TYPE == "CAA"  & class(loaded_CatchatAGE) != "try-error") {
loaded_CatchatAGE_fs <- loaded_CatchatAGE[,c(1:2, (fs+2), (length(FLEETSEGMENTS_names)+3))]

 dataframe_F_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
 colnames(dataframe_F_M) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_F_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
  colnames(dataframe_F_F) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
   for (yy in 1:length(years) ) {
       loaded_CatchatAGE_fs_y <- loaded_CatchatAGE_fs[loaded_CatchatAGE_fs$Year == years[yy], ]
       to_add <- data.frame(matrix(  loaded_CatchatAGE_fs_y[loaded_CatchatAGE_fs_y[ncol(loaded_CatchatAGE_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_F_M <- rbind(dataframe_F_M, to_add)
       to_add <- data.frame(matrix(  loaded_CatchatAGE_fs_y[loaded_CatchatAGE_fs_y[ncol(loaded_CatchatAGE_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_F_F <- rbind(dataframe_F_F, to_add) 
   }     # .GlobalEnv$
 FleetList_simulation[[fs]]@catchAtAge.M.vector <<- dataframe_F_M
 FleetList_simulation[[fs]]@catchAtAge.F.vector <<- dataframe_F_F
 }
 }
 
 
# } 
}


} # end fleet segment


# -------------------------------------------------------------------------------------------------------------------
if (length(errorsVector) > 0) {
      errorsVector <- c(errorsVector, ". Inconsistent settings and data!")    
}


#if (!IN_BEMTOOL) {
#  reload_fleetsegment_info()
#} 
 
 if (showCompTime)  {
# SIMULATION_EXPLOITED_ptm <- proc.time()
proc_ <- proc.time()
print(paste("setBiologicalParams [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-setBiologicalParams_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - setBiologicalParams_ptm, quote=F ) 
rm(setBiologicalParams_ptm)
}

}
