# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같?additional code for BEMTOOL integration
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같

n_ages_males <- 1
first_age_mal <- 0
n_ages_females <- 1
first_age_fem <- 0

if (IN_BEMTOOL) {

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
print(FLEETSEGMENTS_names)
#BMT_FLEETSEGMENTS 
if (phase == "SIMULATION") {
n_ord <- 1
for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
  if (n_int %in% associated_fleetsegment_indices) {

productionMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(productionMatr) <- c("year",	"seed",	MONTHS)
pproductionMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(pproductionMatr) <- c("year",	"seed",	MONTHS)
vesselsMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(vesselsMatr) <- c("year",	"seed",	MONTHS)
daysMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(daysMatr) <- c("year",	"seed",	MONTHS)
gtMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(gtMatr) <- c("year",	"seed",	MONTHS)
fisheffMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+2)))
colnames(fisheffMatr) <- c("year",	"seed",	MONTHS)
discMatr <- data.frame(matrix(nrow=0, ncol=4))


n_ages_males <- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan[1,1]))
first_age_mal <- 0
# {
    n_ages_males <- n_ages_males - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
#} 


n_ages_females <- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan[2,1]))
first_age_fem <- 0
#} 
    n_ages_females <- n_ages_females - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)
#} 

Fmatrix_M <- data.frame(matrix(NA, nrow=length(years), ncol=n_ages_males+1)) 
colnames(Fmatrix_M) <- c("year", paste("age", c(first_age_mal:(n_ages_males+first_age_mal-1)), sep=""))

Fmatrix_F <- data.frame(matrix(NA, nrow=length(years), ncol=n_ages_females+1)) 
colnames(Fmatrix_F) <- c("year", paste("age", c(first_age_fem:(n_ages_females+first_age_fem-1)), sep=""))

Fmatrix_M[,1] <- years
Fmatrix_F[,1] <- years


Discard_extvector_M <- data.frame(matrix(NA, nrow=length(years), ncol=n_ages_males+1)) 
colnames(Discard_extvector_M) <- c("year", paste("age", c(first_age_mal:(n_ages_males+first_age_mal-1)), sep=""))

Discard_extvector_F <- data.frame(matrix(NA, nrow=length(years), ncol=n_ages_females+1)) 
colnames(Discard_extvector_F) <- c("year", paste("age", c(first_age_fem:(n_ages_females+first_age_fem-1)), sep=""))

Discard_extvector_M[,1] <- years
Discard_extvector_F[,1] <- years



SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1]) 
if (SAtool =="VIT" ) {
    VIT.analysis.discard <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),4]) 
    if (VIT.analysis.discard) {
       DISCARD_analysis <- TRUE
    } else {
       DISCARD_analysis <- FALSE
    }
} else {
       DISCARD_analysis <- FALSE
}

print(".......................................... [add_BMT_fleet.r]", quote=F)
new_aldFleetsegment <<- new(Class="aldFleetsegment",
                            fleetname = FLEETSEGMENTS_names[n_ord],
                            production.datatype  = "Production data",
                            production.vector = productionMatr,
                            pproduction.vector = pproductionMatr,
                            effort.datatype  = "Effort data",
                            vessels.vector = vesselsMatr,
                            days.vector = daysMatr,
                            gt.vector = gtMatr,
                            fishingeffort.vector = fisheffMatr,
                            discard.calculation = ifelse(DISCARD_analysis, "YES", "NA"),
                            discard.vector = discMatr,
                            fishingmortality.M.vector = Fmatrix_M,
                            fishingmortality.F.vector = Fmatrix_F,
                            discard_extvector.M.vector = Discard_extvector_M,
                            discard_extvector.F.vector = Discard_extvector_F) 

FleetList_simulation <<- c(FleetList_simulation, new_aldFleetsegment)
FleetList_forecast <<- c(FleetList_forecast, new_aldFleetsegment)  
  n_ord<- n_ord+1
  }
}

n_ord <- 1
for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
  if (n_int %in% associated_fleetsegment_indices) {
  add_BMT_fleetsegments(n_ord, n_int)
  FleetList_forecast[[n_ord]] <-  FleetList_simulation[[n_ord]]
  n_ord<- n_ord+1
  }
}



} else {           # FORECAST

          new_aldPopulation <<- ALADYM_GUI_populations[[ALADYM_spe]]
          new_aldSimulation <<- ALADYM_GUI_simulations[[ALADYM_spe]]
          FleetList_simulation <<- ALADYM_GUI_fleets[[ALADYM_spe]]
          
# path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/GUIfle_fore.Rdata", sep="")
#load(envir = .GlobalEnv, file= path_to_save)  
#
#          FleetList_forecast <<- ALADYM_GUI_fleets_fore[[ALADYM_spe]]
 
#associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
#associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
#associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
   
         #rm(Forecast_reduction)
#         Forecast_reduction <- INP$Forecast_reduction 
             
        # print(paste("Forecast reduction of", BMT_SPECIES[ALADYM_spe]))
#         print(Forecast_reduction)

if (INTEGRATED_APPROACH) {
 current_year <- 1
} else {
  current_year <- foreperiod
}

if (IN_BEMTOOL) {

INP <- new.env()
path_to_save <- paste(casestudy_path, "/Diagnosis/working files/INP_", ALADYM_spe, ".Rdata", sep="")
no_error <- try( load(file= path_to_save , INP), silent=TRUE)
 
 if (class(no_error) !=  "try-error") {

print(paste("Reading", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
     assign("Fishing_efforts", get("Fishing_efforts", envir = INP), envir=INP_simulation)     
}

}


associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
# print(FLEETSEGMENTS_names)


   n_ord <- 1                                 
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
    if (n_int %in% associated_fleetsegment_indices) {
      # FleetList_forecast <<-
      if (IN_BEMTOOL & INTEGRATED_APPROACH) {
       update_BMT_fleetsegments_fore.int(n_ord, n_int) #, FleetList_forecast
       } else {
       update_BMT_fleetsegments_fore(n_ord, n_int) #, FleetList_forecast 
       }
       # update_BMT_fishingeffort_fore(n_ord, n_int)
      n_ord <- n_ord+1
      # FleetList_simulation
    }
}



          # print(paste("Forecast reduction of", BMT_SPECIES[ALADYM_spe], "dopo la lettura dal bmtconfig..."))
         #  INP$Forecast_reduction <- Forecast_reduction  
#           print(INP$Forecast_reduction)    

#
}

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
#print(FLEETSEGMENTS_names)
}
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같?additional code for BEMTOOL integration
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같






# print(".......................................... [fishery.r]", quote=F)
vboxFishery <- gtkVBox(homogeneous = FALSE, 5)
# hboxFisheryName <- gtkHBox(FALSE, 5)
#hboxFisheryName$PackStart(gtkLabel("Fleet segment name"), expand = FALSE, fill = FALSE, padding = 10)

entryGearName <- gtkEntry()

# hboxFisheryName$PackStart(entryGearName, expand = FALSE, fill = FALSE, padding = 10)
# vboxFishery$PackStart(hboxFisheryName, expand = FALSE, fill = FALSE, padding = 10)

combo_fleetsegments <<- gtkComboBoxNewText()
gSignalConnect(combo_fleetsegments, "changed", reload_fleetsegment_info)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
if (IN_BEMTOOL) {

for (item in BMT_FLEETSEGMENTS[associated_fleetsegment_indices]) {
  combo_fleetsegments$appendText(item)
}

}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
  
 

vboxEFFORTvars_choice <- gtkVBox(FALSE, 5)

hboxEffortData <- gtkHBox(FALSE, 5)
## Create a radio button with a GtkEntry widget 

radio_effortdata <- gtkRadioButton()
radio_effortdata$add(gtkLabel("Effort data"))
radio_fishingcoeff <- gtkRadioButtonNewWithLabelFromWidget(radio_effortdata, "Fishing coefficient")

radio_data <- gtkRadioButton()
radio_data$add(gtkLabel("Production data"))
radio_pp <- gtkRadioButtonNewWithLabelFromWidget(radio_data, "P production")

radio_f_by_fleet <- gtkRadioButton()
radio_f_by_fleet$add(gtkLabel("F by fleet"))
radio_f_overall <- gtkRadioButtonNewWithLabelFromWidget(radio_f_by_fleet, "F overall")

radio_production_splitting <- gtkRadioButton()
radio_production_splitting$add(gtkLabel("split by production data"))
radio_catch_by_age_splitting <- gtkRadioButtonNewWithLabelFromWidget(radio_production_splitting, "split by catch at age")
  
button_load_production <- gtkButtonNewWithLabel("Load production data...")
button_load_production$AddCallback("clicked", loadProductionfromFile)

button_load_discard_data <- gtkButtonNewWithLabel("Load discard data...")
button_load_discard_data$AddCallback("clicked", loadmonthlyDiscardfromFile)      

button_load_effortdata <- gtkButtonNewWithLabel("Load effort data...")
button_load_effortdata$AddCallback("clicked", loadEffortDatafromFile) 

button_load_p_production <- gtkButtonNewWithLabel("Load P production...")
button_load_p_production$AddCallback("clicked", loadP_productionfromFile) 

button_load_fishingcoeff <- gtkButtonNewWithLabel("Load fishing coefficient...")
button_load_fishingcoeff$AddCallback("clicked", loadFishingCoefficientfromFile)

button_exp_production <- gtkButtonNewWithLabel("Export production data...")
button_exp_production$AddCallback("clicked", exportProductiontoFile)  

button_exp_discard_data <- gtkButtonNewWithLabel("Export discard data...")
button_exp_discard_data$AddCallback("clicked", exportmonthlyDiscardtoFile)    

button_exp_effortdata <- gtkButtonNewWithLabel("Export effort data...")
button_exp_effortdata$AddCallback("clicked", saveEffortDatatoFile) 

button_exp_p_production <- gtkButtonNewWithLabel("Export P production...")
button_exp_p_production$AddCallback("clicked", exportP_ProductiontoFile) 

button_exp_fishingcoeff <- gtkButtonNewWithLabel("Export fishing coefficient...")
button_exp_fishingcoeff$AddCallback("clicked", exportFishingCoefficienttoFile)

#vboxEFFORT$PackStart(vboxEFFORTvars_choice, expand = FALSE, fill = FALSE, padding = 5)


tbl_fleets <- gtkTable(1,7,homogeneous = FALSE)
tbl_fleets$SetRowSpacings(10)
tbl_fleets$SetColSpacings(30)
tbl_fleets$SetBorderWidth(5)

j=0  # row 1
i=0  # column 1 

tbl_fleets$Attach( gtkLabel("FLEET SEGMENTS") ,i, i+1,  j, j+1)    

i=i+1  # column 2 
tbl_fleets$Attach(combo_fleetsegments ,i, i+1,  j, j+1)


i=i+1  # column 2 
tbl_fleets$Attach(entryGearName,i, i+1, j, j+1) 

i=i+1  # column 3
button_save_fleet <- gtkButtonNewWithLabel("Add")
button_save_fleet$AddCallback("clicked", add_fleetsegment)
tbl_fleets$Attach(button_save_fleet ,i, i+1,  j, j+1)



i=i+1  # column 6
button_remove_fleet <- gtkButtonNewWithLabel("Remove")
button_remove_fleet$AddCallback("clicked", remove_fleetsegment)
tbl_fleets$Attach(button_remove_fleet ,i, i+1,  j, j+1)

i=i+1  # column 7
button_savechanges_fleet <- gtkButtonNewWithLabel("Save changes")
button_savechanges_fleet$AddCallback("clicked", update_gear)
tbl_fleets$Attach(button_savechanges_fleet ,i, i+1,  j, j+1)

j=j+1
i=0  # column 3
tbl_fleets$Attach(vboxEFFORTvars_choice ,i, i+6, j, j+1)


 hbox_temp <-   gtkHBox(FALSE, 5)
 hbox_temp$packStart(tbl_fleets, expand = TRUE, fill = TRUE, padding=5)
vboxFishery$packStart(hbox_temp, expand = FALSE, fill = FALSE, padding=0)

hboxFisheryTabs <- gtkHBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fisheryTabs.r", sep="")))

vboxFishery$PackStart(hboxFisheryTabs, expand = FALSE, fill = TRUE, padding = 5)

gSignalConnect(radio_f_by_fleet, "toggled", deactivate_FishingM_unused_params)
gSignalConnect(radio_f_overall, "toggled", deactivate_FishingM_unused_params)
gSignalConnect(radio_production_splitting, "toggled", deactivate_splittingF_unused_params)
gSignalConnect(radio_catch_by_age_splitting, "toggled", deactivate_splittingF_unused_params)

if (exists("FLEETSEGMENTSnames") ) {
gtkComboBoxSetActive(combo_fleetsegments, 0 ) 
}
