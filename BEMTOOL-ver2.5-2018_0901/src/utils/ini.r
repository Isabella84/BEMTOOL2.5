# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# install.packages(repos="http://flr-project.org/R")   ggplot2, gridExtra, akima
# FLAdvice non c'è più sul rep R-CRAN
# link a cui scaricare: http://flr-project.org/doku.php



#install.packages("ggplot2")
#install.packages("akima")
## list of packages to install:
#
## FLCore_2.5.0
## FLAssess_2.5.0
## FLXSA_2.5
## FLash_2.5.0
## FLBRP_2.5.0
## ggplotFL_0.1
## FLAdvice_1.0
#
## Load the libraries
#library(FLCore)
#library(FLAssess)
#library(FLXSA)
#library(FLash)
#library(FLBRP)
#
#library(ggplot2)
#library(akima)
#library(ggplotFL)
#library(FLAdvice) 
#
#require(plyr)
#require(akima)
#require(ggplot2)

# vector of months
MONTHS <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
RUN_ALADYM_BMT <<- TRUE
# ALADYM_home <<- paste(getwd(), "/src/biol/bmtALADYM/ALADYM-ver10.1.4-2015_aperto", sep="")  # versione aperta
ALADYM_home <<- paste(getwd(), "/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501", sep="")  # versione chiusa
MTF_home <<- paste(getwd(), "/src/biol/bmtMTF/MTF", sep="")
STF_home <<- paste(getwd(), "/src/biol/bmtSTF/STF", sep="")

# bio_indicators_to_plot <<- c("Z",	"F",	"ER",	"Ls",	"Ls0",	"Lc",	"Lcri",	"Lcri0",	"unex_stock_L0_95",	"ex_stock_L0_95",	"Biol_prod",	"F_Fref",	"B",	"SSB_SSBref",	"B0",	"SSB",	"SSB0",	"Exp_pop_nb",	"Unexp_pop_nb",	"Spawners_nb_exp_pop",	"Spawners_nb_unexp_pop",	"Mean_length_exp_pop",	"Mean_length_unexp_pop",	"Harvest ratio",	"Y",	"D",	"Discard rate")

# bio_indicators_to_plot <<- c("Z",	"F",	"ER",	"Spawners_length",	"Spawners_length(F=0)",	"Catch_length",	"Lcri",	"Lcri(F=0)",	"unex_stock_L0_95",	"ex_stock_L0_95",	"Biol_prod",	"F_Fref",	"B",	"SSB_SSBref",	"B(F=0)",	"SSB",	"SSB(F=0)",	"Exp_pop_nb",	"Unexp_pop_nb",	"Spawners_nb_exp_pop",	"Spawners_nb_unexp_pop",	"Mean_length_exp_pop",	"Mean_length_unexp_pop",	"Harvest ratio",	"Y",	"D",	"Discard rate")

# CONFIGURATION file                                                         
cfg <<- try(read.csv(file=paste(getwd(), "/bmtconfig.csv", sep=""), sep=";", na.strings = "NA", header=FALSE) )

if (class(cfg) != "try-error") {
nm <- as.character(cfg[,1])
cfg_rownames <<- nm
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
cfg <<- cfg[,2:ncol(cfg)]
rownames(cfg)[empty_indices] <- nm
casestudy.endsimulation <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endsimulation",1])) 
casestudy.startsimulation <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startsimulation",1])) 
years <<- c(casestudy.startsimulation:casestudy.endsimulation )
simperiod <<- casestudy.endsimulation - casestudy.startsimulation + 1

phase <<- "SIMULATION"
BMT_SCENARIO <<- 0

casestudy_name <<- as.character(cfg[rownames(cfg) == "casestudy.name",1])
casestudy_path <<- as.character(cfg[rownames(cfg) == "casestudy.name",2])
harvest_rule_code <<- ifelse(phase=="FORECAST",  as.character(cfg[rownames(cfg) == "casestudy.HR",1]) , "Diagnosis")
harvest_rule_level <<- ifelse(phase=="FORECAST",  as.character(cfg[rownames(cfg) == "casestudy.HR",2]), "")
harvest_rule_id <<- ifelse(phase=="FORECAST", paste(harvest_rule_code, "-", harvest_rule_level, sep=""), "" )


casestudy.endforecast <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endforecast",1])) 
casestudy.startforecast <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startforecast",1])) 
years.forecast <<- c(casestudy.startforecast:casestudy.endforecast)

foreperiod <<-  casestudy.endforecast - casestudy.startforecast  +  1


#casestudy.endforecast <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endforecast",1])) 
#casestudy.startforecast <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startforecast",1]))
#years_forecast <- c(casestudy.startforecast:casestudy.endforecast )

      #6 # Y in the document "Economic Data Input"
n_fleet <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.fleetsegmentno",1]))                         # to be read from cfg table
m_stock <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.stockno",1]))

BMT_SPECIES <<- as.character(cfg[rownames(cfg) == "casestudy.S1",1])
if (m_stock > 1)  {
for (sp in 2:m_stock) {
if (as.character(cfg[rownames(cfg) == paste("casestudy.S", sp, sep=""),1]) != "" ) {
   BMT_SPECIES <<- c(BMT_SPECIES, as.character(cfg[rownames(cfg) == paste("casestudy.S", sp, sep=""),1]))
}
}
}
BMT_FLEETSEGMENTS <<- as.character(cfg[rownames(cfg) == "casestudy.F1",1])
BMT_FLEETSEGMENTS_FTEC <<- as.character(cfg[rownames(cfg) == "casestudy.F1",2])
BMT_FLEETSEGMENTS_LOA <<- as.character(cfg[rownames(cfg) == "casestudy.F1",3])
if (n_fleet > 1) {
for (fl in 2:n_fleet) {
if (as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),1]) != "" ) {
   BMT_FLEETSEGMENTS <<- c(BMT_FLEETSEGMENTS, as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),1]))
}
if (as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),1]) != "" ) {
   BMT_FLEETSEGMENTS_FTEC <<- c(BMT_FLEETSEGMENTS_FTEC, as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),2]))
}
if (as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),1]) != "" ) {
   BMT_FLEETSEGMENTS_LOA <<- c(BMT_FLEETSEGMENTS_LOA, as.character(cfg[rownames(cfg) == paste("casestudy.F", fl, sep=""),3]))
}
}
}

BMT_GSA <<- as.character(cfg[rownames(cfg) == "casestudy.GSA",1]) 

} 

if (BMT_STATE != "START" & BMT_STATE != "DIAGNOSIS") {
cfg_rownames <<- NULL
casestudy.endsimulation <<- NULL
casestudy.startsimulation <<- NULL
years <<- c()
simperiod <<- NULL

phase <<- "SIMULATION"
BMT_SCENARIO <<- 0
MEY_CALCULATION <<- FALSE

casestudy_name <<- ""
casestudy_path <<- ""
harvest_rule_code <<- ""
harvest_rule_level <<- ""
harvest_rule_id <<- ""


casestudy.endforecast <<- NULL 
casestudy.startforecast <<- NULL 
years.forecast <<- c()

foreperiod <<-  NULL


#casestudy.endforecast <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endforecast",1])) 
#casestudy.startforecast <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startforecast",1]))
#years_forecast <- c(casestudy.startforecast:casestudy.endforecast )

      #6 # Y in the document "Economic Data Input"

BMT_SPECIES <<- c()
BMT_FLEETSEGMENTS <<- c()
BMT_FLEETSEGMENTS_FTEC <<- c()
BMT_FLEETSEGMENTS_LOA <<- c()
BMT_GSA <<- NULL

}

#}



# 
BMT_HR_CHANGE_SELECTIVITY <- 1
BMT_HR_CHANGE_FISHEFFORT <- 2
BMT_HR_CHANGE_FISHMORTALITY <- 3
BMT_HR_CHANGE_TOTAL_FISHMORTALITY <- 4
BMT_HR_STATUS_QUO <- 5
BMT_HR_TAC_VARIATION <- 6
BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT <- 7
BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL <- 8
BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL <- 9
BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL <- 10
BMT_HR_STATUS_QUO_BEHAVIOURAL <- 11

#BMT_SCENARIO_ALADYMGUI <<- c( BMT_HR_CHANGE_SELECTIVITY , BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL , BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT , BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL )


w <<- NULL
k_GVA_ROI_PROFITS <<- NULL
k_RBER <<- NULL
k_WAGE <<- NULL
k_EMPL <<- NULL
k_SSB <<- NULL
k_F <<- NULL
k_Y <<- NULL
k_D <<- NULL



u_gva_mey <<- NULL
u_gva_0.5mey <<- NULL

#u_roi_mey<-as.numeric(as.character(utility_params$Value[3]))
#u_roi_0.5mey<-as.numeric(as.character(utility_params$Value[4]))

u_rber_1 <<- NULL
u_rber_1.5 <<- NULL

u_empl_ce <<- NULL
u_empl_0.5ce <<- NULL

u_wage_mnw <<- NULL

u_ssb_0.2 <<- NULL
u_ssb_msy <<- NULL

u_f_msy <<- NULL
u_f_2msy <<- NULL

u_y_msy <<- NULL
u_y_0.5msy <<- NULL

u_d_0.25 <<- NULL
u_d_0.5 <<- NULL


GVA_or_ROI_or_PROFITS <<- NULL
last_values <<- NULL
