# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# Catch_nb	Yield	Landing	Landing_nb	Discard	Discard_nb	F	Mean_length_catch	Mean_length_landing	Mean_length_discard	Discard_ratio

getPressureImpactTable_CI <-function() {

all_the_years <- c(years, years.forecast)

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments", "quantile")

table_output <- data.frame(matrix(nrow=0, ncol= 11), stringsAsFactors=F)
colnames(table_output) <- head_table

casestudy_name <- as.character(cfg[rownames(cfg) == "casestudy.name",1])
casestudy_path <- as.character(cfg[rownames(cfg) == "casestudy.name",2])
#harvest_rule_code <- ifelse(all(years.forecast %in% all_the_years),  paste("HR", as.character(cfg[rownames(cfg) == "casestudy.HR",1]), sep="") , "Diagnosis")
#harvest_rule_level <- ifelse(all(years.forecast %in% all_the_years),  as.character(cfg[rownames(cfg) == "casestudy.HR",2]), "-")
#harvest_rule_id <- ifelse(all(years.forecast %in% all_the_years), paste( harvest_rule_code, "-", harvest_rule_level, sep=""), "-" )

harvest_rule_code <-   paste("HR", as.character(cfg[rownames(cfg) == "casestudy.HR",1]), sep="")
harvest_rule_level <-  as.character(cfg[rownames(cfg) == "casestudy.HR",2])
harvest_rule_id <- paste( harvest_rule_code, "-", harvest_rule_level, sep="")




# all_the_years <- c(years, years.forecast)
# all_the_years <- all_the_years[1:5] 

nr <- length(years.forecast)

for (ss in 1:length(BMT_SPECIES)) {

ALADYM_spe <<- ss
source(paste(ALADYM_home, "/src/paths.r", sep=""))

production_table_percentiles <- suppressWarnings(try(read.table(paste(PRODUCTION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")) )


ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

SR_relationship <- as.logical(cfg[rownames(cfg) == paste("casestudy.S",ss,".params", sep=""),19])
ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ss, sep=""),2])
SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ss, ".StockAssessmentTool", sep=""),1])
ALADYM_exe <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

#ALADYM_spe <<- ss
#source(paste(ALADYM_home, "/src/paths.r", sep=""))
#
#
if (phase != "FORECAST") {
 production_table_percentiles <- NULL
}

#population_table_percentiles <- read.table(paste(POPULATION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";")
   if (!is.null(production_table_percentiles) ) {
if (class(production_table_percentiles) != "try-error") {
percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)

#INP <- get(paste("INP_", ALADYM_spe, sep=""))                           #ADRIATIC - Population C.I. FORE HR5-SQ_2910_1 10 runs


# reference points
     if (SR_relationship) {
     if (ALADYM_RP) {
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",2]
            # BRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",5] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",3]
          } else {
                                   # factor, F, Y, Y/R, B, B/R, SSB, ER, SPR , SSB/R
              SAtool <- ifelse(SAtool == "NONE" | SAtool == "SURBA", "Report", SAtool)
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,2] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,3]
     }
                  note = "msy"
     } else {
     if (ALADYM_RP) {
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",2]
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",3]
          } else {
              SAtool <- ifelse(SAtool == "NONE" | SAtool == "SURBA", "Report", SAtool)
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,2] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,3]           
          }
         note = "0.1"
     }

# F ref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Fref", cbind(FRef_value, cbind("", cbind(paste("F",note, sep=""), "") ))))))))), stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Y ref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Yref", cbind(YRef_value, cbind("", cbind(paste("YF",note, sep=""), "")))))))))), stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)



#ALADYM_spe <<- ss
#source(paste(ALADYM_home, "/src/paths.r", sep=""))
#mortalities_table <- read.csv(MORTALITY_table, sep=";" )


for (yea_f in 1:length(years.forecast)) {

#print(paste("year:", years.forecast[yea_f]))
 yea <- simperiod + yea_f
 
ff_ord <- 0  
for (ff in 1:length(BMT_FLEETSEGMENTS)) {

if (ff %in% associated_fleetsegment_indices)  {
   ff_ord <- ff_ord + 1

			# catches -------------------------------------------------------------------------------------------
#catch_nb_value <- rowSums(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@numbers.CI.perc)
#to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Catch_nb",n=5), cbind(catch_nb_value/1000, cbind(rep("thousands",n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

catch_w_value <-  as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@totalweight.CI.perc ))      # [1,PERC]
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Catch", n=5), cbind(catch_w_value, cbind(rep("tons",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# landings -----------------------------------------------------------------------------
landing_w_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@totalweight.CI.perc ))    # [1,PERC]
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Landing", n=5), cbind(landing_w_value, cbind(rep("tons", n=5),  cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#landing_nb_value <- rowSums(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@numbers.CI.perc)
#to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Landing_nb", n=5), cbind(landing_nb_value/1000, cbind(rep("thousands", n=5),  cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# discard -----------------------------------------------------------------------------
discard_w_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@totalweight.CI.perc)) 
discard_w_value[which(is.na(discard_w_value))] <- 0
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Discard",n=5), cbind(discard_w_value, cbind(rep("tons",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#discard_nb_value <- rowSums(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@numbers.CI.perc, na.rm = F)
#to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Discard_nb",n=5), cbind(discard_nb_value/1000, cbind(rep("thousands",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Interactionsyear[[65]][[1]]@interactions[[1]]

# mean lengths
catch_meanlength_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@meanLength.CI.perc)) 
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Mean_length_catch",n=5), cbind(catch_meanlength_value, cbind(rep("mm",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

landing_meanlength_value <-  as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@meanLength.CI.perc)) 
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Mean_length_landing",n=5), cbind(landing_meanlength_value, cbind(rep("mm",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

discard_meanlength_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@meanLength.CI.perc)) 
discard_meanlength_value[which(is.na(discard_meanlength_value))] <- 0
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Mean_length_discard",n=5), cbind(discard_meanlength_value, cbind(rep("mm",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

F_value <-  as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@fishing_mortality.CI.perc ))      # [1,PERC]
to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("F", n=5), cbind(F_value, cbind(rep("",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Discard_ratio",n=5), cbind(discard_w_value/(landing_w_value + discard_w_value), cbind(rep("",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

if (length(associated_fleetsegment_indices) > 1) {
this_ratio <- production_table_percentiles[production_table_percentiles$Year == all_the_years[yea], colnames(production_table_percentiles) == paste("Discard_ratio_",BMT_FLEETSEGMENTS[ff],sep="") | colnames(production_table_percentiles) == "percentile"]
} else {
this_ratio <- production_table_percentiles[production_table_percentiles$Year == all_the_years[yea], colnames(production_table_percentiles) == "Discard_ratio" | colnames(production_table_percentiles) == "percentile" ]
}

if (nrow(this_ratio) == 0) {
   to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Discard_ratio",n=5), cbind(rep(NA, 5), cbind(rep("",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

}  else {
   to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("Discard_ratio",n=5), cbind(this_ratio[,1], cbind(rep("",n=5), cbind(rep("",n=5), this_ratio[,2])) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

}


#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind(production_table[yea, colnames(production_table) == paste("Discard_ratio_",BMT_FLEETSEGMENTS[ff],sep="")], cbind("", "")  ) ) ) ) ) ) ) ) )

colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)


#if ( all(is.finite(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@mortalities$F)) ) )  |  ALADYM_flag ) {
#F_annual_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@mortalities$F[ff]))
#} else {
#F_annual_value <- NA
#}

#to_add <- data.frame(cbind(rep(casestudy_name,n=5) , cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id, n=5), cbind(rep(BMT_FLEETSEGMENTS[ff],n=5), cbind(rep(BMT_SPECIES[ss], n=5), cbind(rep(all_the_years[yea], n=5), cbind(rep("F",n=5), cbind(rep(F_annual_value,n=5), cbind(rep("",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)


if (ff_ord==1) {
# fford <- 1
totalD <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totaldiscard@totalweight.CI.perc ))     # ??????????????????????????????????????
totalY <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totalcatch@totalweight.CI.perc ))
totalL <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totallanding@totalweight.CI.perc ))
totalF <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totalcatch@fishing_mortality.CI.perc ))

# Yield
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Catch",n=5), cbind(totalY, cbind(rep("tons",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Discard
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Discard",n=5), cbind(totalD, cbind(rep("tons",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Landing
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Landing",n=5), cbind(totalL, cbind(rep("tons",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# F
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("F",n=5), cbind(totalF, cbind(rep("",n=5), cbind(rep("",n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Discard rate

this_ratio <- production_table_percentiles[production_table_percentiles$Year == all_the_years[yea], colnames(production_table_percentiles) == "Discard_ratio" | colnames(production_table_percentiles) == "percentile" ]

if (nrow(this_ratio) == 0) {
to_add <-  data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Discard_ratio", n=5), cbind( rep(NA, 5), cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

}  else {
to_add <-  data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Discard_ratio", n=5), cbind( this_ratio[,1], cbind(rep("", n=5), cbind(rep("", n=5), this_ratio[,2])) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

}

#
#to_add <-  data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Discard_ratio", n=5), cbind( this_ratio, cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)

#to_add <-  data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Discard_ratio", n=5), cbind( totalD/(totalD+totalL), cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Annual_F_estimated
#to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("F", n=5), cbind(rep(Interactionsyear[[yea]][[ss]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1], n=5), cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Exploitation_rate
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Exploitation_rate", n=5), cbind(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@exploitationRate.CI.perc)), cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Lc (Mean_length_in_catch)
#to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Catch_length", n=5), cbind(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@meanLength_catches.CI.perc)), cbind(rep("mm", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Harvest ratio             #  ifelse(length(Interactionsyear[[yea]][[ss]]@exploitedStock@harvestRate.CI)==0, "not saved", 
#harvestRatio_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@harvestRate.CI.perc))
#to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("Harvest ratio", n=5), cbind(rep(harvestRatio_value, n=5), cbind(rep("", n=5), cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) ) , stringsAsFactors=F)
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# F/Fref
to_add <- data.frame(cbind(rep(casestudy_name,n=5), cbind( rep(harvest_rule_code,n=5), cbind(rep(harvest_rule_id,n=5), cbind(rep("ALL",n=5), cbind(rep(BMT_SPECIES[ss],n=5), cbind(rep(all_the_years[yea],n=5), cbind(rep("F_Fref", n=5), cbind(rep(totalF/FRef_value, n=5), cbind(rep("", n=5),cbind(rep(paste("F_F",note, sep=""), n=5), percentiles_numb) ) ) ) ) ) ) ) ) ) , stringsAsFactors=F)        # cbind("", percentiles_numb[PERC])
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)
			
}  # solo al primo fleet segment per i valori ALL

} # end if associated fleet segment
} # end loop fleet segment
} # end loop years

} # end if check CI
}

} # end loop species

 if (nrow(table_output) != 0) {
 if (length(table_output$Value[is.na(as.numeric(as.character(table_output$Value)))] ) != 0) {
table_output$Value[is.na(as.numeric(as.character(table_output$Value)))] <- 0
}
 if (length(table_output$Value[as.numeric(as.character(table_output$Value)) == -1] ) != 0) {
table_output$Value[as.numeric(as.character(table_output$Value)) == -1] <- 0
}
#write.table(table_output, paste(getwd(), "/test_Pressure_CI_table.csv", sep=""), sep=";", row.names=F)
#
#table_output <- data.frame(read.csv(paste(getwd(), "/test_Pressure_CI_table.csv", sep=""), sep=";"), stringsAsFactors=F)
#table_output$Value <- as.numeric(as.character(table_output$Value))

species_to_add_past <-  unique(as.character(table_output$Stock)) 
species_to_add_past <- species_to_add_past[species_to_add_past != "ALL"]


   name_pressind_sim <- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Pressure impact indicators.csv", sep="")
table_output_past <-  data.frame(read.csv(name_pressind_sim, sep=";") , stringsAsFactors=F)

table_output_past <- table_output_past[table_output_past$Stock %in% species_to_add_past , ]

 if (length(table_output_past$Value[is.na(as.numeric(as.character(table_output_past$Value)))] ) != 0) {
table_output_past$Value[is.na(as.numeric(as.character(table_output_past$Value)))] <- 0
}
 if (length(table_output_past$Value[as.numeric(as.character(table_output_past$Value)) == -1] ) != 0) {
table_output_past$Value[as.numeric(as.character(table_output_past$Value)) == -1] <- 0
}

for (PERC in 1:length(percentiles_numb))  {
   table_output_past_perc <- data.frame(  cbind(table_output_past, rep(percentiles_numb[PERC], nrow(table_output_past))) , stringsAsFactors=F)
colnames(table_output_past_perc) <- colnames(table_output)

#table_output_past_perc$Value <- as.numeric(as.character(table_output_past_perc$Value))
   table_output <- data.frame(rbind(table_output, table_output_past_perc) , stringsAsFactors=F)
}

table_output <- table_output[with(table_output, order( Year, Stock)), ]

#harvest_rule_code <-   paste("HR", as.character(cfg[rownames(cfg) == "casestudy.HR",1]), sep="")
#harvest_rule_level <-  as.character(cfg[rownames(cfg) == "casestudy.HR",2])
#harvest_rule_id <- paste( harvest_rule_code, "-", harvest_rule_level, sep="")

table_output$Scenario <- harvest_rule_code
table_output$ID_scenario <- harvest_rule_id

}

return(table_output)
}
