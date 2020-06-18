# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# Catch_nb	Yield	Landing	Landing_nb	Discard	Discard_nb	F	Mean_length_catch	Mean_length_landing	Mean_length_discard	Discard_ratio

getPressureImpactTable<-function(all_the_years) {

if (FALSE) {
all_the_years <- years    # present
all_the_years <- c(years, years.forecast) # future
ss=1
ff=1
yea=9
}


head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments")

table_output <- data.frame(matrix(nrow=0, ncol= 10))
colnames(table_output) <- head_table

casestudy_name <- as.character(cfg[rownames(cfg) == "casestudy.name",1])
casestudy_path <- as.character(cfg[rownames(cfg) == "casestudy.name",2])
harvest_rule_code <- ifelse(all(years.forecast %in% all_the_years),  as.character(cfg[rownames(cfg) == "casestudy.HR",1]) , "Diagnosis")
harvest_rule_level <- ifelse(all(years.forecast %in% all_the_years),  as.character(cfg[rownames(cfg) == "casestudy.HR",2]), "-")
harvest_rule_id <- ifelse(all(years.forecast %in% all_the_years), paste("HR",harvest_rule_code, "-", harvest_rule_level, sep=""), "-" )

# all_the_years <- c(years, years.forecast)
# all_the_years <- all_the_years[1:5] 

nr <- length(all_the_years)

for (ss in 1:length(BMT_SPECIES)) {

ALADYM_spe <<- ss
source(paste(ALADYM_home, "/src/paths.r", sep=""))
 # paste(casestudy_path, "/", harvest_rule_name, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], sep="") 
production_table <- suppressWarnings(try(read.table(PRODUCTION_table,header=TRUE,sep=";")) )

ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)


SR_relationship <- as.logical(cfg[rownames(cfg) == paste("casestudy.S",ss,".params", sep=""),19])
ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ss, sep=""),2])
SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ss, ".StockAssessmentTool", sep=""),1])
ALADYM_exe <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

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
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Fref", cbind(FRef_value, cbind("", paste("F",note, sep="")) )))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Y ref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Yref", cbind(YRef_value, cbind("", paste("YF",note, sep="")))))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)


# F ref
a_upper <- 0.007801555
b_upper <- 1.349401721

FUpper_value <- a_upper + FRef_value * b_upper
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Fupper", cbind(FUpper_value, cbind("", paste("F",note, sep="")) )))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#ALADYM_spe <<- ss
#source(paste(ALADYM_home, "/src/paths.r", sep=""))
#mortalities_table <- read.csv(MORTALITY_table, sep=";" )
ff_ord <- 1  
for (ff in 1:length(BMT_FLEETSEGMENTS)) {

if (ff %in% associated_fleetsegment_indices)  {

for (yea in 1:length(all_the_years)) {

ff_ord_inner <- 1  
for (ff_inner in 1:length(BMT_FLEETSEGMENTS)) {

if (ff_inner %in% associated_fleetsegment_indices)  {
if (ff_ord_inner == 1) {
   vect_catches <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord_inner]]$catches@totalweight ))
   vect_landings <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord_inner]]$landings@totalweight ))
} else {
   vect_catches <- c(vect_catches, as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord_inner]]$catches@totalweight)) )
   vect_landings <- c( vect_landings, as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord_inner]]$landings@totalweight )) )
}
ff_ord_inner <- ff_ord_inner +1
}

}

#print(paste("Year n", yea))
#print(vect_catches)
#print(vect_landings)
            
if ( all(is.finite(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@mortalities$F)) ) )  |  ALADYM_flag ) {
F_annual_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@mortalities$F[ff]))
} else {
F_annual_value <- NA
}

to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("F", cbind(F_annual_value, cbind("", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)
                                                                         #  & ALADYM_flag
#if ( (all(is.finite(vect_catches ) )  & length(vect_catches) != 0  ) | ALADYM_flag) {
#if (nrow(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@numbers) != 0) {
#catch_nb_value <- sum(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@numbers)
#} else {
#catch_nb_value <- NA
#}
#} else {
#catch_nb_value <- NA
#} 
#
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Catch_nb", cbind(catch_nb_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

if ( ( all(is.finite(vect_catches ) ) & length(vect_catches) != 0) | ALADYM_flag) {
catch_w_value <- Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@totalweight
} else {
catch_w_value <-  NA
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Catch", cbind(catch_w_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

if ( (all(is.finite(vect_landings ) ) & length(vect_landings) != 0) | ALADYM_flag) {
landing_w_value <- Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@totalweight
} else {
landing_w_value <- NA
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Landing", cbind(landing_w_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#if ( (all(is.finite(vect_landings ) ) & length(vect_landings) != 0 ) | ALADYM_flag) {
#if (nrow(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@numbers) != 0 ) {
#landing_nb_value <- sum(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@numbers)
#} else {
#landing_nb_value <- NA
#}
#} else {
#landing_nb_value <- NA
#}
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Landing_nb", cbind(landing_nb_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

if (length(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@totalweight) != 0) {
discard_w_value <- ifelse(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@totalweight != -1, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@totalweight , 0)
} else {
discard_w_value <-  NA
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard", cbind(discard_w_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#if (dim(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@numbers)[1] != 0 & 
#!(all(is.na(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@numbers)))) {
#discard_nb_value <- sum(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@numbers, na.rm = F)
#} else {
#discard_nb_value <- NA
#}
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_nb", cbind(discard_nb_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

if ( (all(is.finite(vect_catches ) )  & length(vect_catches ) != 0) | ALADYM_flag) {
catch_meanlength_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@meanLength) ==0, NA, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catches@meanLength)
} else {
catch_meanlength_value <- NA
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_catch", cbind(catch_meanlength_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

if ( (all(is.finite(vect_landings ) ) & length(vect_landings) != 0) | ALADYM_flag ) {
landing_meanlength_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@meanLength) == 0, NA, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$landings@meanLength)
} else {
landing_meanlength_value <-  NA
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_landing", cbind(landing_meanlength_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

discard_meanlength_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@meanLength) == 0, NA, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discard@meanLength)
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_discard", cbind(discard_meanlength_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)                                                                                                                            # discard_w_value/(discard_w_value+landing_w_value)
                                          
# ifelse(!is.na(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight) & Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight != -1, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight / Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catch@totalweight



#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind(ifelse(!is.na(Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight) & Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight != -1, Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$discards@totalweight, 0) / Interactionsyear[[yea]][[ss]]@interactions[[ff_ord]]$catch@totalweight, cbind("", "")  ) ) ) ) ) ) ) ) )
 if (length(associated_fleetsegment_indices) > 1) {
  if (class(production_table)=="try-error") { 
    v_to_a <- 0
  } else {
   v_to_a <- production_table[yea, colnames(production_table) == paste("Discard_ratio_",BMT_FLEETSEGMENTS[ff],sep="")]
  }
   
    to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind(v_to_a, cbind("", "")  ) ) ) ) ) ) ) ) )
 } else {
 if (class(production_table)=="try-error") { 
     v_to_a <- 0
 } else {
     v_to_a <- production_table$Discard_ratio[yea]
 }

      to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind(BMT_FLEETSEGMENTS[ff], cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind(v_to_a, cbind("", "")  ) ) ) ) ) ) ) ) )
 }


colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)


if (ff_ord==1) {
# fford <- 1
totalD <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totaldiscard@totalweight ))
totalY <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totalcatch@totalweight))
totalL <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@totallanding@totalweight ))   

# commentato il 13 giugno
#for (ff2 in 1:length(BMT_FLEETSEGMENTS)) {
#      if (ff2 %in% associated_fleetsegment_indices) {
#       if ( length(Interactionsyear[[yea]][[ss]]@interactions[[fford]]$catches@totalweight) != 0) {
#           totalY <- totalY + ifelse(!is.na(Interactionsyear[[yea]][[ss]]@interactions[[fford]]$catches@totalweight), Interactionsyear[[yea]][[ss]]@interactions[[fford]]$catches@totalweight, 0)
#           }
#           if (length(Interactionsyear[[yea]][[ss]]@interactions[[fford]]$discard@totalweight) != 0) {
#           if (Interactionsyear[[yea]][[ss]]@interactions[[fford]]$discard@totalweight != -1) {
#                totalD <- totalD + Interactionsyear[[yea]][[ss]]@interactions[[fford]]$discard@totalweight
#           }
#           }
#           #print(paste("YIELD: ", Interactionsyear[[yea]][[ss]]@interactions[[fford]]$catches@totalweight))
#          # print(paste("DISCARD: ", Interactionsyear[[yea]][[ss]]@interactions[[fford]]$discard@totalweight))
#           fford <- fford+1
#      }
#}

#if (!all(is.finite(vect_landings ) ) ) {
#totalY <- NA
#}

# Yield
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Catch", cbind(totalY, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Discard
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard", cbind(totalD, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Landing
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Landing", cbind(totalL, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Discard rate                                                               #  totalD/(totalD+totalL)



# calcolato come D/(Y-D), perché è D/Landing
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind( ifelse(!is.na(Interactionsyear[[yea]][[ss]]@totaldiscard@totalweight) & Interactionsyear[[yea]][[ss]]@totaldiscard@totalweight != -1, Interactionsyear[[yea]][[ss]]@totaldiscard@totalweight, 0) / Interactionsyear[[yea]][[ss]]@totalcatch@totalweight, cbind("", "") ) ) ) ) ) ) ) ) )

if (class(production_table)=="try-error") {
  disc_value_to_add <- 0
} else {
  disc_value_to_add <- production_table$Discard_ratio[yea]
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Discard_ratio", cbind( disc_value_to_add, cbind("", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Annual_F_estimated
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("F", cbind(Interactionsyear[[yea]][[ss]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1], cbind("", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Exploitation_rate
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Exploitation_rate", cbind(Interactionsyear[[yea]][[ss]]@exploitedStock@exploitationRate, cbind("", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Lc (Mean_length_in_catch)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Catch_length", cbind(Interactionsyear[[yea]][[ss]]@meanLength_catches, cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Harvest ratio
#harvestRatio_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@exploitedStock@harvestRate)==0, "not saved", Interactionsyear[[yea]][[ss]]@exploitedStock@harvestRate)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Harvest ratio", cbind(harvestRatio_value, cbind("", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# F/Fref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("F_Fref", cbind(Interactionsyear[[yea]][[ss]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1]/FRef_value, cbind("",paste("F_F",note, sep="") ) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

}

} # end loop years

ff_ord <- ff_ord+1
} # end if associated fleet segment
} # end loop fleet segment

# F combined


} # end loop species


table_output$Value[!is.finite(as.numeric(as.character(table_output$Value)))] <- NA
table_output$Value[as.numeric(as.character(table_output$Value)) == -1] <- NA

return(table_output)
}
