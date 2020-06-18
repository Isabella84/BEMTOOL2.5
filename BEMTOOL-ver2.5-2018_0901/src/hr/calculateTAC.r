# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





calculateTAC <- function() {
                                                                   
option_TAC <<- as.numeric( as.character(cfg[rownames(cfg) == "casestudy.HR6",1] ) )
species_TAC <<-  as.numeric( substring(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] ), 2, nchar(as.character(cfg[rownames(cfg) == "casestudy.HR6",2] )) ) )

reduction_by_stock_tbl <- read.csv( paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Reduction by stock.csv", sep=""), sep=";")
colnames(reduction_by_stock_tbl) <-  c("Stock", "percent_needed_reduction_last_year", "Fcurrent", "Reference_point", "SSBcurrent",	"SSBref",	"Comments") 

M <- Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]][[species_TAC]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] - Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]][[species_TAC]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1]
costant <- min(0.5, 1-M) 
F_MSY <- as.numeric(as.character(reduction_by_stock_tbl$Reference_point[species_TAC]))
B_MSY <- as.numeric(as.character(reduction_by_stock_tbl$SSBref[species_TAC]))
B <- mean(as.numeric(as.character(Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]][[ss]]@exploitedStock@SSB))) 
F_current <- Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]][[species_TAC]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1]
MSST <- B_MSY/2

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", species_TAC, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

  total_landing_in_weight <- 0
  fl_seg_ord <- 1
  for (fl_seg in 1:length(BMT_FLEETSEGMENTS)) {   
      if (fl_seg %in% associated_fleetsegment_indices) {
          total_landing_in_weight <- total_landing_in_weight + as.numeric(as.character(Interactionsyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]][[species_TAC]]@interactions[[1]]$landings@totalweight))
          fl_seg_ord <- fl_seg_ord + 1
      }
  }


if (option_TAC == 1) { 
# OPTION 1
# 1)	using information contained in Reduction by stock table (Diagnosis folder) + natural mortality of the stock selected by the user (the user can select only one stock at a time), the Fref for the next year can be calculated:

# F_TAC = ( F_MSY * B) / (c * B_MSY )  , se B <= c * B_MSY
#           F_MSY                      , se B > c * B_MSY

# where FMSY and BMSY are respectively the F and the spawning stock biomass corresponding to the maximum sustainable yield, B the current level of spawning stock and c is a constant equal to the minimum between 1-M and 0.5, where M is the natural mortality (see deliverable D2). The percentage of reduction needed to reach FTAC is calculated as 1- FTAC/F current;



if ( B <= costant * B_MSY) {
   F_TAC <- ( F_MSY * B) / (costant * B_MSY )
} else {
   F_TAC <-  F_MSY 
}

TAC <- ( F_TAC / F_current ) *  total_landing_in_weight
    
} else if (option_TAC == 2) {

# OPTION 2
# 1)	using information contained in Reduction by stock table (Diagnosis folder) + overall catch of the stock selected by the user (the user can select only one stock at a time), the Fref for the next year can be calculated, multiplying the current F by a factor that changes according to the state of the SSB respect to the SSBMSY:
# f = 1.00 * Fcurrent, if SSB  > SSBMSY
# f = 0.67 * Fcurrent  if MSST < SSB  < SSBMSY				
# f = 0.33 * Fcurrent  if SSB < MSST

# where the MSST is the minimum stock size threshold below which the stock is considered overfished. This value is taken equal to one half of SSBMSY. The factors 1.00, 0.67 and 0.33 are based on the default precautionary target multipliers suggested to interpret the MSY control rules in data poor situation (see deliverable D2).The percentage of reduction need to reach FTAC exactly 1-f *100;

B_MSY <- as.numeric(as.character(reduction_by_stock_tbl$SSBref[species_TAC]))
B <- as.numeric(as.character(reduction_by_stock_tbl$SSBcurrent[species_TAC]))

if (B > B_MSY) {
   TAC <- 1.00 * total_landing_in_weight
} else if (MSST < B & B_MSY > B) {
   TAC <- 0.67 * total_landing_in_weight
} else {
   TAC <- 0.33 * total_landing_in_weight
}

}  else {

if (TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR == (simperiod+1) )  {

  abundance_file <<-  as.character(cfg[rownames(cfg) == "casestudy.HR6",3] ) 
  TAC_file <<- as.character(cfg[rownames(cfg) == "casestudy.HR6",4] ) 

  abundance_path_temp <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - Abundance indices FORE", harvest_rule_id,".csv", sep="")
  TAC_path_temp <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - TAC values FORE", harvest_rule_id,".csv", sep="")
  
  abundance_mat <- read.csv(abundance_file, sep=";")
  TAC_mat <-  read.csv(TAC_file, sep=";")
  
  write.table(abundance_mat, file=abundance_path_temp, sep=";", row.names=F)
  write.table(TAC_mat, file=TAC_path_temp, sep=";", row.names=F)

  }

  abundance_path_temp <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - Abundance indices FORE", harvest_rule_id,".csv", sep="")
  TAC_path_temp <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - TAC values FORE", harvest_rule_id,".csv", sep="")

  abundance_mat <- read.csv(abundance_path_temp, sep=";")
  TAC_mat <-  read.csv(TAC_path_temp, sep=";")

mean_TAC <- mean(TAC_mat$TAC, na.rm=T)

abundance_mat$ratio <- NA
abundance_mat$ratio[-1] <- abundance_mat$Index_SSB[-1]/abundance_mat$Index_SSB[-nrow(abundance_mat)]

mean_ratio <- mean(abundance_mat$ratio, na.rm=TRUE)

TAC <- mean_ratio * mean_TAC

to_add <- data.frame(cbind(years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod], TAC))
colnames(to_add) <- colnames(TAC_mat)

TAC_mat <- rbind(TAC_mat , to_add)

write.table(TAC_mat, file=TAC_path_temp, sep=";", row.names=FALSE)

}

return(TAC)
}
