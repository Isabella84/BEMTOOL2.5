# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





if (phase == "SIMULATION") { 
vect_years <- years
index_year <- y
} else {
vect_years <- years.forecast
index_year <- y-simperiod
}


associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", m, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

# Example of object of type FleetStockInteraction with name FLEETSTOCKINTERACTION_1
# ------------------------------------------------------------------------------

# print(paste("fleet segments associated to:", BMT_SPECIES[m]), quote=FALSE)
# print(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], quote=FALSE)

nord_ass <- 0
for (n_ass in associated_fleetsegment_indices )  {
 nord_ass <- nord_ass+1
# structure of historical Landings data frame by month
historicalLandings_df <- data.frame(matrix(-1, nrow=1, ncol=12))
colnames(historicalLandings_df) <- MONTHS
rownames(historicalLandings_df) <- vect_years[index_year]

if (phase == "SIMULATION") {

landings_ts <<- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.productionData",m]), sep=";", na.strings = "NA", header=FALSE)     # landings time series by month

nm <- as.character(landings_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
landings_ts <<- landings_ts[,2:ncol(landings_ts)]
rownames(landings_ts)[empty_indices] <- nm

# for (n_int in 1:n_fleet) {     
     #  for (y_int in 1:simperiod) {         
           index_col <-  (n_ass-1)*simperiod  + index_year
           #print(index_col) 
           for (month in 1:12) {                                 
              historicalLandings_df[1,month] = as.numeric(as.character(landings_ts[rownames(landings_ts) == paste("casestudy.month", month,sep=""),index_col]))
           } # end loop month
     #  }   # end loop years
# }   # end loop fleetsegments

}

# structure of p-production data frame by month
pProduction_df <- data.frame(matrix(0, nrow=1, ncol=12))
colnames(pProduction_df) <- MONTHS
rownames(pProduction_df) <- vect_years[index_year]

selectivity_df <- data.frame(matrix(NA, nrow=12, ncol=6))
rownames(selectivity_df) <- MONTHS
colnames(selectivity_df) <- c("param1", "param2","param3","param4","param5", "param6")
# structure of selectivity data frame
selectivity_list <- list(code=NA, parameters=selectivity_df)

# structure of reference points
rp_temp <-  data.frame(matrix(NA, ncol=10, nrow=4))
rownames(rp_temp) <- c("VIT", "XSA", "Report", "ALADYM") 
colnames(rp_temp) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB", "ER", "SPR", "SSB/R")

mortalities_temp <-	data.frame(matrix(NA, ncol=2, nrow=(length(BMT_FLEETSEGMENTS)+1)))
rownames(mortalities_temp) <- c(BMT_FLEETSEGMENTS, "ALL") 
colnames(mortalities_temp) <- c("Z", "F")

one_interaction <- list(fleetsegment = CatchesAssociations[[nord_ass]][[1]], 
                                            landings = CatchesAssociations[[nord_ass]][[2]], 
                                            discards = CatchesAssociations[[nord_ass]][[3]], 
                                            catches = CatchesAssociations[[nord_ass]][[4]],
                                            historicalLandings = historicalLandings_df,
                                            pProduction = pProduction_df,
                                            selectivity = selectivity_list,
                                            landing_obligation = "N" 
                                            ) 

if (nord_ass==1) {
new_interactions <- list(one_interaction)
} else {
new_interactions <- c(new_interactions, list(one_interaction))
}

}

perc_empty <-  data.frame(matrix(0, nrow=1, ncol=5))

new_fleetstock_interaction <- new(Class= "bmtInteraction",
                population = Populations[[m]]@species, #POPULATION_1,
                unexploitedStock = Stocks[[m]][[2]], #UNEXPLOITEDSTOCK_1,
                exploitedStock = Stocks[[m]][[1]], #EXPLOITEDSTOCK_1,
                mortalities = mortalities_temp,
                interactions = new_interactions,
                referencePoints = new(Class= "bmtBioreferencepoint", 
                                      F0.1 = rp_temp,
                                      F0.2 = rp_temp,
                                      Fmax = rp_temp,
                                      FMSY = rp_temp ) ,
                #L95_catches = 0,
                meanWeight_catches = 0,
                meanLength_catches = 0,
                meanWeight_catches.CI.perc = perc_empty,
                meanLength_catches.CI.perc = perc_empty
                )
                
# print(paste("Interaction successfully created for year ",  vect_years[index_year], "!", sep=""), quote=FALSE)
