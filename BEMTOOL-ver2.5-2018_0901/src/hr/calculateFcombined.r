calculateFcombined <- function(all_the_years,  path_pressure, path_eco) {

if (FALSE){
   all_the_years <- c(years) #, years.forecast)  # years #
#   path_biological <- name_bioind
   path_pressure <- name_pressind 
    path_eco <- name_econind
}

          #"TEST Fcombined_Economic output.csv"
          #"TEST Fcombined_Pressure impact indicators"
#BIOIndicators <- data.frame(read.csv(path_biological, sep=";",header=T), stringsAsFactors=F)
PRESSIndicators <- data.frame(read.csv("TEST Fcombined_Pressure impact indicators.csv", sep=";",header=T), stringsAsFactors=F)                #path_pressure
ECOIndicators <- data.frame(read.csv("TEST Fcombined_Economic output.csv", sep=";",header=T), stringsAsFactors=F)     #path_eco

# calculate the Fref_combined

Fref_Overall <- PRESSIndicators[PRESSIndicators$Variable == "Fref",c(4:8)]

Fcurrent_ByFleet <- PRESSIndicators[PRESSIndicators$Fleet_segment != "ALL" & PRESSIndicators$Variable == "F" ,c(4:8)]

Fcurrent_Overall <- PRESSIndicators[PRESSIndicators$Fleet_segment == "ALL" & PRESSIndicators$Variable == "F" ,c(4:8)]

merged <- merge( merge(Fcurrent_ByFleet, Fcurrent_Overall, by =c("Stock", "Year")), Fref_Overall, by =c("Stock")) 
merged <- merged[,c(1:3, 5, 8, 12) ]
colnames(merged) <- c("Stock", "Fleet_segment", "Year", "Fcurrent_byfleet", "Fcurrent_overall", "Fref") 

Revenues <- ECOIndicators[ECOIndicators$Variable == "revenues.landing" & ECOIndicators$Year == all_the_years[length(all_the_years)], c(4:6, 8)]

colnames(Revenues)[colnames(Revenues) == "Species"] <- "Stock"
colnames(Revenues)[colnames(Revenues) == "Value"] <- "Revenues" 

# riportare le revenues per ogni anno in modo da avere Fcombined ogni anno... ora lo fa solo nel 2014

merged <- merge(merged, Revenues, by =c("Stock", "Fleet_segment", "Year"))

RevenuesOverall <- aggregate(merged$Revenues, by=list(merged$Stock), FUN="sum")
colnames(RevenuesOverall) <- c("Stock", "Revenues_overall")

merged <- merge(merged, RevenuesOverall, by =c("Stock"))

merged$Fref_X_RevOverall <-   merged$Fref * merged$Revenues_overall

Fref_combMEAN_prods <- aggregate(merged$Fref_X_RevOverall, by=list(merged$Stock), FUN="mean")
colnames(Fref_combMEAN_prods) <- c("Stock", "Fref_X_RevOverall")
Fref_combSUM_rev <- aggregate(merged$Revenues, by=list(merged$Stock), FUN="sum")
colnames(Fref_combSUM_rev) <- c("Stock", "RevSpecies")

Fref_combined <- sum(Fref_combMEAN_prods[,2]) /  sum(Fref_combSUM_rev[,2])

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments")
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind("ALL", cbind("ALL", cbind("Fref_combined", cbind(Fref_combined, cbind("","" ) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
PRESSIndicators <- rbind(PRESSIndicators, to_add) 


# calcolo Fcurrent_combined

FcurrentbyY_ByFleet <- PRESSIndicators[PRESSIndicators$Fleet_segment != "ALL" & PRESSIndicators$Variable == "F",c(4:8)]

Fref_combSUM_rev_byYear <- aggregate(merged$Revenues, by=list(merged$Stock, merged$Year), FUN="sum")
colnames(Fref_combSUM_rev_byYear) <- c("Stock", "Year", "RevSpecies")

Fref_combSUM_rev_byYear_byFleet <- aggregate(merged$Revenues, by=list(merged$Fleet_segment, merged$Year), FUN="sum")
colnames(Fref_combSUM_rev_byYear_byFleet) <- c("Fleet_segment", "Year", "RevFleet_segment")

merged <- merge( merge(FcurrentbyY_ByFleet, Fref_combSUM_rev_byYear, by =c("Stock", "Year")) , Fref_combSUM_rev_byYear_byFleet, by=c("Fleet_segment", "Year") )
merged <- merged[,c(1:3, 5:7) ]
colnames(merged) <- c("Fleet_segment", "Year", "Stock", "Fcurrent", "RevenuesSpecies", "RevenuesFleet") 

#Fref_combSUM_rev_byYear <- aggregate(merged$Revenues, by=list(merged$Stock, merged$Year), FUN="sum")
#colnames(Fref_combSUM_rev_byYear) <- c("Stock", "Year", "RevSpecies")
#
merged$prodFcurr_X_Revenues <- as.numeric(as.character(merged$Fcurrent)) * as.numeric(as.character(merged$RevenuesSpecies))

#revenues by fleet

write.table(merged, "test_Fcomb.csv", sep=";")

SUM_prodFcurr_X_Revenues <- aggregate(merged$prodFcurr_X_Revenues, by=list(merged$Fleet_segment, merged$Year), FUN="sum")
colnames(SUM_prodFcurr_X_Revenues) <- c("Fleet_segment", "Year", "SUM_prod_byFleet")

SUM_Revenues <- aggregate(merged$RevenuesSpecies, by=list(merged$Fleet_segment, merged$Year), FUN="sum")
colnames(SUM_Revenues) <- c("Fleet_segment", "Year", "SUM_revenues_byFleet")

SUM_prodFcurr_X_Revenues <- merge(SUM_prodFcurr_X_Revenues, SUM_Revenues, by=c("Fleet_segment", "Year") )

write.table(SUM_prodFcurr_X_Revenues, "test_Fcomb2.csv", sep=";")

SUM_prodFcurr_X_Revenues$Fcombined <- as.numeric(as.character(SUM_prodFcurr_X_Revenues$SUM_prod_byFleet)) / as.numeric(as.character(SUM_prodFcurr_X_Revenues$SUM_revenues_byFleet))     # errore


Fcombined_overall <- aggregate(SUM_prodFcurr_X_Revenues$Fcombined, by=list( SUM_prodFcurr_X_Revenues$Year), FUN="sum")
colnames(Fcombined_overall) <- c("Year", "Fcombined_overall" )

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments")
to_add <- data.frame(cbind(rep(casestudy_name, nrow(SUM_prodFcurr_X_Revenues) ), cbind( rep(harvest_rule_code, nrow(SUM_prodFcurr_X_Revenues) ), cbind(rep(harvest_rule_id, nrow(SUM_prodFcurr_X_Revenues) ), cbind(as.character(SUM_prodFcurr_X_Revenues$Fleet_segment), cbind(rep("ALL", nrow(SUM_prodFcurr_X_Revenues) ), cbind(as.numeric(as.character(SUM_prodFcurr_X_Revenues$Year)), cbind(rep("Fcombined",nrow(SUM_prodFcurr_X_Revenues) ), cbind(as.numeric(as.character(SUM_prodFcurr_X_Revenues$Fcombined)), cbind(rep("", nrow(SUM_prodFcurr_X_Revenues)), rep("", nrow(SUM_prodFcurr_X_Revenues)) ) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
PRESSIndicators <- rbind(PRESSIndicators, to_add) 

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments")
to_add <- data.frame(cbind(rep(casestudy_name, nrow(Fcombined_overall) ), cbind( rep(harvest_rule_code, nrow(Fcombined_overall) ), cbind(rep(harvest_rule_id, nrow(Fcombined_overall) ), cbind(rep("ALL", nrow(Fcombined_overall) ), cbind(rep("ALL", nrow(Fcombined_overall) ), cbind(as.numeric(as.character(Fcombined_overall$Year)), cbind(rep("Fcombined",nrow(Fcombined_overall) ), cbind(as.numeric(as.character(Fcombined_overall$Fcombined_overall)), cbind(rep("",nrow(Fcombined_overall) ), rep("",nrow(Fcombined_overall) ) ) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
PRESSIndicators <- rbind(PRESSIndicators, to_add) 

write.table(PRESSIndicators, "combined results_test.csv", sep=";", row.names=F)

return(table_output)
}