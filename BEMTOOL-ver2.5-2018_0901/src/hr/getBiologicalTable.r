# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





getBiologicalTable<-function(all_the_years) {


if (FALSE) {
  all_the_years <- c(years, years.forecast)
  ss=1
  yea=1
}

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Stock",	"Year",	"Variable",	"Value",	"Unit", "Comments")

table_output <- data.frame(matrix(nrow=0, ncol= 10))
colnames(table_output) <- head_table

nr <- length(all_the_years)
for (ss in 1:length(BMT_SPECIES) ) {

# for the test
# ss <- 1
associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

SR_relationship <- as.logical(cfg[rownames(cfg) == paste("casestudy.S",ss,".params", sep=""),19])
ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ss, sep=""),2])
SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ss, ".StockAssessmentTool", sep=""),1])
ALADYM_exe <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

SCENARIO_is_ALADYM <<- TRUE
if (BMT_SCENARIO == 4 | !ALADYM_flag) {
   SCENARIO_is_ALADYM <<- FALSE
}
    
#if (ALADYM_flag & SCENARIO_is_ALADYM) {
#ALADYM_spe <<- ss
#source(paste(ALADYM_home, "/src/paths.r", sep=""))
#production_table <- read.csv(PRODUCTION_table, sep=";" )  
#
#  # Biol_prod (Biological_production)
#to_add <- data.frame(cbind(rep(casestudy_name, nr), cbind( rep(harvest_rule_code, nr), cbind(rep(harvest_rule_id,nr), cbind(rep("ALL", nr), cbind(rep(BMT_SPECIES[ss],nr), cbind(all_the_years, cbind(rep("Biol_prod",nr), cbind(production_table$Biological_Production[1:nr], cbind(rep("tons",nr), rep("",nr) ) ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)
#} else {
#to_add <- data.frame(cbind(rep(casestudy_name, nr), cbind( rep(harvest_rule_code, nr), cbind(rep(harvest_rule_id,nr), cbind(rep("ALL", nr), cbind(rep(BMT_SPECIES[ss],nr), cbind(all_the_years, cbind(rep("Biol_prod",nr), cbind(rep(-1,nr), cbind(rep("",nr), rep("",nr) ) ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)
#}
                       # se c'? la relazione di stock recruitment metto Fmsy altrimenti F0.1
# reference points
     if (SR_relationship) {
     if (ALADYM_RP) {
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",2]
             BRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",5] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",3]
             SSBRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == "ALADYM",7]

          } else {
                                   # factor, F, Y, Y/R, B, B/R, SSB, ER, SPR , SSB/R 
                 SAtool <- ifelse(SAtool == "NONE" | SAtool == "SURBA", "Report", SAtool)
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,2] 
             BRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,5] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,3]
             SSBRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@FMSY) == SAtool,7] 
     }
                  note = "msy"
     } else {
     if (ALADYM_RP) {
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",2]
             BRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",5] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",3]
             SSBRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == "ALADYM",7]

          } else {
              SAtool <- ifelse(SAtool == "NONE" | SAtool == "SURBA", "Report", SAtool)
             FRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,2] 
             BRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,5] 
             YRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,3]
             SSBRef_value <- Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1[rownames(Interactionsyear[[simperiod]][[ss]]@referencePoints@F0.1) == SAtool,7]

          }
         note = "0.1"
     }

# B ref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Bref", cbind(BRef_value, cbind("",paste("BF",note, sep="") ))))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# SSB ref
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("SSBref", cbind(SSBRef_value, cbind("", paste("SSBF",note, sep="")) )))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

for (yea in 1:length(all_the_years)) {
     
#SSB_value <- mean(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@SSB)), na.rm=T)
 SSB_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@annual.SSB))
 
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("SSB_exploited_pop", cbind(SSB_value, cbind("tons", "" ) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# B
# monthly_B_value <- colSums(Interactionsyear[[yea]][[ss]]@exploitedStock@SB$F, na.rm=T) +  colSums(Interactionsyear[[yea]][[ss]]@exploitedStock@SB$M, na.rm=T)
B_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@annual.SB)) # mean(monthly_B_value, na.rm=T)
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Total_biomass_exploited_pop", cbind(B_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# B/B_ref (SSB/SSB_ref)
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("SSB_SSBref", cbind(SSB_value/SSBRef_value, cbind("",paste("SSB_SSBF",note, sep="")) ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)


#B0_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@annual.SB  )) # mean(monthly_B0_value, na.rm=T)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("B(F=0)", cbind(B0_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

SSB0_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@annual.SSB)) # mean(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@SSB)), na.rm=T)
if (length(SSB0_value)==0) {
    SSB0_value <- -1
}
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("SSB_unexploited_pop", cbind(SSB0_value, cbind("tons", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# riprendere da qui con la sostituzione delle medie al posto delle somme

#ExplPopuNB_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@annual.numbers )) # mean(monthly_ExplPopuNB_value, na.rm=T)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Exp_pop_nb", cbind(ExplPopuNB_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

#unExplPopuNB_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@annual.numbers))# mean(monthly_unExplPopuNB_value, na.rm=T)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Unexp_pop_nb", cbind(unExplPopuNB_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

#SS_ExplPopuNB_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@annual.SS.numbers)) # mean(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@exploitedStock@SS.numbers)), na.rm=T)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Spawners_nb_exp_pop", cbind(SS_ExplPopuNB_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

#SS_unExplPopuNB_value <- as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@annual.SS.numbers)) # mean(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@unexploitedStock@SS.numbers)), na.rm=T)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Spawners_nb_unexp_pop", cbind(SS_unExplPopuNB_value/1000, cbind("thousands", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Mean_length_exp_pop
ExplPopu_meanlength_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@exploitedStock@meanLength)==0, NA, Interactionsyear[[yea]][[ss]]@exploitedStock@meanLength)
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_of_exploited_pop", cbind(ExplPopu_meanlength_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Mean_length_unexp_pop
#unExplPopu_meanlength_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@unexploitedStock@meanLength) == 0, NA, Interactionsyear[[yea]][[ss]]@unexploitedStock@meanLength)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_unexp_pop", cbind(unExplPopu_meanlength_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# Annual_Z_estimated
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Z", cbind(Interactionsyear[[yea]][[ss]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1], cbind("year^-1", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

if (ALADYM_flag & SCENARIO_is_ALADYM) {
ALADYM_spe <<- ss
source(paste(ALADYM_home, "/src/paths.r", sep=""))
#mortalities_table <- read.csv(MORTALITY_table, sep=";" )
#indicators_table <- read.csv(INDICATORS_table, sep=";" )
#RPs_table <- read.csv(REFERENCEPOINTS_table, sep=";" )
population_table <- read.csv(POPULATION_table, sep=";" )
#production_table <- read.csv(PRODUCTION_table, sep=";" ) 
 
# Ls (Mean_length_SS_of_exploited_pop)
to_add <-  data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Mean_length_SS_of_exploited_pop", cbind(as.numeric(as.character(population_table$Mean_length_SS_of_exploited_pop[yea] )), cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

to_add <-  data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("SPR", cbind(as.numeric(as.character(population_table$ESSBratioUSSB[yea] )), cbind("", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

# Ls0 (Mean_length_SS_of_unexploited_pop)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Spawners_length(F=0)", cbind(as.numeric(as.character(population_table$Mean_length_SS_of_unexploited_pop[yea] )), cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

}

# Lcri (Critical_length_expl_pop)
to_add <-  data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Critical_length_expl_pop", cbind(Interactionsyear[[yea]][[ss]]@exploitedStock@criticalLength, cbind("mm", "") ) ) ) ) ) ) ) ) )
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

#unepl_critical_length_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@unexploitedStock@criticalLength)==0, NA, Interactionsyear[[yea]][[ss]]@unexploitedStock@criticalLength)
## Lcri0 (Critical_length_unexpl_pop)
#to_add <-  data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("Lcri(F=0)", cbind(unepl_critical_length_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# unex_stock_L0_95
#unepl_L95_value <- ifelse(length(Interactionsyear[[yea]][[ss]]@unexploitedStock@L95)==0, NA, Interactionsyear[[yea]][[ss]]@unexploitedStock@L95)
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("unex_stock_L0_95", cbind(unepl_L95_value, cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

# ex_stock_L0_95
#to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind(all_the_years[yea], cbind("ex_stock_L0_95", cbind(Interactionsyear[[yea]][[ss]]@exploitedStock@L95, cbind("mm", "") ) ) ) ) ) ) ) ) )
#colnames(to_add) <-  head_table
#table_output <- rbind(table_output, to_add)

} # end loop years

Blim_value <-  min(as.numeric(as.character(table_output$Value[table_output$Variable == "SSB_exploited_pop" & table_output$Year %in% years]  )))
Bpa_value <- Blim_value * 2
Btrigger_value <- (Bpa_value-Blim_value)/2 + Blim_value
to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Bpa", cbind(Bpa_value, cbind("tons","" ))))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Blim", cbind(Blim_value, cbind("tons","" ))))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

to_add <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(harvest_rule_id, cbind("ALL", cbind(BMT_SPECIES[ss], cbind("ALL", cbind("Btrigger", cbind(Btrigger_value, cbind("tons","" ))))))))))
colnames(to_add) <-  head_table
table_output <- rbind(table_output, to_add)

} # end loop species

table_output$Value[!is.finite(as.numeric(as.character(table_output$Value)))] <- NA
table_output$Value[as.numeric(as.character(table_output$Value)) == -1] <- NA

return(table_output)
}
