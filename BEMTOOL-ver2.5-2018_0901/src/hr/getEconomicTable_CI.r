# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




getEconomicTable_CI <- function() {

numb_digits=3

all_the_years <- years.forecast

time_star_eco <- proc.time()

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Species",	"Year",	"Variable",	"Value",	"Unit", "quantile")

table_output <- data.frame(matrix(nrow=0, ncol = 10))
colnames(table_output) <- head_table

casestudy_name <- as.character(cfg[rownames(cfg) == "casestudy.name",1])
casestudy_path <- as.character(cfg[rownames(cfg) == "casestudy.name",2])
harvest_rule_code <-   paste("HR", as.character(cfg[rownames(cfg) == "casestudy.HR",1]), sep="")
harvest_rule_level <-   as.character(cfg[rownames(cfg) == "casestudy.HR",2])
harvest_rule_id <-  paste(harvest_rule_code, "-", harvest_rule_level, sep="")

# added by PA
minwage <- data.frame(matrix(0, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))

for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
  minwage[[n_int]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n_int,sep=""),5])
}

var55_tot <- min(as.numeric(as.character(minwage)))
## -------------------------------

# column to add :  casestudy_name,  harvest_rule_code, paste(harvest_rule_code, "-", harvest_rule_level, sep="")
 percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)
 
 existCI <- F
 
 for (spee in 1:length(BMT_SPECIES)) {
 if (!existCI) {

 ALADYM_spe <<- spee
source(paste(ALADYM_home, "/src/paths.r", sep=""))


production_table_percentiles <- try(read.table(paste(PRODUCTION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";") )

if (class(production_table_percentiles) != "try-error") {
     existCI <- T
}

}
 }


 if (existCI) {
 
for (yy_f in 1:length(all_the_years)) {

yy <- yy_f + simperiod

year_name_in_row <-  years.forecast[yy_f]

# added by PA
    var1_tot <- 0
    var9_tot <- 0   
        
    var13_tot <- vector(mode="numeric", length=5)
	    var13_tot_discard <- vector(mode="numeric", length=5)
    var14_tot <- vector(mode="numeric", length=5)
	    var14_tot_disc <- vector(mode="numeric", length=5)
		    var14_tot_land <- vector(mode="numeric", length=5)

    var15_tot <- vector(mode="numeric", length=5)

    var17_tot <- vector(mode="numeric", length=5)
    var18_tot <- vector(mode="numeric", length=5)
    var19_tot <- vector(mode="numeric", length=5)
    var20_tot <- vector(mode="numeric", length=5)
    var22_tot <- vector(mode="numeric", length=5)
    var23_tot <- vector(mode="numeric", length=5)
    var24_tot <- vector(mode="numeric", length=5)
    var25_tot <- vector(mode="numeric", length=5)
    var26_tot <- vector(mode="numeric", length=5)
    var27_tot <- vector(mode="numeric", length=5)
    var27_tot_bis <- vector(mode="numeric", length=5)
    var28_tot <- vector(mode="numeric", length=5)
    
    var29_tot <- 0

    var32_tot <- vector(mode="numeric", length=5)
    var33_tot <- vector(mode="numeric", length=5)
    var34_tot <- vector(mode="numeric", length=5)
    var35_tot <- vector(mode="numeric", length=5)
    var36_tot <- vector(mode="numeric", length=5)
    var37_tot <- vector(mode="numeric", length=5)
    var38_tot <- vector(mode="numeric", length=5)

    var40_tot <- vector(mode="numeric", length=5)
    var41_tot <- vector(mode="numeric", length=5)
    #var46_tot <- 0
# ---------------------------------------------

    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
    
var1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@VESSELS.annual ))       
var9 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@DAYS.annual    ))

var13 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.landings.CI.perc   ))
to_add13 <- data.frame(  cbind(rep("total.landings", n=5), cbind(round(var13,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <-  to_add13 

var13_discard <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.discards.CI.perc   ))
to_add13_discard <- data.frame(  cbind(rep("total.discards", n=5), cbind(round(var13_discard,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add13_discard)  

var14 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.CI.perc   ))
to_add14 <- data.frame(  cbind(rep("total.revenues", n=5), cbind(round(var14,numb_digits), cbind(rep("?", n=5), percentiles_numb) ) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14)

var14_land <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landings.CI.perc   ))
to_add14_land <- data.frame(  cbind(rep("total.revenues.landing", n=5), cbind(round(var14_land,numb_digits), cbind(rep("?", n=5), percentiles_numb) ) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14_land)

var14_disc <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landed_discard.CI.perc   ))
to_add14_disc <- data.frame(  cbind(rep("total.revenues.discard", n=5), cbind(round(var14_disc,numb_digits), cbind(rep("?", n=5), percentiles_numb) ) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14_disc)

var17 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost.CI.perc$tot.var.cost  ))
to_add17 <- data.frame( cbind(rep("variable.cost[tot.var.cost]", n=5), cbind(round(var17,numb_digits), cbind(rep( "?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add17)  

var18 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost.CI.perc$fuel.cost   ))
to_add18 <- data.frame(  cbind(rep("variable.cost[fuel.cost]", n=5), cbind(round(var18,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add18)

var19 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost.CI.perc$commercial.cost   ))
to_add19 <- data.frame( cbind(rep("variable.cost[commercial.cost]", n=5), cbind(round(var19,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add19)

var20 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost.CI.perc$other.var.cost  ))
to_add20 <- data.frame(  cbind(rep("variable.cost[other.var.cost]", n=5), cbind(round(var20,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add20)

var22 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$tot.fix.cost   ))
to_add22 <- data.frame(  cbind(rep("fixed.cost[tot.fix.cost]", n=5), cbind( round(var22,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add22)

var23 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$maint.cost    )) 
to_add23 <- data.frame(  cbind(rep("fixed.cost[maint.cost]", n=5), cbind(round(var23,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add23)

var24 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$other.fix.cost   ))  
to_add24 <- data.frame(  cbind(rep("fixed.cost[other.fix.cost]", n=5), cbind( round(var24,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add24)

var25 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$ess.cost        ))  
to_add25 <- data.frame(  cbind(rep("fixed.cost[ess.cost]", n=5), cbind(round(var25,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add25)

var26 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$avoid.main.cost  ))  
to_add26 <- data.frame( cbind(rep("fixed.cost[avoid.main.cost]", n=5), cbind( round(var26,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add26)

var27 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$unavoid.main.cost  )) 
to_add27 <- data.frame(  cbind(rep("fixed.cost[unavoid.main.cost]", n=5), cbind(round(var27,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add27)

var27_bis <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$new.equipment.cost  )) 
to_add27_bis <- data.frame(  cbind(rep("new.equipment.cost", n=5), cbind( round(var27_bis,numb_digits),cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add27_bis)

var28 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@labour.cost.CI.perc       ))            
to_add28 <- data.frame(  cbind(rep("labour.cost", n=5), cbind( round(var28,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add28)

var29 <- rep(as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.cost$tot.cap.cost[1]  )) , n=5)
to_add29 <- data.frame(  cbind(rep("capital.cost[tot.cap.cost]", n=5), cbind( round(var29,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add29)

var32 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.cost.CI.perc            ))     
to_add32 <- data.frame( cbind(rep("total.cost", n=5), cbind(round(var32,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add32)

var33 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@gross.value.added.CI.perc     ))  
to_add33 <- data.frame( cbind(rep("gross.value.added", n=5), cbind(round(var33,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add33)

var34 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@gross.cash.flow.CI.perc     ))   
to_add34 <- data.frame( cbind(rep("gross.cash.flow", n=5), cbind( round(var34,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add34)

var35 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@profit.CI.perc             ))    
to_add35 <- data.frame( cbind(rep("profit", n=5), cbind( round(var35,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add35) 

var36 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@investment.CI.perc$number   ))
to_add36 <- data.frame( cbind(rep("investment[number]", n=5), cbind( round(var36,numb_digits), cbind(rep("number", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add36)

var37 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@investment.CI.perc$value  ))  
to_add37 <- data.frame( cbind(rep("investment[value]", n=5), cbind(round(var37,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add37)

var38 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.value.CI.perc     ))   
to_add38 <- data.frame( cbind(rep("capital.value", n=5), cbind( round(var38,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add38) 

var40 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@net.profit.CI.perc    ))     
to_add40 <- data.frame( cbind(rep("net.profit", n=5), cbind( round(var40,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add40)

var41 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@employment.CI.perc     ))    
to_add41 <- data.frame( cbind(rep("employment", n=5), cbind( round(var41,numb_digits), cbind(rep("number", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add41)

var42 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@average.wage.CI.perc    ))   
to_add42 <- data.frame( cbind(rep("average.wage", n=5), cbind(round(var42,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add42)

var44 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.BER.CI.perc )) 
to_add44 <- data.frame( cbind(rep("Economic.indicators[break.even.revenue]", n=5), cbind(round(var44,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add44)

var45 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.ROI.CI.perc            ))   
to_add45 <- data.frame( cbind(rep("Economic.indicators[ROI]",n=5), cbind(round(var45,numb_digits), cbind(rep("", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add45)  

var46 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.NPV.discounted.CI.perc        ))   
to_add46 <- data.frame( cbind(rep("Economic.indicators[NPV.discounted]",,n=5), cbind( round(var46,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add46)

var47 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.landings.day.CI.perc ))  
to_add47 <- data.frame( cbind(rep("Economic.indicators[tot.landings.day]",n=5), cbind(round(var47,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add47)

var48 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.landings.vessel.CI.perc ))
to_add48 <- data.frame( cbind(rep("Economic.indicators[tot.landings.vessel]",n=5), cbind(round(var48,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add48)

var49 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.revenues.day.CI.perc ))     
to_add49 <- data.frame( cbind(rep("Economic.indicators[tot.revenues.day]",n=5), cbind( round(var49,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add49)

var50 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.revenues.vessel.CI.perc  )) 
to_add50 <- data.frame( cbind(rep("Economic.indicators[tot.revenues.vessel]",n=5), cbind(round(var50,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add50)

var51 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.GVA.vessel.CI.perc    ))    
to_add51 <- data.frame( cbind(rep("Economic.indicators[GVA.vessel]",n=5), cbind(round(var51,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add51)

var52 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.GCF.vessel.CI.perc      )) 
to_add52 <- data.frame( cbind(rep("Economic.indicators[GCF.vessel]",n=5), cbind( round(var52,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add52)

var53 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.profit.vessel.CI.perc   ))  
to_add53 <- data.frame( cbind(rep("Economic.indicators[profit.vessel]",n=5), cbind(round(var53,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add53)

var54 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.net.profit.vessel.CI.perc )) 
to_add54 <- data.frame( cbind(rep("Economic.indicators[net.profit.vessel]",n=5), cbind(round(var54,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54)

var54_1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.R_BER.CI.perc )) 
to_add54_1 <- data.frame( cbind(rep("Economic.indicators[CR.BER]",n=5), cbind( round(var54_1,numb_digits), cbind(rep("", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_1)

var54_2 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.vessel.util.ratio.CI.perc ))
to_add54_2 <- data.frame( cbind(rep("Economic.indicators[vessel.util.ratio]",n=5), cbind( round(var54_2,numb_digits),cbind(rep("", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_2)

var54_3 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.RoFTA.CI.perc ))  
to_add54_3 <- data.frame( cbind(rep("Economic.indicators[RoFTA]",n=5), cbind( round(var54_3,numb_digits), cbind(rep("", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_3)

var54_4 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.NP.cap.value.CI.perc ))  
to_add54_4 <- data.frame( cbind(rep("Economic.indicators[NP.cap.value]",n=5), cbind( round(var54_4,numb_digits), cbind(rep("", n=5), percentiles_numb)) ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_4)

nro <- nrow(temp_indicators_by_fleet)
temp_indicators_by_fleet <- data.frame(cbind(rep(casestudy_name, nro) , cbind( rep(harvest_rule_code, nro), cbind( paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind( BMT_FLEETSEGMENTS[n_int], cbind( rep("ALL" , nro), cbind( rep(year_name_in_row, nro) ,temp_indicators_by_fleet )))))), stringsAsFactors=F)

#   , "ALL" (species)

colnames(temp_indicators_by_fleet) <- head_table

table_output <- data.frame(rbind(table_output, temp_indicators_by_fleet ))
 
 
 # Estimation of values for the whole fleet
 var1_tot <- sum(as.numeric(as.character(var1_tot)), as.numeric(as.character(var1)) , na.rm=T)
 var9_tot <- sum(as.numeric(as.character(var9_tot)) , as.numeric(as.character(var9))   , na.rm=T)
 
 
var13_tot <- rowSums(cbind(as.numeric(as.character(var13_tot)) , as.numeric(as.character(var13)))    , na.rm=T)
var13_tot_discard <- rowSums(cbind(as.numeric(as.character(var13_tot_discard)) , as.numeric(as.character(var13_discard))    )    , na.rm=T)

var14_tot <- rowSums(cbind(as.numeric(as.character(var14_tot)) , as.numeric(as.character(var14))    )    , na.rm=T)

var14_tot_disc <- rowSums(cbind(as.numeric(as.character(var14_tot_disc)) , as.numeric(as.character(var14_disc))    )    , na.rm=T)
var14_tot_land <- rowSums(cbind(as.numeric(as.character(var14_tot_land)) , as.numeric(as.character(var14_land))    )    , na.rm=T)



var17_tot <- rowSums(cbind(as.numeric(as.character(var17_tot)) , as.numeric(as.character(var17))     )    , na.rm=T)
var18_tot <- rowSums(cbind(as.numeric(as.character(var18_tot)) , as.numeric(as.character(var18))     )    , na.rm=T)
var19_tot <- rowSums(cbind(as.numeric(as.character(var19_tot)) , as.numeric(as.character(var19))     )    , na.rm=T)
var20_tot <- rowSums(cbind( as.numeric(as.character(var20_tot)) , as.numeric(as.character(var20))      )    , na.rm=T)
var22_tot <- rowSums(cbind(as.numeric(as.character(var22_tot)) , as.numeric(as.character(var22))      )    , na.rm=T)
var23_tot <- rowSums(cbind(as.numeric(as.character(var23_tot)) , as.numeric(as.character(var23))      )    , na.rm=T)
var24_tot <- rowSums(cbind(as.numeric(as.character(var24_tot)) , as.numeric(as.character(var24))      )    , na.rm=T)
var25_tot <- rowSums(cbind(as.numeric(as.character(var25_tot)) , as.numeric(as.character(var25))     )    , na.rm=T)
var26_tot <- rowSums(cbind(as.numeric(as.character(var26_tot)) , as.numeric(as.character(var26))     )    , na.rm=T)
var27_tot <- rowSums(cbind(as.numeric(as.character(var27_tot)) , as.numeric(as.character(var27))     )    , na.rm=T)
var27_tot_bis <- rowSums(cbind(as.numeric(as.character(var27_tot_bis)) , as.numeric(as.character(var27_bis))     )    , na.rm=T)
var28_tot <- rowSums(cbind(as.numeric(as.character(var28_tot)) , as.numeric(as.character(var28))      )    , na.rm=T)

var29_tot <- sum(as.numeric(as.character(var29_tot)) , as.numeric(as.character(var29[1]))      , na.rm=T)


var32_tot <- rowSums(cbind(as.numeric(as.character(var32_tot)) , as.numeric(as.character(var32))     )    , na.rm=T)
var33_tot <-rowSums(cbind( as.numeric(as.character(var33_tot)) , as.numeric(as.character(var33))     )    , na.rm=T)
var34_tot <- rowSums(cbind(as.numeric(as.character(var34_tot)) , as.numeric(as.character(var34))      )    , na.rm=T)
var35_tot <- rowSums(cbind(as.numeric(as.character(var35_tot)) , as.numeric(as.character(var35))      )    , na.rm=T)
var36_tot <- rowSums(cbind(as.numeric(as.character(var36_tot)) , as.numeric(as.character(var36))      )    , na.rm=T)
var37_tot <-rowSums(cbind( as.numeric(as.character(var37_tot)) , as.numeric(as.character(var37))      )    , na.rm=T)
var38_tot <- rowSums(cbind(as.numeric(as.character(var38_tot)) , as.numeric(as.character(var38))       )    , na.rm=T)

var40_tot <- rowSums(cbind(as.numeric(as.character(var40_tot)) , as.numeric(as.character(var40))      )    , na.rm=T)
var41_tot <- rowSums(cbind(as.numeric(as.character(var41_tot)) , as.numeric(as.character(var41))       )    , na.rm=T)
#var46_tot <- sum(as.numeric(as.character(var46_tot)) , as.numeric(as.character(var46))       , na.rm=T)
 
     
    } #  end loop fleet segments 
    
               
# added by PA

 numb_digits = 3
    
    to_add13_tot <- data.frame( cbind(rep("total.landings", n=5), cbind(round(var13_tot,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <- to_add13_tot  

     to_add13_tot_disc <- data.frame( cbind(rep("total.discards", n=5), cbind(round(var13_tot_discard,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <-  rbind(temp_indicators_fleet_overall, to_add13_tot_disc)    
    
    to_add14_tot <- data.frame( cbind(rep("total.revenues", n=5), cbind( round(var14_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot) 

to_add14_tot_land <- data.frame( cbind(rep("total.revenues.landing", n=5), cbind( round(var14_tot_land,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot_land)

to_add14_tot_disc <- data.frame( cbind(rep("total.revenues.discard", n=5), cbind( round(var14_tot_disc,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot_disc)     
    
    to_add15_tot <- data.frame( cbind(rep("other.income", n=5), cbind( round(var15_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add15_tot)   
        
    to_add17_tot <- data.frame( cbind(rep("variable.cost[tot.var.cost]", n=5), cbind( round(var17_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add17_tot)   
    
    to_add18_tot <- data.frame( cbind(rep("variable.cost[fuel.cost]", n=5), cbind(round(var18_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add18_tot)  
    
    to_add19_tot <- data.frame( cbind(rep("variable.cost[commercial.cost]", n=5), cbind(round(var19_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add19_tot)  
    
    to_add20_tot <- data.frame( cbind(rep("variable.cost[other.var.cost]", n=5), cbind(round(var20_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add20_tot)  
    
    to_add22_tot <- data.frame( cbind(rep("fixed.cost[tot.fix.cost]", n=5), cbind( round(var22_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add22_tot)  
    
    to_add23_tot <- data.frame( cbind(rep("fixed.cost[maint.cost]", n=5), cbind(round(var23_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add23_tot)  
    
    to_add24_tot <- data.frame( cbind(rep("fixed.cost[other.fix.cost]", n=5), cbind( round(var24_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add24_tot)  
    
    to_add25_tot <- data.frame( cbind(rep("fixed.cost[ess.cost]", n=5), cbind( round(var25_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add25_tot)  
    
    to_add26_tot <- data.frame( cbind(rep("fixed.cost[avoid.main.cost]", n=5), cbind(round(var26_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add26_tot)  
    
    to_add27_tot <- data.frame( cbind(rep("fixed.cost[unavoid.main.cost]", n=5), cbind( round(var27_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ), stringsAsFactors=F )
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add27_tot) 
	
	    to_add27_tot_bis <- data.frame( cbind(rep("new.equipment.cost", n=5), cbind( round(var27_tot_bis,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add27_tot_bis)   
    
    to_add28_tot <- data.frame( cbind(rep("labour.cost", n=5), cbind(round(var28_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add28_tot)  
    
    
    to_add29_tot <-  data.frame( cbind(rep("capital.cost[tot.cap.cost]", n=5), cbind(round(rep(var29_tot, n=5),numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add29_tot)  
    
    
    to_add32_tot <- data.frame( cbind(rep("total.cost", n=5), cbind( round(var32_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add32_tot)   
    
    to_add33_tot <- data.frame( cbind(rep("gross.value.added", n=5), cbind( round(var33_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add33_tot)  
    
    to_add34_tot <- data.frame( cbind(rep("gross.cash.flow", n=5), cbind( round(var34_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add34_tot) 
    
    to_add35_tot <- data.frame( cbind(rep("profit", n=5), cbind( round(var35_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add35_tot) 
    
    to_add36_tot <- data.frame( cbind(rep("investment[number]", n=5), cbind( round(var36_tot,numb_digits), cbind(rep("number", n=5), percentiles_numb)) ) , stringsAsFactors=F)
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add36_tot)  
    
    to_add37_tot <- data.frame( cbind(rep("investment[value]", n=5), cbind( round(var37_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add37_tot) 
    
    to_add38_tot <- data.frame( cbind(rep("capital.value", n=5), cbind( round(var38_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add38_tot) 
    
    to_add40_tot <- data.frame( cbind(rep("net.profit", n=5), cbind( round(var40_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add40_tot)  
    
    to_add41_tot <- data.frame( cbind(rep("employment", n=5), cbind( round(var41_tot,numb_digits), cbind(rep("number", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add41_tot)    

    var42_tot <- as.numeric(as.character(var28_tot))/as.numeric(as.character(var41_tot))
    to_add42_tot <- data.frame( cbind(rep("average.wage", n=5), cbind( round(var42_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
    temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add42_tot) 
    
    # var44_tot <- (as.numeric(as.character(var28_tot))+as.numeric(as.character(var22_tot))+as.numeric(as.character(var29_tot)))/(1-(as.numeric(as.character(var17_tot))/as.numeric(as.character(var14_tot))))
 
 res_BER_vect <- rep(0, 5)
  
  # BER <- (fixed_cost + capital_cost) /  (1 - ( (labour_cost + variable_cost + fixed_cost) / ) )
  
  for (lres_v in 1:5) {
    res_BER_vect[lres_v] <-  sum(as.numeric(as.character(var22_tot[lres_v])),as.numeric(as.character(var29_tot)), na.rm=T)/ (1-(  sum(as.numeric(as.character(var28_tot[lres_v])), as.numeric(as.character(var17_tot[lres_v])) ,  as.numeric(as.character(var23_tot[lres_v])) , na.rm=T) /as.numeric(as.character(var14_tot[lres_v])) ))
  } 
  
  var44_tot <- res_BER_vect
  
  # codice che stava
  #  var44_tot <- (as.numeric(as.character(var22_tot))+as.numeric(as.character(var29_tot)))/(1-(  (as.numeric(as.character(var28_tot))+ as.numeric(as.character(var17_tot)) +  as.numeric(as.character(var23_tot)) ) /sum(as.numeric(as.character(var14_tot_land)) ,as.numeric(as.character(var14_tot_disc )), na.rm=T)))
  
  # DETERMINISTIC CODE
 # var44_tot <- sum(as.numeric(as.character(var22_tot)),as.numeric(as.character(var29_tot)), na.rm=T)/ (1-(  sum(as.numeric(as.character(var28_tot)), as.numeric(as.character(var17_tot)) ,  as.numeric(as.character(var23_tot)) , na.rm=T) /sum(as.numeric(as.character(var14_tot)) ,as.numeric(as.character(var14_tot_discard )), na.rm=T) ))

    to_add44_tot <- data.frame( cbind(rep("Economic.indicators[break.even.revenue]", n=5), cbind(round(var44_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
        temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add44_tot) 
        
        R_BER_all  <-  var14_tot / var44_tot

        to_add_R_BER_all <- data.frame( cbind(rep("Economic.indicators[CR.BER]", n=5), cbind(round(R_BER_all, numb_digits), cbind(rep("", n=5), percentiles_numb)) ), stringsAsFactors=F) 
        temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add_R_BER_all) 
   
    ROFTA_with_NP_all <- as.numeric(as.character(var40_tot))/as.numeric(as.character(var38_tot))
    to_addROFTA_with_NP_all <- data.frame( cbind(rep("Economic.indicators[NP.cap.value]", n=5), cbind(round(ROFTA_with_NP_all,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
    temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_addROFTA_with_NP_all) 

        var45_tot <- as.numeric(as.character(var35_tot))/as.numeric(as.character(var38_tot))
    to_add45_tot <- data.frame( cbind(rep("Economic.indicators[ROI]", n=5), cbind(round(var45_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add45_tot) 
          
     var47_tot <- as.numeric(as.character(var13_tot))/as.numeric(as.character(var9_tot))   
    to_add47_tot <- data.frame( cbind(rep("Economic.indicators[tot.landings.day]", n=5), cbind( round(var47_tot,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F)
            temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add47_tot) 
            
    var48_tot <- as.numeric(as.character(var13_tot))/as.numeric(as.character(var1_tot))    
    to_add48_tot <- data.frame( cbind(rep("Economic.indicators[tot.landings.vessel]", n=5), cbind( round(var48_tot,numb_digits), cbind(rep("kg", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
           temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add48_tot) 
           
        var49_tot <- as.numeric(as.character(var14_tot))/as.numeric(as.character(var9_tot))
    to_add49_tot <- data.frame( cbind(rep("Economic.indicators[tot.revenues.day]", n=5), cbind(round(var49_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
           temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add49_tot) 
           
        var50_tot <- as.numeric(as.character(var14_tot))/as.numeric(as.character(var1_tot))    
    to_add50_tot <- data.frame( cbind(rep("Economic.indicators[tot.revenues.vessel]", n=5), cbind( round(var50_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F)
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add50_tot) 
          
      var51_tot <- as.numeric(as.character(var33_tot))/as.numeric(as.character(var1_tot))  
    to_add51_tot <- data.frame( cbind(rep("Economic.indicators[GVA.vessel]", n=5), cbind( round(var51_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add51_tot) 
          
      var52_tot <- as.numeric(as.character(var34_tot))/as.numeric(as.character(var1_tot))  
    to_add52_tot <- data.frame( cbind(rep("Economic.indicators[GCF.vessel]", n=5), cbind( round(var52_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
         temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add52_tot) 
         
        var53_tot <- as.numeric(as.character(var35_tot))/as.numeric(as.character(var1_tot))
    to_add53_tot <- data.frame( cbind(rep("Economic.indicators[profit.vessel]", n=5), cbind( round(var53_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) , stringsAsFactors=F) 
             temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add53_tot) 
             
        var54_tot <- as.numeric(as.character(var40_tot))/as.numeric(as.character(var1_tot))
    to_add54_tot <- data.frame( cbind(rep("Economic.indicators[net.profit.vessel]", n=5), cbind( round(var54_tot,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ), stringsAsFactors=F) 
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add54_tot) 
  
   
    nro_ov <- nrow(temp_indicators_fleet_overall)
temp_indicators_fleet_overall <- data.frame(cbind(rep(casestudy_name, nro_ov) , cbind( rep(harvest_rule_code, nro_ov), cbind( paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind( "ALL", cbind( rep("ALL" , nro_ov), cbind( rep(year_name_in_row, nro_ov) , temp_indicators_fleet_overall )))))), stringsAsFactors=F)

#   , "ALL" (species)

colnames(temp_indicators_fleet_overall) <- head_table

table_output <- data.frame(rbind(table_output, temp_indicators_fleet_overall ), stringsAsFactors=F)


 # ------------------------------------------------------------------------------
}   #  end loop years  # ONLY FORECAST

for (yy_f in 1:length(all_the_years)) {

  yy <- yy_f + simperiod

year_name_in_row <- years.forecast[yy_f]

  for (m_int in 1:length(BMT_SPECIES) ) {         

production_table_percentiles <- try(read.table(paste(PRODUCTION_table_CI, " quantiles.csv", sep=""),header=TRUE,sep=";") )

if (class(production_table_percentiles) != "try-error") {

    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
                  
          varsp1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing.weight.CI.perc[,m_int] ))
          varsp2 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing.number.CI.perc[,m_int]))
          varsp5 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@discard.weight.CI.perc[,m_int] ))
          varsp6 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@discard.number.CI.perc[,m_int]))
          varsp3 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@price.CI.perc[,m_int]    ))
          varsp4 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@revenues.landing.CI.perc[,m_int]  ))
 varsp7 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@price.landed_discard.CI.perc[,m_int]   ))
varsp8 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@revenues.landed_discard.CI.perc[,m_int]    ))
    
      landObl_sp <- as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing_obligation[m_int]    )
        
       to_addspLandObl <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("landing.obligation", n=5), cbind(landObl_sp, cbind(rep("", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
    
                  
          to_addsp1 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("landing.weight", n=5), cbind(varsp1, cbind(rep("kg", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp2 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("landing.number", n=5), cbind(varsp2, cbind(rep("number", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp5 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("discard.weight", n=5), cbind(varsp5, cbind(rep("kg", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp6 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("discard.number", n=5), cbind(varsp6, cbind(rep("number", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp3 <- data.frame(cbind(rep(casestudy_name, n=5), cbind(rep( harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("price", n=5), cbind(varsp3, cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp4 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("revenues.landing", n=5), cbind(varsp4, cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          
          to_addsp7 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("price.discard", n=5), cbind(varsp7, cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)
          to_addsp8 <- data.frame(cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep(BMT_SPECIES[m_int], n=5), cbind(rep(year_name_in_row, n=5), cbind(rep("revenues.discard", n=5), cbind(varsp8, cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F)		 
       
          colnames(to_addsp1) <- head_table
          colnames(to_addsp2) <- head_table
          colnames(to_addsp3) <- head_table
          colnames(to_addsp4) <- head_table
          colnames(to_addsp5) <- head_table
          colnames(to_addsp6) <- head_table
                    colnames(to_addsp7) <- head_table
                              colnames(to_addsp8) <- head_table
                              colnames(to_addspLandObl) <- head_table
          
          table_output <- data.frame( rbind(table_output, rbind(to_addspLandObl, rbind(to_addsp1,	rbind(to_addsp2,	rbind(to_addsp3,	rbind(	to_addsp4, rbind(to_addsp5,	rbind(to_addsp6,	rbind(to_addsp7,	to_addsp8)))) ))))) , stringsAsFactors=F)
    
     } #  end loop fleet segments
     
     }
     
     
  } # end loop species 
}   #  end loop years      # ONLY FORECAST



  for (n_int in 1:length(BMT_FLEETSEGMENTS)) {

 numb_digits = 3
 
 var46_a <- as.numeric(as.character(Fleetyear[[foreperiod]]@fleetsegments[[n_int]]@EC.NPV15.CI.perc         ))   
to_add46_a <- data.frame( cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("Economic.indicators[NPV15]", n=5), cbind(round(var46_a,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F) 
colnames(to_add46_a) <- head_table
 table_output <- rbind(table_output,  to_add46_a)

 var46_b <- as.numeric(as.character(Fleetyear[[foreperiod]]@fleetsegments[[n_int]]@EC.NPV15.infinite.CI.perc         ))   
to_add46_b <- data.frame( cbind(rep(casestudy_name, n=5), cbind(rep( harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep(BMT_FLEETSEGMENTS[n_int], n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("Economic.indicators[NPV15.infinite]", n=5), cbind( round(var46_b,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F) 
colnames(to_add46_b) <- head_table
 table_output <- data.frame(rbind(table_output,  to_add46_b) , stringsAsFactors=F)

}

NPV15_vect <- vector(mode="numeric", length=5)
for (PERC in percentiles_numb) {
 NPV15_vect[PERC] <- sum(as.numeric(as.character(table_output$Value[table_output$Variable == "Economic.indicators[NPV15]" &  table_output$quantile == percentiles_numb[PERC]])), na.rm=T) 
 }
 
to_add46_a_tot <- data.frame( cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("Economic.indicators[NPV15]", n=5), cbind( round(NPV15_vect,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) ) , stringsAsFactors=F) 
colnames(to_add46_a_tot) <- head_table
 table_output <- data.frame( rbind(table_output,  to_add46_a_tot), stringsAsFactors=F)
 
 
NPV15_infinite_vect <- vector(mode="numeric", length=5)
for (PERC in percentiles_numb) {
 NPV15_infinite_vect[PERC] <- sum(as.numeric(as.character(table_output$Value[table_output$Variable == "Economic.indicators[NPV15.infinite]" &  table_output$quantile == percentiles_numb[PERC]])), na.rm=T) 
 }

to_add46_b_tot <- data.frame( cbind(rep(casestudy_name, n=5), cbind( rep(harvest_rule_code, n=5), cbind(rep(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("ALL", n=5), cbind(rep("Economic.indicators[NPV15.infinite]", n=5), cbind( round(NPV15_infinite_vect,numb_digits), cbind(rep("?", n=5), percentiles_numb)) ) ) ) ) ) ) )  , stringsAsFactors=F) 
colnames(to_add46_b_tot) <- head_table
 table_output <- data.frame(rbind(table_output,  to_add46_b_tot) , stringsAsFactors=F)


table_output <- data.frame(cbind(table_output[,colnames(table_output) != "quantile"], cbind(rep("", nrow(table_output)), table_output[,colnames(table_output) == "quantile"]) ), stringsAsFactors=F)
colnames(table_output)[ncol(table_output)-1] <- "Comments"
  
# questa ? l'istruzione per scrivere il file csv. Il percorso verr? creato a partire dalle variabili nel file di configurazione
# write.table(table_output, file=paste(casestudy_path, "\\", casestudy_name, "_HR", harvest_rule_code, harvest_rule_level," - economic output.csv", sep=""), sep=";", row.names=F)
  print(proc.time() - time_star_eco, quote=F )

} # end check CI   
  
   if (nrow(table_output) != 0) {
 if (length(table_output$Value[!is.finite(as.numeric(as.character(table_output$Value)))] ) != 0) {
table_output$Value[!is.finite(as.numeric(as.character(table_output$Value)))] <- NA
}
 if (length(table_output$Value[as.numeric(as.character(table_output$Value)) == -1] ) != 0) {
table_output$Value[as.numeric(as.character(table_output$Value)) == -1] <- NA
}
#write.table(table_output, paste(getwd(), "/test_Pressure_CI_table.csv", sep=""), sep=";", row.names=F)
#
#table_output <- data.frame(read.csv(paste(getwd(), "/test_Pressure_CI_table.csv", sep=""), sep=";"), stringsAsFactors=F)
#table_output$Value <- as.numeric(as.character(table_output$Value))


   name_eco_sim <- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Economic output.csv", sep="")
table_output_past <-  data.frame(read.csv(name_eco_sim, sep=";") , stringsAsFactors=F)


species_to_add_past <-  unique(as.character(table_output$Species)) 
# species_to_add_past <- species_to_add_past[species_to_add_past != "ALL"]
                                                                                                 # | table_output_past$Species == "ALL"
table_output_past <- table_output_past[table_output_past$Species %in% species_to_add_past, ]

for (PERC in 1:length(percentiles_numb))  {
   table_output_past_perc <- data.frame(  cbind(table_output_past, rep(percentiles_numb[PERC], nrow(table_output_past))) , stringsAsFactors=F)
colnames(table_output_past_perc) <- colnames(table_output)

#table_output_past_perc$Value <- as.numeric(as.character(table_output_past_perc$Value))
   table_output <- data.frame(rbind(table_output, table_output_past_perc) , stringsAsFactors=F)
}

table_output <- table_output[with(table_output, order( Year)), ]
colnames(table_output) <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Species",	"Year",	"Variable",	"Value",	"Unit", "Comments", "quantile") 

 table_output$Scenario <- harvest_rule_code
table_output$ID_scenario <- harvest_rule_id
}



  
return(table_output)
}
