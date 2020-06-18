# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





getEconomicTable <- function(all_the_years) {

time_star_eco <- proc.time()

head_table <- c("Case_study", "Scenario",	"ID_scenario",	"Fleet_segment",	"Species",	"Year",	"Variable",	"Value",	"Unit")

table_output <- data.frame(matrix(nrow=0, ncol= 9))
colnames(table_output) <- head_table

casestudy_name <- as.character(cfg[rownames(cfg) == "casestudy.name",1])
casestudy_path <- as.character(cfg[rownames(cfg) == "casestudy.name",2])
harvest_rule_code <- ifelse(all(years.forecast %in% all_the_years),  paste("HR", as.character(cfg[rownames(cfg) == "casestudy.HR",1]), sep="") , "Diagnosis")
harvest_rule_level <- ifelse(all(years.forecast %in% all_the_years),  as.character(cfg[rownames(cfg) == "casestudy.HR",2]), "-")
harvest_rule_id <- ifelse(all(years.forecast %in% all_the_years), paste(harvest_rule_code, "-", harvest_rule_level, sep=""), "-" )

# added by PA
minwage <- data.frame(matrix(0, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))

if (phase == "FORECAST") {
for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
  minwage[[n_int]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n_int,sep=""),5])
}
}

var55_tot <- min(as.numeric(as.character(minwage)))
## -------------------------------

# column to add :  casestudy_name,  harvest_rule_code, paste(harvest_rule_code, "-", harvest_rule_level, sep="")

for (yy in 1:length(all_the_years)) {

year_name_in_row <- ifelse(yy<=simperiod, years[yy], years.forecast[yy-simperiod])

# added by PA
    var1_tot <- 0
    var3_tot <- 0
    var6_tot <- 0
    var9_tot <- 0
    var11_tot <- 0
    var12_tot <- 0
    var13_tot <- 0
    var13_tot_discard <- 0
    var14_tot <- 0
    var14_tot_discard <- 0
    var14_tot_land_disc <- 0
    var15_tot <- 0
    var16_tot <- 0
    var17_tot <- 0
    var18_tot <- 0
    var19_tot <- 0
    var20_tot <- 0
    var22_tot <- 0
    var23_tot <- 0
    var24_tot <- 0
    var25_tot <- 0
    var26_tot <- 0
    var27_tot <- 0
    var27_tot_bis <- 0
    var28_tot <- 0
    var29_tot <- 0
    var30_tot <- 0
    var31_tot <- 0
    var32_tot <- 0
    var33_tot <- 0
    var34_tot <- 0
    var35_tot <- 0
    var36_tot <- 0
    var37_tot <- 0
    var38_tot <- 0
    var39_tot <- 0
    var40_tot <- 0
    var41_tot <- 0
    #var46_tot <- 0
# ---------------------------------------------

    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {

 
 numb_digits = 3
 
var1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@VESSELS.annual ))       
to_add1 <- data.frame( cbind("VESSELS.annual", cbind(ifelse(length(var1) ==0, -1, round(var1,numb_digits) ), "number") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- to_add1

var2 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@GT.average.annual  )) 
to_add2 <- data.frame( cbind("GT.average.annual", cbind(ifelse(length(var2) ==0, -1, round(var2,numb_digits) ), "GT") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add2)

var3 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@GT.annual  )) 
to_add3 <- data.frame(  cbind("GT.annual", cbind(ifelse(length(var3) ==0, -1, round(var3, numb_digits) ), "GT") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add3) 

# var4 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@GT     ))					# non necessario
#to_add4 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind("ALL", cbind(year_name_in_row, cbind("GT", cbind(ifelse(length(var4) ==0, -1, var4), "GT") ) ) ) ) ) ) ) )

var5 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@KW.average.annual      ))
to_add5 <- data.frame( cbind("KW.average.annual", cbind(ifelse(length(var5) ==0, -1, round(var5,numb_digits) ), "KW") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add5)

var6 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@KW.annual ))
to_add6 <- data.frame( cbind("KW.annual", cbind(ifelse(length(var6) ==0, -1, round(var6,numb_digits)), "KW") ), stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add6)

# var7 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@KW     ))					# non necessario
#to_add7 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind("ALL", cbind(year_name_in_row, cbind("KW", cbind(ifelse(length(var7) ==0, -1, var7), "KW") ) ) ) ) ) ) ) )

var8 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@DAYS.average.annual   ))
to_add8 <- data.frame( cbind("DAYS.average.annual", cbind(ifelse(length(var8) ==0, -1, round(var8,numb_digits)), "days") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add8)

var9 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@DAYS.annual    ))
to_add9 <- data.frame( cbind("DAYS.annual", cbind(ifelse(length(var9) ==0, -1, round(var9,numb_digits)), "days") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add9) 

var10 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@MAXDAYS.average.annual ))
to_add10 <- data.frame(cbind("MAXDAYS.average.annual", cbind(ifelse(length(var10) ==0, -1, round(var10,numb_digits)), "days") ), stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add10)  

var11 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@GT.DAYS.annual    ))
to_add11 <- data.frame( cbind("GT.DAYS.annual", cbind(ifelse(length(var11) ==0, -1, round(var11,numb_digits)), "GT*days") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add11)

var12 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@KW.DAYS.annual   ))
to_add12 <- data.frame(  cbind("KW.DAYS.annual", cbind(ifelse(length(var12) ==0, -1, round(var12,numb_digits)), "KW*days") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add12)  

var13 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.landings   ))
to_add13 <- data.frame(  cbind("total.landings", cbind(ifelse(length(var13) ==0, -1, round(var13,numb_digits)), "kg") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add13) 

var13_discard <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.discards   ))
to_add13_discard <- data.frame(  cbind("total.discards", cbind(ifelse(length(var13_discard) ==0, -1, round(var13_discard,numb_digits)), "kg") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add13_discard)

#print(var13)

var14 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landings   ))
to_add14 <- data.frame(  cbind("total.revenues.landing", cbind(ifelse(length(var14) ==0, -1, round(var14,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14)

var14_discard <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landed_discard   ))
to_add14_discard <- data.frame(  cbind("total.revenues.discard", cbind(ifelse(length(var14_discard) ==0, -1, round(var14_discard,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14_discard)

var14_land_disc <- sum(as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landings   )), as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.revenues.landed_discard   )), na.rm=T)
to_add14_land_disc <- data.frame(  cbind("total.revenues", cbind(ifelse(length(var14_land_disc) ==0, -1, round(var14_land_disc,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add14_land_disc)

var15 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@other.income   ))
to_add15 <- data.frame(  cbind("other.income", cbind(ifelse(length(var15) ==0, -1, round(var15,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add15)

var16 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.income   ))
to_add16 <- data.frame(  cbind("total.income", cbind(ifelse(length(var16) ==0, -1, round(var16,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add16)

var17 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost$tot.var.cost[1]  ))
to_add17 <- data.frame( cbind("variable.cost[tot.var.cost]", cbind(ifelse(length(var17) ==0, -1, round(var17,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add17)  

var18 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost$fuel.cost[1]   ))
to_add18 <- data.frame(  cbind("variable.cost[fuel.cost]", cbind(ifelse(length(var18) ==0, -1, round(var18,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add18)

var19 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost$commercial.cost[1]   ))
to_add19 <- data.frame( cbind("variable.cost[commercial.cost]", cbind(ifelse(length(var19) ==0, -1, round(var19,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add19)

var20 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@variable.cost$other.var.cost[1]  ))
to_add20 <- data.frame(  cbind("variable.cost[other.var.cost]", cbind(ifelse(length(var20) ==0, -1, round(var20,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add20)

var21 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fuel.price                 ))
to_add21 <- data.frame(  cbind("fuel.price", cbind(ifelse(length(var21) ==0, -1, round(var21,numb_digits)), "€/l") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add21)

var22 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$tot.fix.cost[1]   ))
to_add22 <- data.frame(  cbind("fixed.cost[tot.fix.cost]", cbind(ifelse(length(var22) ==0, -1, round(var22,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add22)

var23 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$maint.cost[1]    )) 
to_add23 <- data.frame(  cbind("fixed.cost[maint.cost]", cbind(ifelse(length(var23) ==0, -1, round(var23,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add23)

var24 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$other.fix.cost[1]    ))  
to_add24 <- data.frame(  cbind("fixed.cost[other.fix.cost]", cbind(ifelse(length(var24) ==0, -1, round(var24,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add24)

var25 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$ess.cost[1]        ))  
to_add25 <- data.frame(  cbind("fixed.cost[ess.cost]", cbind(ifelse(length(var25) ==0, -1, round(var25,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add25)

var26 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$avoid.main.cost[1]  ))  
to_add26 <- data.frame( cbind("fixed.cost[avoid.main.cost]", cbind(ifelse(length(var26) ==0, -1, round(var26,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add26)

var27 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$unavoid.main.cost[1]  )) 
to_add27 <- data.frame(  cbind("fixed.cost[unavoid.main.cost]", cbind(ifelse(length(var27) ==0, -1, round(var27,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add27)

var27_bis <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@fixed.cost$new.equipment.cost[1]  )) 
to_add27_bis <- data.frame(  cbind("new.equipment.cost", cbind(ifelse(length(var27_bis) ==0, -1, round(var27_bis,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add27_bis)

var28 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@labour.cost       ))            
to_add28 <- data.frame(  cbind("labour.cost", cbind(ifelse(length(var28) ==0, -1, round(var28,numb_digits)), "€")) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add28)

var29 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.cost$tot.cap.cost[1]  ))
to_add29 <- data.frame(  cbind("capital.cost[tot.cap.cost]", cbind(ifelse(length(var29) ==0, -1, round(var29,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add29)

var30 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.cost$depreciation[1] )) 
to_add30 <- data.frame(  cbind("capital.cost[depreciation]", cbind(ifelse(length(var30) ==0, -1, round(var30,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add30)

var31 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.cost$interest[1]    ))  
to_add31 <- data.frame(  cbind("capital.cost[interest]", cbind(ifelse(length(var31) ==0, -1, round(var31,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add31) 

var32 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@total.cost            ))     
to_add32 <- data.frame( cbind("total.cost", cbind(ifelse(length(var32) ==0, -1, round(var32,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add32)

var33 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@gross.value.added     ))  
to_add33 <- data.frame( cbind("gross.value.added", cbind(ifelse(length(var33) ==0, -1, round(var33,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add33)

var34 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@gross.cash.flow     ))   
to_add34 <- data.frame( cbind("gross.cash.flow", cbind(ifelse(length(var34) ==0, -1, round(var34,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add34)

var35 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@profit             ))    
to_add35 <- data.frame( cbind("profit", cbind(ifelse(length(var35) ==0, -1, round(var35,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add35) 

var36 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@investment$number[1]   ))
to_add36 <- data.frame( cbind("investment[number]", cbind(ifelse(length(var36) ==0, -1, round(var36,numb_digits)), "number") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add36)

var37 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@investment$value[1]  ))  
to_add37 <- data.frame( cbind("investment[value]", cbind(ifelse(length(var37) ==0, -1, round(var37,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add37)

var38 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@capital.value     ))   
to_add38 <- data.frame( cbind("capital.value", cbind(ifelse(length(var38) ==0, -1, round(var38,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add38) 

var39 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@taxes           ))     
to_add39 <- data.frame( cbind("taxes", cbind(ifelse(length(var39) ==0, -1, round(var39,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add39)

var40 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@net.profit    ))     
to_add40 <- data.frame( cbind("net.profit", cbind(ifelse(length(var40) ==0, -1, round(var40,numb_digits)), "€") ), stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add40)

var41 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@employment     ))    
to_add41 <- data.frame( cbind("employment", cbind(ifelse(length(var41) ==0, -1, round(var41,numb_digits)), "number") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add41)

var42 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@average.wage    ))   
to_add42 <- data.frame( cbind("average.wage", cbind(ifelse(length(var42) ==0, -1, round(var42,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add42)

var43 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@technology      ))    
to_add43 <- data.frame( cbind("technology", cbind(ifelse(length(var43) ==0, -1, round(var43,numb_digits)), "") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add43)

var44 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.BER )) 
to_add44 <- data.frame( cbind("Economic.indicators[break.even.revenue]", cbind(ifelse(length(var44) ==0, -1, round(var44,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add44)

var45 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.ROI            ))   
to_add45 <- data.frame( cbind("Economic.indicators[ROI]", cbind(ifelse(length(var45) ==0, -1, round(var45,numb_digits)), "") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add45)  

var46 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.NPV.discounted        ))   
to_add46 <- data.frame( cbind("Economic.indicators[NPV.discounted]", cbind(ifelse(length(var46) ==0, -1, round(var46,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add46)

var47 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.landings.day ))  
to_add47 <- data.frame( cbind("Economic.indicators[tot.landings.day]", cbind(ifelse(length(var47) ==0, -1, round(var47,numb_digits)), "kg") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add47)

var48 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.landings.vessel ))
to_add48 <- data.frame( cbind("Economic.indicators[tot.landings.vessel]", cbind(ifelse(length(var48) ==0, -1, round(var48,numb_digits)), "kg") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add48)

var49 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.revenues.day ))     
to_add49 <- data.frame( cbind("Economic.indicators[tot.revenues.day]", cbind(ifelse(length(var49) ==0, -1, round(var49,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add49)

var50 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.tot.revenues.vessel  )) 
to_add50 <- data.frame( cbind("Economic.indicators[tot.revenues.vessel]", cbind(ifelse(length(var50) ==0, -1, round(var50,numb_digits)), "€") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add50)

var51 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.GVA.vessel    ))    
to_add51 <- data.frame( cbind("Economic.indicators[GVA.vessel]", cbind(ifelse(length(var51) ==0, -1, round(var51,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add51)

var52 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.GCF.vessel      )) 
to_add52 <- data.frame( cbind("Economic.indicators[GCF.vessel]", cbind(ifelse(length(var52) ==0, -1, round(var52,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add52)

var53 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.profit.vessel   ))  
to_add53 <- data.frame( cbind("Economic.indicators[profit.vessel]", cbind(ifelse(length(var53) ==0, -1, round(var53,numb_digits)), "€") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add53)

var54 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.net.profit.vessel )) 
to_add54 <- data.frame( cbind("Economic.indicators[net.profit.vessel]", cbind(ifelse(length(var54) ==0, -1, round(var54,numb_digits)), "€") ) , stringsAsFactors=F)  
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54)

var54_1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.R_BER )) 
to_add54_1 <- data.frame( cbind("Economic.indicators[CR.BER]", cbind(ifelse(length(var54_1) ==0, -1, round(var54_1,numb_digits)), "") ) , stringsAsFactors=F) 
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_1)

var54_2 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.vessel.util.ratio ))
to_add54_2 <- data.frame( cbind("Economic.indicators[vessel.util.ratio]", cbind(ifelse(length(var54_2) ==0, -1, round(var54_2,numb_digits)), "") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_2)

var54_3 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.RoFTA ))  
to_add54_3 <- data.frame( cbind("Economic.indicators[RoFTA]", cbind(ifelse(length(var54_3) ==0, -1, round(var54_3,numb_digits)), "") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_3)

var54_4 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@EC.NP.cap.value ))  
to_add54_4 <- data.frame( cbind("Economic.indicators[NP.cap.value]", cbind(ifelse(length(var54_4) ==0, -1, round(var54_4,numb_digits)), "") ) , stringsAsFactors=F)   
temp_indicators_by_fleet <- rbind(temp_indicators_by_fleet, to_add54_4)

nro <- nrow(temp_indicators_by_fleet)
temp_indicators_by_fleet <- data.frame(cbind(rep(casestudy_name, nro) , cbind( rep(harvest_rule_code, nro), cbind( paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind( BMT_FLEETSEGMENTS[n_int], cbind( rep("ALL" , nro), cbind( rep(year_name_in_row, nro) ,temp_indicators_by_fleet )))))), stringsAsFactors=F) 

#   , "ALL" (species)

colnames(temp_indicators_by_fleet) <- head_table

table_output <- data.frame(rbind(table_output, temp_indicators_by_fleet ), stringsAsFactors=F) 
 
 
 # Estimation of values for the whole fleet
var1_tot <- sum(as.numeric(as.character(var1_tot)), as.numeric(as.character(var1)) , na.rm=T)
var3_tot <- sum(as.numeric(as.character(var3_tot)), as.numeric(as.character(var3))  , na.rm=T)
var6_tot <- sum(as.numeric(as.character(var6_tot)), as.numeric(as.character(var6))   , na.rm=T)
var9_tot <- sum(as.numeric(as.character(var9_tot)) , as.numeric(as.character(var9))   , na.rm=T)
var11_tot <- sum(as.numeric(as.character(var11_tot)), as.numeric(as.character(var11))   , na.rm=T)
var12_tot <- sum(as.numeric(as.character(var12_tot)) , as.numeric(as.character(var12))    , na.rm=T)
var13_tot <- sum(as.numeric(as.character(var13_tot)) , as.numeric(as.character(var13))    , na.rm=T)

var13_tot_discard <- sum(as.numeric(as.character(var13_tot_discard)) , as.numeric(as.character(var13_discard))    , na.rm=T)

var14_tot <- sum(as.numeric(as.character(var14_tot)) , as.numeric(as.character(var14))    , na.rm=T)

var14_tot_discard <- sum(as.numeric(as.character(var14_tot_discard)) , as.numeric(as.character(var14_discard))    , na.rm=T)

var14_tot_land_disc  <- sum(as.numeric(as.character(var14_tot_land_disc)) , as.numeric(as.character(var14_land_disc))    , na.rm=T)

var15_tot <- sum(as.numeric(as.character(var15_tot)) , as.numeric(as.character(var15))    , na.rm=T)
var16_tot <- sum(as.numeric(as.character(var16_tot)) , as.numeric(as.character(var16))     , na.rm=T)
var17_tot <- sum(as.numeric(as.character(var17_tot)) , as.numeric(as.character(var17))     , na.rm=T)
var18_tot <- sum(as.numeric(as.character(var18_tot)) , as.numeric(as.character(var18))     , na.rm=T)
var19_tot <- sum(as.numeric(as.character(var19_tot)) , as.numeric(as.character(var19))     , na.rm=T)
var20_tot <-sum( as.numeric(as.character(var20_tot)) , as.numeric(as.character(var20))      , na.rm=T)
var22_tot <- sum(as.numeric(as.character(var22_tot)) , as.numeric(as.character(var22))      , na.rm=T)
var23_tot <- sum(as.numeric(as.character(var23_tot)) , as.numeric(as.character(var23))      , na.rm=T)
var24_tot <-sum( as.numeric(as.character(var24_tot)) , as.numeric(as.character(var24))      , na.rm=T)
var25_tot <- sum(as.numeric(as.character(var25_tot)) , as.numeric(as.character(var25))     , na.rm=T)
var26_tot <- sum(as.numeric(as.character(var26_tot)) , as.numeric(as.character(var26))     , na.rm=T)
var27_tot <- sum(as.numeric(as.character(var27_tot)) , as.numeric(as.character(var27))     , na.rm=T)
var27_tot_bis <- sum(as.numeric(as.character(var27_tot_bis)) , as.numeric(as.character(var27_bis))     , na.rm=T)
var28_tot <- sum(as.numeric(as.character(var28_tot)) , as.numeric(as.character(var28))      , na.rm=T)
var29_tot <- sum(as.numeric(as.character(var29_tot)) , as.numeric(as.character(var29))      , na.rm=T)
var30_tot <- sum(as.numeric(as.character(var30_tot)) , as.numeric(as.character(var30))     , na.rm=T)
var31_tot <-sum( as.numeric(as.character(var31_tot)) , as.numeric(as.character(var31))     , na.rm=T)
var32_tot <- sum(as.numeric(as.character(var32_tot)) , as.numeric(as.character(var32))     , na.rm=T)
var33_tot <-sum( as.numeric(as.character(var33_tot)) , as.numeric(as.character(var33))     , na.rm=T)
var34_tot <- sum(as.numeric(as.character(var34_tot)) , as.numeric(as.character(var34))      , na.rm=T)
var35_tot <- sum(as.numeric(as.character(var35_tot)) , as.numeric(as.character(var35))      , na.rm=T)
var36_tot <- sum(as.numeric(as.character(var36_tot)) , as.numeric(as.character(var36))      , na.rm=T)
var37_tot <-sum( as.numeric(as.character(var37_tot)) , as.numeric(as.character(var37))      , na.rm=T)
var38_tot <- sum(as.numeric(as.character(var38_tot)) , as.numeric(as.character(var38))       , na.rm=T)
var39_tot <- sum(as.numeric(as.character(var39_tot)) , as.numeric(as.character(var39))      , na.rm=T)
var40_tot <- sum(as.numeric(as.character(var40_tot)) , as.numeric(as.character(var40))      , na.rm=T)
var41_tot <- sum(as.numeric(as.character(var41_tot)) , as.numeric(as.character(var41))       , na.rm=T)
#var46_tot <- sum(as.numeric(as.character(var46_tot)) , as.numeric(as.character(var46))       , na.rm=T)
 
   # print(var13_tot) 
    } #  end loop fleet segments 
    
               
# added by PA

 numb_digits = 3

 to_add1_tot <- data.frame(cbind("VESSELS.annual", cbind(ifelse(length(var1_tot) ==0, -1,  round(var1_tot,numb_digits)), "number") ) , stringsAsFactors=F) 
temp_indicators_fleet_overall <- to_add1_tot
 
 to_add3_tot <- data.frame(cbind("GT.annual", cbind(ifelse(length(var3_tot) ==0, -1,  round(var3_tot,numb_digits)), "GT") ) , stringsAsFactors=F) 
temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add3_tot)
 
 to_add6_tot <- data.frame( cbind("KW.annual", cbind(ifelse(length(var6_tot) ==0, -1, round(var6_tot,numb_digits)), "KW") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add6_tot)
 
    to_add9_tot <- data.frame( cbind("DAYS.annual", cbind(ifelse(length(var9_tot) ==0, -1, round(var9_tot,numb_digits)), "days") ) , stringsAsFactors=F) 
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add9_tot)   
    
    to_add11_tot <- data.frame( cbind("GT.DAYS.annual", cbind(ifelse(length(var11_tot) ==0, -1, round(var11_tot,numb_digits)), "GT*days") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add11_tot)  
    
    to_add12_tot <- data.frame( cbind("KW.DAYS.annual", cbind(ifelse(length(var12_tot) ==0, -1, round(var12_tot,numb_digits)), "KW*days") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add12_tot)   
    
    to_add13_tot <- data.frame( cbind("total.landings", cbind(ifelse(length(var13_tot) ==0, -1, round(var13_tot,numb_digits)), "kg") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add13_tot) 
 
     to_add13_tot_discard <- data.frame( cbind("total.discards", cbind(ifelse(length(var13_tot_discard) ==0, -1, round(var13_tot_discard,numb_digits)), "kg") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add13_tot_discard)  
 
    to_add14_tot_land_disc <- data.frame( cbind("total.revenues", cbind(ifelse(length(var14_tot_land_disc) ==0, -1, round(var14_tot_land_disc,numb_digits)), "kg") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot_land_disc)  
    
    to_add14_tot <- data.frame( cbind("total.revenues.landing", cbind(ifelse(length(var14_tot) ==0, -1, round(var14_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot)  
  
     to_add14_tot_discard <- data.frame( cbind("total.revenues.discard", cbind(ifelse(length(var14_tot_discard) ==0, -1, round(var14_tot_discard,numb_digits)), "€") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add14_tot_discard)  
    
    to_add15_tot <- data.frame( cbind("other.income", cbind(ifelse(length(var15_tot) ==0, -1, round(var15_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add15_tot)   
    
    to_add16_tot <- data.frame( cbind("total.income", cbind(ifelse(length(var16_tot) ==0, -1, round(var16_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add16_tot)  
    
    to_add17_tot <- data.frame( cbind("variable.cost[tot.var.cost]", cbind(ifelse(length(var17_tot) ==0, -1, round(var17_tot,numb_digits)), "€") ), stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add17_tot)   
    
    to_add18_tot <- data.frame( cbind("variable.cost[fuel.cost]", cbind(ifelse(length(var18_tot) ==0, -1, round(var18_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add18_tot)  
    
    to_add19_tot <- data.frame( cbind("variable.cost[commercial.cost]", cbind(ifelse(length(var19_tot) ==0, -1, round(var19_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add19_tot)  
    
    to_add20_tot <- data.frame( cbind("variable.cost[other.var.cost]", cbind(ifelse(length(var20_tot) ==0, -1, round(var20_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add20_tot)  
    
    to_add22_tot <- data.frame( cbind("fixed.cost[tot.fix.cost]", cbind(ifelse(length(var22_tot) ==0, -1, round(var22_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add22_tot)  
    
    to_add23_tot <- data.frame( cbind("fixed.cost[maint.cost]", cbind(ifelse(length(var23_tot) ==0, -1, round(var23_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add23_tot)  
    
    to_add24_tot <- data.frame( cbind("fixed.cost[other.fix.cost]", cbind(ifelse(length(var24_tot) ==0, -1, round(var24_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add24_tot)  
    
    to_add25_tot <- data.frame( cbind("fixed.cost[ess.cost]", cbind(ifelse(length(var25_tot) ==0, -1, round(var25_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add25_tot)  
    
    to_add26_tot <- data.frame( cbind("fixed.cost[avoid.main.cost]", cbind(ifelse(length(var26_tot) ==0, -1, round(var26_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add26_tot)  
    
    to_add27_tot <- data.frame( cbind("fixed.cost[unavoid.main.cost]", cbind(ifelse(length(var27_tot) ==0, -1, round(var27_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add27_tot) 
	
	    to_add27_tot_bis <- data.frame( cbind("new.equipment.cost", cbind(ifelse(length(var27_tot_bis) ==0, -1, round(var27_tot_bis,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add27_tot_bis)   
    
    to_add28_tot <- data.frame( cbind("labour.cost", cbind(ifelse(length(var28_tot) ==0, -1, round(var28_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add28_tot)  
    
    to_add29_tot <- data.frame( cbind("capital.cost[tot.cap.cost]", cbind(ifelse(length(var29_tot) ==0, -1, round(var29_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add29_tot)  
    
    to_add30_tot <- data.frame( cbind("capital.cost[depreciation]", cbind(ifelse(length(var30_tot) ==0, -1, round(var30_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add30_tot)  
    
    to_add31_tot <- data.frame( cbind("capital.cost[interest]", cbind(ifelse(length(var31_tot) ==0, -1, round(var31_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add31_tot)  
    
    to_add32_tot <- data.frame( cbind("total.cost", cbind(ifelse(length(var32_tot) ==0, -1, round(var32_tot,numb_digits)), "€") ), stringsAsFactors=F)  
 temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add32_tot)   
    
    to_add33_tot <- data.frame( cbind("gross.value.added", cbind(ifelse(length(var33_tot) ==0, -1, round(var33_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add33_tot)  
    
    to_add34_tot <- data.frame( cbind("gross.cash.flow", cbind(ifelse(length(var34_tot) ==0, -1, round(var34_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add34_tot) 
    
    to_add35_tot <- data.frame( cbind("profit", cbind(ifelse(length(var35_tot) ==0, -1, round(var35_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add35_tot) 
    
    to_add36_tot <- data.frame( cbind("investment[number]", cbind(ifelse(length(var36_tot) ==0, -1, round(var36_tot,numb_digits)), "number") ) , stringsAsFactors=F) 
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add36_tot)  
    
    to_add37_tot <- data.frame( cbind("investment[value]", cbind(ifelse(length(var37_tot) ==0, -1, round(var37_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add37_tot) 
    
    to_add38_tot <- data.frame( cbind("capital.value", cbind(ifelse(length(var38_tot) ==0, -1, round(var38_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
   temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add38_tot) 
    
    to_add39_tot <- data.frame( cbind("taxes", cbind(ifelse(length(var39_tot) ==0, -1, round(var39_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add39_tot)  
    
    to_add40_tot <- data.frame( cbind("net.profit", cbind(ifelse(length(var40_tot) ==0, -1, round(var40_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add40_tot)  
    
    to_add41_tot <- data.frame( cbind("employment", cbind(ifelse(length(var41_tot) ==0, -1, round(var41_tot,numb_digits)), "number") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add41_tot)  
    
# to_add46_tot <- data.frame( cbind("Economic.indicators[NPV.discounted]", cbind(ifelse(length(var46_tot) ==0, -1, round(var46_tot,numb_digits)), "€") ) )
#  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add46_tot) 
    
    to_add55_tot <- data.frame( cbind("Min.national.wage", cbind(ifelse(length(var55_tot) ==0, -1, round(var55_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
  temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add55_tot)
  

    var42_tot <- as.numeric(as.character(var28_tot))/as.numeric(as.character(var41_tot))
    to_add42_tot <- data.frame( cbind("average.wage", cbind(ifelse(length(var42_tot) ==0, -1, round(var42_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
    temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add42_tot) 
    
  #  var23_tot = MC
   # var22_tot = total FC
   #  var29_tot = total CC
   # var28_tot = LC
   # var17_tot = total VC
   # var14_tot = revenues
   
                                                                                                                                                                                                                                      # landing + discard 
var44_tot <- sum(as.numeric(as.character(var22_tot)),as.numeric(as.character(var29_tot)), na.rm=T)/ (1-(  sum(as.numeric(as.character(var28_tot)), as.numeric(as.character(var17_tot)) ,  as.numeric(as.character(var23_tot)) , na.rm=T) /sum(as.numeric(as.character(var14_tot)) ,as.numeric(as.character(var14_tot_discard )), na.rm=T) ))
       
    to_add44_tot <- data.frame( cbind("Economic.indicators[break.even.revenue]", cbind(ifelse(length(var44_tot) ==0, -1, round(var44_tot,numb_digits)), "€") ), stringsAsFactors=F) 
        temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add44_tot) 
        


R_BER_all  <-  var14_tot_land_disc / var44_tot

        to_add_R_BER_all <- data.frame( cbind("Economic.indicators[CR.BER]", cbind(ifelse(length(R_BER_all) ==0, -1, round(R_BER_all,numb_digits)), "") ), stringsAsFactors=F) 
        temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add_R_BER_all) 
    
    
 ROFTA_with_NP_all <-  var40_tot/var38_tot
 
  to_add_ROFTA_with_NP_all <- data.frame( cbind("Economic.indicators[NP.cap.value]", cbind(ifelse(length(ROFTA_with_NP_all) ==0, -1, round(ROFTA_with_NP_all,numb_digits)), "") ), stringsAsFactors=F) 
        temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add_ROFTA_with_NP_all) 
    
    
                                                            
    #                                            # (  tot_fixed_cost + total_cap_cost) / (1- ((tot_var_cost)/revenues) )
#    sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest)), na.rm=T) /  (1 -  ( sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$maint.cost)) , na.rm=T)/  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues))  ) ) 
#        
        var45_tot <- as.numeric(as.character(var35_tot))/as.numeric(as.character(var38_tot))
    to_add45_tot <- data.frame( cbind("Economic.indicators[ROI]", cbind(ifelse(length(var45_tot) ==0, -1, round(var45_tot,numb_digits)), "") ) , stringsAsFactors=F)  
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add45_tot) 
          
     var47_tot <- as.numeric(as.character(var13_tot))/as.numeric(as.character(var9_tot))   
    to_add47_tot <- data.frame( cbind("Economic.indicators[tot.landings.day]", cbind(ifelse(length(var47_tot) ==0, -1, round(var47_tot,numb_digits)), "kg") ) , stringsAsFactors=F) 
            temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add47_tot) 
            
    var48_tot <- as.numeric(as.character(var13_tot))/as.numeric(as.character(var1_tot))    
    to_add48_tot <- data.frame( cbind("Economic.indicators[tot.landings.vessel]", cbind(ifelse(length(var48_tot) ==0, -1, round(var48_tot,numb_digits)), "kg") ) , stringsAsFactors=F)  
           temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add48_tot) 
           
        var49_tot <- as.numeric(as.character(var14_tot))/as.numeric(as.character(var9_tot))
    to_add49_tot <- data.frame( cbind("Economic.indicators[tot.revenues.day]", cbind(ifelse(length(var49_tot) ==0, -1, round(var49_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
           temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add49_tot) 
           
        var50_tot <- as.numeric(as.character(var14_tot))/as.numeric(as.character(var1_tot))    
    to_add50_tot <- data.frame( cbind("Economic.indicators[tot.revenues.vessel]", cbind(ifelse(length(var50_tot) ==0, -1, round(var50_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add50_tot) 
          
      var51_tot <- as.numeric(as.character(var33_tot))/as.numeric(as.character(var1_tot))  
    to_add51_tot <- data.frame( cbind("Economic.indicators[GVA.vessel]", cbind(ifelse(length(var51_tot) ==0, -1, round(var51_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add51_tot) 
          
      var52_tot <- as.numeric(as.character(var34_tot))/as.numeric(as.character(var1_tot))  
    to_add52_tot <- data.frame( cbind("Economic.indicators[GCF.vessel]", cbind(ifelse(length(var52_tot) ==0, -1, round(var52_tot,numb_digits)), "€") ) , stringsAsFactors=F) 
         temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add52_tot) 
         
        var53_tot <- as.numeric(as.character(var35_tot))/as.numeric(as.character(var1_tot))
    to_add53_tot <- data.frame( cbind("Economic.indicators[profit.vessel]", cbind(ifelse(length(var53_tot) ==0, -1, round(var53_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
             temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add53_tot) 
             
        var54_tot <- as.numeric(as.character(var40_tot))/as.numeric(as.character(var1_tot))
    to_add54_tot <- data.frame( cbind("Economic.indicators[net.profit.vessel]", cbind(ifelse(length(var54_tot) ==0, -1, round(var54_tot,numb_digits)), "€") ) , stringsAsFactors=F)  
          temp_indicators_fleet_overall <- rbind(temp_indicators_fleet_overall, to_add54_tot) 
  
   
    nro_ov <- nrow(temp_indicators_fleet_overall)
temp_indicators_fleet_overall <- data.frame(cbind(rep(casestudy_name, nro_ov) , cbind( rep(harvest_rule_code, nro_ov), cbind( paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind( "ALL", cbind( rep("ALL" , nro_ov), cbind( rep(year_name_in_row, nro_ov) , temp_indicators_fleet_overall )))))), stringsAsFactors=F) 

#   , "ALL" (species)

colnames(temp_indicators_fleet_overall) <- head_table

table_output <- data.frame(rbind(table_output, temp_indicators_fleet_overall ))


 # ------------------------------------------------------------------------------
}   #  end loop years

for (yy in 1:length(all_the_years)) {

year_name_in_row <-  ifelse(yy<=simperiod, years[yy], years.forecast[yy-simperiod])

  for (m_int in 1:length(BMT_SPECIES) ) {         

    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
                  
          varsp1 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing.weight[m_int] ))
          varsp2 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing.number[m_int]))
          varsp5 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@discard.weight[m_int] ))
          varsp6 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@discard.number[m_int]))
          varsp3 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@price[m_int]    ))
         varsp4 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@revenues.landing[m_int]  ))
          
           varsp7 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@price.landed_discard[m_int]    ))
                    varsp8 <- as.numeric(as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@revenues.landed_discard[m_int]    ))
    
        landObl_sp <- as.character(Fleetyear[[yy]]@fleetsegments[[n_int]]@landing_obligation[m_int]    )
        
       to_addspLandObl <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("landing.obligation", cbind(landObl_sp, "") ) ) ) ) ) ) ) , stringsAsFactors=F) 
                  
          to_addsp1 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("landing.weight", cbind(varsp1, "kg") ) ) ) ) ) ) ) , stringsAsFactors=F) 
#          to_addsp2 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("landing.number", cbind(varsp2, "number") ) ) ) ) ) ) ) , stringsAsFactors=F) 
          to_addsp5 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("discard.weight", cbind(varsp5, "kg") ) ) ) ) ) ) ) , stringsAsFactors=F) 
#          to_addsp6 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("discard.number", cbind(varsp6, "number") ) ) ) ) ) ) ) , stringsAsFactors=F) 
          to_addsp3 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("price", cbind(varsp3, "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
        to_addsp4 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("revenues.landing", cbind(varsp4, "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
          
          to_addsp7 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("price.discard", cbind(varsp7, "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
           to_addsp8 <- data.frame(cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind(BMT_SPECIES[m_int], cbind(year_name_in_row, cbind("revenues.discard", cbind(varsp8, "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
                              
          colnames(to_addsp1) <- head_table
         # colnames(to_addsp2) <- head_table
          colnames(to_addsp3) <- head_table
          colnames(to_addsp4) <- head_table
          colnames(to_addsp5) <- head_table
     #     colnames(to_addsp6) <- head_table
            colnames(to_addsp7) <- head_table
           colnames(to_addsp8) <- head_table
              colnames(to_addspLandObl) <- head_table
                                                                                      # rbind(to_addsp2, 	rbind(to_addsp6,
          table_output <- data.frame(rbind(table_output, rbind(to_addspLandObl, rbind(to_addsp1,		rbind(to_addsp3,	rbind(	to_addsp4, rbind(to_addsp5,	rbind(to_addsp7,	to_addsp8)))) ))),  stringsAsFactors=F) 
        #            table_output <- data.frame(rbind(table_output, rbind(to_addsp1,	rbind(to_addsp2,	rbind(to_addsp3,	rbind(to_addsp5,	rbind(to_addsp6,to_addsp7)) )))),  stringsAsFactors=F) 
     } #  end loop fleet segments
  } # end loop species 
}   #  end loop years


if (phase =="FORECAST" ) {

  for (n_int in 1:length(BMT_FLEETSEGMENTS)) {

 numb_digits = 3
 
 var46_a <- as.numeric(as.character(Fleetyear[[foreperiod]]@fleetsegments[[n_int]]@EC.NPV15         ))   
to_add46_a <- data.frame( cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind("ALL", cbind("ALL", cbind("Economic.indicators[NPV15]", cbind(ifelse(length(var46_a) ==0, -1, round(var46_a,numb_digits)), "€") ) ) ) ) ) ) ) , stringsAsFactors=F)  
colnames(to_add46_a) <- head_table
 table_output <- data.frame( rbind(table_output,  to_add46_a) , stringsAsFactors=F) 

 var46_b <- as.numeric(as.character(Fleetyear[[foreperiod]]@fleetsegments[[n_int]]@EC.NPV15.infinite         ))   
to_add46_b <- data.frame( cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind(BMT_FLEETSEGMENTS[n_int], cbind("ALL", cbind("ALL", cbind("Economic.indicators[NPV15.infinite]", cbind(ifelse(length(var46_b) ==0, -1, round(var46_b,numb_digits)), "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
colnames(to_add46_b) <- head_table
 table_output <- data.frame(rbind(table_output,  to_add46_b), stringsAsFactors=F) 

}

 var46_a_tot <- sum(as.numeric(as.character(table_output$Value[table_output$Variable == "Economic.indicators[NPV15]" ])), na.rm=T) 
to_add46_a_tot <- data.frame( cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind("ALL", cbind("ALL", cbind("ALL", cbind("Economic.indicators[NPV15]", cbind(ifelse(length(var46_a_tot) ==0, -1, round(var46_a_tot,numb_digits)), "€") ) ) ) ) ) ) ), stringsAsFactors=F) 
colnames(to_add46_a_tot) <- head_table
 table_output <- data.frame( rbind(table_output,  to_add46_a_tot) , stringsAsFactors=F) 

  var46_b_tot <- sum(as.numeric(as.character(table_output$Value[table_output$Variable == "Economic.indicators[NPV15.infinite]" ])), na.rm=T) 
to_add46_b_tot <- data.frame( cbind(casestudy_name, cbind( harvest_rule_code, cbind(paste(harvest_rule_code, "-", harvest_rule_level, sep=""), cbind("ALL", cbind("ALL", cbind("ALL", cbind("Economic.indicators[NPV15.infinite]", cbind(ifelse(length(var46_b_tot) ==0, -1, round(var46_b_tot,numb_digits)), "€") ) ) ) ) ) ) ) , stringsAsFactors=F) 
colnames(to_add46_b_tot) <- head_table
 table_output <- data.frame( rbind(table_output,  to_add46_b_tot) , stringsAsFactors=F) 

 }

table_output <- data.frame(cbind(table_output, rep("", nrow(table_output))) , stringsAsFactors=F) 
colnames(table_output)[ncol(table_output)] <- "Comments"
  
# questa è l'istruzione per scrivere il file csv. Il percorso verrà creato a partire dalle variabili nel file di configurazione
# write.table(table_output, file=paste(casestudy_path, "\\", casestudy_name, "_HR", harvest_rule_code, harvest_rule_level," - economic output.csv", sep=""), sep=";", row.names=F)
  print(round(as.numeric(( proc.time() - time_star_eco) ), 2), quote=F )
return(table_output)
}
