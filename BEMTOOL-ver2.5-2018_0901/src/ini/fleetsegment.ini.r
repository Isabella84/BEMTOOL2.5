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
####### sviluppato da MT ##############
dataframe_NA <- data.frame(matrix(NA, nrow=1, ncol=12))
colnames(dataframe_NA) <- MONTHS
rownames(dataframe_NA) <- vect_years[index_year]
# structure of landing weight data frame
landing.weight_df <- data.frame(matrix(0, nrow=1, ncol=length(BMT_SPECIES))  )
colnames(landing.weight_df) <- BMT_SPECIES
rownames(landing.weight_df) <- vect_years[index_year]
# structure of landing number data frame
landing.number_df <- data.frame(matrix(0, nrow=1, ncol=length(BMT_SPECIES))  )
colnames(landing.number_df) <- BMT_SPECIES
rownames(landing.number_df) <- vect_years[index_year]
# structure of price data frame
price_df <- data.frame(matrix(0, nrow=1, ncol=length(BMT_SPECIES))  )
colnames(price_df) <- BMT_SPECIES
rownames(price_df) <- vect_years[index_year]
# structure of revenues data frame
revenues_df <- data.frame(matrix(0, nrow=1, ncol=length(BMT_SPECIES))  )
colnames(revenues_df) <- BMT_SPECIES
rownames(revenues_df) <-vect_years[index_year]
# structure of variable cost data frame
variable.cost_df <- data.frame(matrix(0, nrow=1, ncol=4)  )
colnames(variable.cost_df) <- c("tot.var.cost", "fuel.cost", "commercial.cost", "other.var.cost")
rownames(variable.cost_df) <- vect_years[index_year]

landobl_df <- data.frame(matrix("N", nrow=1, ncol=length(BMT_SPECIES))  )
colnames(landobl_df) <- BMT_SPECIES
rownames(landobl_df) <- vect_years[index_year]

# CI
variable.cost_df_ci <- data.frame(matrix(0, nrow=5, ncol=4)  )
colnames(variable.cost_df_ci) <- c("tot.var.cost", "fuel.cost", "commercial.cost", "other.var.cost")
rownames(variable.cost_df_ci) <-  c("perc_0.05","perc_0.25","perc_0.5","perc_0.75","perc_0.95") 

# structure of fixed cost data frame
fixed.cost_df <- data.frame(matrix(0, nrow=1, ncol=7)  )
colnames(fixed.cost_df) <- c("tot.fix.cost", "maint.cost", "other.fix.cost", "ess.cost", "avoid.main.cost", "unavoid.main.cost", "new.equipment.cost")
rownames(fixed.cost_df) <-vect_years[index_year]

fixed.cost_df_ci <- data.frame(matrix(0, nrow=5, ncol=7)  )
colnames(fixed.cost_df_ci) <- c("tot.fix.cost", "maint.cost", "other.fix.cost", "ess.cost", "avoid.main.cost", "unavoid.main.cost", "new.equipment.cost")
rownames(fixed.cost_df_ci) <-c("perc_0.05","perc_0.25","perc_0.5","perc_0.75","perc_0.95") 

# structure of capital cost data frame
capital.cost_df <- data.frame(matrix(0, nrow=1, ncol=3)  )
colnames(capital.cost_df) <- c("tot.cap.cost", "depreciation", "interest")
rownames(capital.cost_df) <-vect_years[index_year]
# structure of investment data frame
investment_df <- data.frame(matrix(0, nrow=1, ncol=2)  )
colnames(investment_df) <- c("number", "value")
rownames(investment_df) <- vect_years[index_year]

investment_df_ci <- data.frame(matrix(0, nrow=5, ncol=2)  )
colnames(investment_df_ci) <-  c("number", "value")
rownames(investment_df_ci) <- c("perc_0.05","perc_0.25","perc_0.5","perc_0.75","perc_0.95")  

# structure of Economic indicators data frame
# before_13october: 
# Economic.indicators_df <- data.frame(matrix(0, nrow=1, ncol=13)  )
 Economic.indicators_df <- data.frame(matrix(NA, nrow=1, ncol=17)  )
# before_13october: 
#colnames(Economic.indicators_df) <- c("break.even.revenue", "ROI", "NPV15", "NPVinf", "tot.landings.day", "tot.landings.vessel", "tot.revenues.day", "tot.revenues.vessel", "GVA.vesel", "GCF.vessel", "profit.vessel", "net.profit.vessel", "employment.vessel")
colnames(Economic.indicators_df) <- c("break.even.revenue",  "ROI", "NPV15", "tot.landings.day", "tot.landings.vessel", "tot.revenues.day", "tot.revenues.vessel", "GVA.vessel", "GCF.vessel", "profit.vessel", "net.profit.vessel", "CR.BER", "RoFTA", "vessel.util.ratio", "NP.cap.value", "NPV.discounted", "NPV15.infinite")
rownames(Economic.indicators_df) <-vect_years[index_year]

CI_perc_df <- data.frame(matrix(0, nrow=1, ncol=5))
colnames(CI_perc_df) <-c("perc_0.05","perc_0.25","perc_0.5","perc_0.75","perc_0.95") 
rownames(CI_perc_df) <- vect_years[index_year]

CI_dimspe_df <- data.frame(matrix(0, nrow=5, ncol=length(BMT_SPECIES))  )
colnames(CI_dimspe_df) <- BMT_SPECIES
rownames(CI_dimspe_df) <- c("perc_0.05","perc_0.25","perc_0.5","perc_0.75","perc_0.95") 

new_bmtFleetsegment <- new(Class= "bmtFleetsegment",
                    fleetsegmentcode = BMT_FLEETSEGMENTS[n],
                    fishingtechnique = BMT_FLEETSEGMENTS_FTEC[n],
                    loa = BMT_FLEETSEGMENTS_LOA[n],
                    VESSELS.annual = 0,
                    VESSELS = dataframe_NA,
                    GT.annual = 0,
                    GT = dataframe_NA,
                    GT.average.annual = 0,
                    GT.average = dataframe_NA,
                    KW.annual = 0,
                    KW = dataframe_NA,
                    KW.average.annual = 0,
                    KW.average = dataframe_NA,
                    DAYS.annual = 0,
                    DAYS = dataframe_NA,
                    DAYS.average.annual = 0,
                    DAYS.average = dataframe_NA,
                    MAXDAYS.average.annual = 0,
                    GT.DAYS.annual = 0,
                    GT.DAYS = dataframe_NA,
                    KW.DAYS.annual = 0,
                    KW.DAYS = dataframe_NA,
                    fishingcoefficient = dataframe_NA,
                    landing.weight = landing.weight_df,
                    landing.weight.CI.perc = CI_dimspe_df,  # new !!
                    total.landings = 0,
                    total.landings.CI.perc = CI_perc_df, # new !!!!!
                    landing.number = landing.number_df,
                    landing.number.CI.perc = CI_dimspe_df,
                    discard.weight = landing.weight_df,
                    discard.weight.CI.perc = CI_dimspe_df,  # new !!
                    total.discards = 0,
                    total.discards.CI.perc = CI_perc_df, # new !!!!!
                    discard.number = landing.number_df,
                    discard.number.CI.perc = CI_dimspe_df,
                    landing_obligation = landobl_df,
                    price = price_df,
		price.CI.perc	= CI_dimspe_df,       # new !!!
		                    price.landed_discard = price_df,
		price.landed_discard.CI.perc	= CI_dimspe_df,       # new !!!
                    revenues = revenues_df,
		                revenues.CI.perc = CI_dimspe_df,    # new !!!
		                revenues.landing = revenues_df,
                    revenues.landing.CI.perc = CI_dimspe_df,    # new !!!
		                revenues.landed_discard = revenues_df,
                    revenues.landed_discard.CI.perc = CI_dimspe_df,    # new !!!
                    total.revenues = 0,
                    total.revenues.CI.perc = CI_perc_df,   # new !!!
                    total.revenues.landings = 0,
                    total.revenues.landings.CI.perc = CI_perc_df,   # new !!!
                    total.revenues.landed_discard = 0,
                    total.revenues.landed_discard.CI.perc = CI_perc_df,   # new !!!
                    other.income = 0,
                    total.income = 0,
                    variable.cost = variable.cost_df,
                    variable.cost.CI.perc = variable.cost_df_ci, # new !!!!!
                    fuel.price = 0,
                    fixed.cost = fixed.cost_df,
       fixed.cost.CI.perc =  fixed.cost_df_ci, # new !!!!
                    labour.cost = 0,
                    labour.cost.CI.perc = CI_perc_df,    # # new !!!!! # solo in caso di sorting coefficient = discard ratio
                    capital.cost = capital.cost_df,
                    total.cost = 0,
       total.cost.CI.perc = CI_perc_df, # new !!!!!
                    gross.value.added = 0,
                    gross.cash.flow = 0,
            gross.value.added.CI.perc = CI_perc_df,	  # new !!!
            gross.cash.flow.CI.perc	= CI_perc_df,   # new !!!
                    profit = 0,
            profit.CI.perc = CI_perc_df,	 # new !!!
                    investment = investment_df,
            investment.CI.perc = investment_df_ci,	  # new !!!
                    capital.value = 0,
            capital.value.CI.perc = CI_perc_df,    # new !!!
                    taxes = 0,
                    net.profit = 0,
            net.profit.CI.perc = CI_perc_df,	  # new !!!
                    employment = 0,
                    average.wage = 0,
            employment.CI.perc = CI_perc_df,	# new !!!           # dipendendo dal larour cost, nel caso di sorting coefficient = discard ratio
            average.wage.CI.perc = CI_perc_df,  # new !!!
                    technology = 0,
                    new.equipment.costs = 0,
#                    Economic.indicators = Economic.indicators_df,
										EC.BER = 0,
            EC.BER.CI.perc = CI_perc_df,
EC.R_BER = 0,
            EC.R_BER.CI.perc = CI_perc_df,
EC.ROI = 0,
            EC.ROI.CI.perc = CI_perc_df,
EC.NPV15 = 0,
            EC.NPV15.CI.perc = CI_perc_df ,
EC.tot.landings.day = 0,
            EC.tot.landings.day.CI.perc = CI_perc_df ,
EC.tot.landings.vessel = 0,
            EC.tot.landings.vessel.CI.perc = CI_perc_df ,
EC.tot.revenues.day = 0,
            EC.tot.revenues.day.CI.perc = CI_perc_df ,
EC.tot.revenues.vessel = 0,
            EC.tot.revenues.vessel.CI.perc = CI_perc_df ,
EC.GVA.vessel = 0,
            EC.GVA.vessel.CI.perc = CI_perc_df ,
EC.GCF.vessel = 0,
            EC.GCF.vessel.CI.perc = CI_perc_df ,
EC.profit.vessel = 0,
            EC.profit.vessel.CI.perc = CI_perc_df ,
EC.net.profit.vessel = 0,
             EC.net.profit.vessel.CI.perc = CI_perc_df,
EC.RoFTA = 0,
            EC.RoFTA.CI.perc = CI_perc_df ,
EC.vessel.util.ratio = 0,
            EC.vessel.util.ratio.CI.perc = CI_perc_df ,
EC.NP.cap.value = 0,
            EC.NP.cap.value.CI.perc = CI_perc_df,
EC.NPV.discounted = 0,
            EC.NPV.discounted.CI.perc = CI_perc_df,
EC.NPV15.infinite = 0,
            EC.NPV15.infinite.CI.perc = CI_perc_df)
                    
# print(paste("fleet segment:", BMT_FLEETSEGMENTS[n], "in", vect_years[index_year], "successfully created!"), quote=FALSE)
