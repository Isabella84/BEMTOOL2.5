# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ------------------------------------------------------------------------------
# Definition of object types used in BEMTOOL code (S4 classes and methods)
# - Fleet segment


# definition of object: bmtFleetsegment
setClass(Class="bmtFleetsegment",
        representation=representation(
            fleetsegmentcode = "character",
            fishingtechnique = "character",
            loa = "character",
            VESSELS.annual = "numeric",
            VESSELS = "data.frame",
            GT.annual = "numeric",
            GT = "data.frame",
            GT.average.annual = "numeric",
            GT.average = "data.frame",
            KW.annual = "numeric",
            KW = "data.frame",
            KW.average.annual = "numeric",
            KW.average = "data.frame",
            DAYS.annual = "numeric",
            DAYS = "data.frame",
            DAYS.average.annual = "numeric",
            DAYS.average = "data.frame",
	        	MAXDAYS.average.annual = "numeric",
            GT.DAYS.annual = "numeric",
            GT.DAYS = "data.frame",
            KW.DAYS.annual = "numeric",
            KW.DAYS = "data.frame",
            fishingcoefficient = "data.frame", 
            total.landings = "numeric",  
            total.landings.CI.perc = "data.frame", # new !!!!!
            landing.weight = "data.frame",
            landing.weight.CI.perc = "data.frame",  # new !!
            landing.number = "data.frame",
            landing.number.CI.perc = "data.frame",
            total.discards = "numeric",  
            total.discards.CI.perc = "data.frame", # new! 
            discard.weight = "data.frame",
            discard.weight.CI.perc = "data.frame",   # new !!!
            discard.number = "data.frame",
            discard.number.CI.perc = "data.frame",
            landing_obligation = "data.frame",
            price	= "data.frame",
            price.CI.perc	= "data.frame",       # new !!!
            price.landed_discard	= "data.frame",
            price.landed_discard.CI.perc = "data.frame",       # new !!!
            revenues = "data.frame",
            revenues.CI.perc = "data.frame",    # new !!!
            revenues.landing = "data.frame",
            revenues.landing.CI.perc = "data.frame",    # new !!!
            revenues.landed_discard = "data.frame",
            revenues.landed_discard.CI.perc = "data.frame",    # new !!!
            total.revenues = "numeric",
            total.revenues.CI.perc = "data.frame",   # new !!!
                        total.revenues.landings = "numeric",
            total.revenues.landings.CI.perc = "data.frame",   # new !!!
            total.revenues.landed_discard = "numeric",
            total.revenues.landed_discard.CI.perc = "data.frame",   # new !!!
other.income = "numeric",	
total.income = "numeric",	
            variable.cost = "data.frame",	
            variable.cost.CI.perc = "data.frame",	  # new !!! # solo i commercial costs dipendono dal landing
fuel.price = "numeric",	
            fixed.cost = "data.frame",	
            fixed.cost.CI.perc = "data.frame",      # new !!!!
            labour.cost = "numeric",	
            labour.cost.CI.perc = "data.frame",  # new !!!  # solo in caso di sorting coefficient = discard ratio
            capital.cost = "data.frame",	
            total.cost = "numeric",
						total.cost.CI.perc = "data.frame",	
            gross.value.added = "numeric",
            gross.value.added.CI.perc = "data.frame",	  # new !!!	
            gross.cash.flow	= "numeric",
            gross.cash.flow.CI.perc	= "data.frame",   # new !!!
            profit = "numeric",	
            profit.CI.perc = "data.frame",	 # new !!!
            investment = "data.frame",	
            investment.CI.perc = "data.frame",	  # new !!!
            capital.value = "numeric",
						capital.value.CI.perc = "data.frame",	  # new !!
            taxes = "numeric",	
            net.profit = "numeric",	
            net.profit.CI.perc = "data.frame",	  # new !!!
            employment = "numeric",	
            employment.CI.perc = "data.frame",	# new !!!           # dipendendo dal larour cost, nel caso di sorting coefficient = discard ratio
            average.wage = "numeric",	
						average.wage.CI.perc = "data.frame",  # new !!!
            technology = "numeric",
            new.equipment.costs = "numeric",	 # per memorizzare i costo medio per battello 
#Economic.indicators = "data.frame",

						EC.BER = "numeric",
						EC.BER.CI.perc = "data.frame",
						EC.R_BER = "numeric",
						EC.R_BER.CI.perc = "data.frame",
						EC.ROI = "numeric",
						EC.ROI.CI.perc = "data.frame",
						EC.NPV15 = "numeric",
						EC.NPV15.CI.perc = "data.frame" ,
						EC.tot.landings.day = "numeric",
						EC.tot.landings.day.CI.perc = "data.frame" ,
						EC.tot.landings.vessel = "numeric",
						EC.tot.landings.vessel.CI.perc = "data.frame" ,
						EC.tot.revenues.day = "numeric",
						EC.tot.revenues.day.CI.perc = "data.frame" ,
						EC.tot.revenues.vessel = "numeric",
						EC.tot.revenues.vessel.CI.perc = "data.frame" ,
						EC.GVA.vessel = "numeric",
						EC.GVA.vessel.CI.perc = "data.frame" ,
						EC.GCF.vessel = "numeric",
						EC.GCF.vessel.CI.perc = "data.frame" ,
						EC.profit.vessel = "numeric",
						EC.profit.vessel.CI.perc = "data.frame" ,
						EC.net.profit.vessel = "numeric",
						EC.net.profit.vessel.CI.perc = "data.frame",
						EC.RoFTA = "numeric",
						EC.RoFTA.CI.perc = "data.frame" ,
						EC.vessel.util.ratio = "numeric",
						EC.vessel.util.ratio.CI.perc = "data.frame" ,
						EC.NP.cap.value = "numeric",
						EC.NP.cap.value.CI.perc = "data.frame" ,
						EC.NPV.discounted = "numeric",
						EC.NPV.discounted.CI.perc = "data.frame" ,
						EC.NPV15.infinite = "numeric",
						EC.NPV15.infinite.CI.perc = "data.frame" ) )          # sistemare tutti gli indicatori negli slot
