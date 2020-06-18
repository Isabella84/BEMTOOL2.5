# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION")) {
single_values_to_check_simulation <- list( list(string="Start year of simulation in GENERAL DATA", term=as.numeric(gtkEntryGetText(entry_EndYear_simulation)) ),
list(string="End year of simulation in GENERAL DATA", term=NULL),
list(string="Years to be pre-simulated in GENERAL DATA", term=NULL),
list(string="a [g/mm^b] (MALES) in BIOLOGICAL", term=NULL),
list(string="b (MALES) in BIOLOGICAL", term=NULL),
list(string="a [g/mm^b] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="b (FEMALES) in BIOLOGICAL", term=NULL),
list(string="Lifespan [years] (MALES) in BIOLOGICAL", term=NULL),
list(string="Lifespan [years] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="min of L50% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="max of L50% [mm] (MALES) in BIOLOGICAL", term=gtkEntryGetText(entryVBF_M_lifespan)),
list(string="1st parameter of L50% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of L50% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="min of L50% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="max of L50% [mm] (FEMALES) in BIOLOGICAL", term=gtkEntryGetText(entryVBF_F_lifespan)),
list(string="1st parameter of L50% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of L50% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="min of L75%L25% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="max of L75%L25% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of L75%L25% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of L75%L25% [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="min of L75%L25% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="max of L75%L25% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of L75%L25% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of L75%L25% [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="min of t0 [years] (MALES) in BIOLOGICAL", term=NULL),
list(string="max of t0 [years] (MALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of t0 [years] (MALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of t0 [years] (MALES) in BIOLOGICAL", term=NULL),
list(string="min of t0 [years] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="max of t0 [years] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of t0 [years] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of t0 [years] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="min of K [years^-1] (MALES) in BIOLOGICAL", term=NULL),
list(string="max of K [years^-1] (MALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of K [years^-1] (MALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of K [years^-1] (MALES) in BIOLOGICAL", term=NULL),
list(string="min of K [years^-1] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="max of K [years^-1] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of K [years^-1] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of K [years^-1] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="min of Linfinity [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="max of Linfinity [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of Linfinity [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of Linfinity [mm] (MALES) in BIOLOGICAL", term=NULL),
list(string="min of Linfinity [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="max of Linfinity [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="1st parameter of Linfinity [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="2nd parameter of Linfinity [mm] (FEMALES) in BIOLOGICAL", term=NULL),
list(string="Sex ratio F/F+M in BIOLOGICAL", term=NULL),
list(string="min of R [thousands] in RECRUITMENT", term=NULL),
list(string="max of R [thousands] in RECRUITMENT", term=NULL),
list(string="1st parameter of R [thousands] in RECRUITMENT", term=NULL),
list(string="2nd parameter of R [thousands] in RECRUITMENT", term=NULL),
list(string="Tr [months] in RECRUITMENT", term=c(gtkEntryGetText(entryVBF_M_lifespan), gtkEntryGetText(entryVBF_F_lifespan))),
list(string="parameter a of SR relationship in RECRUITMENT", term=NULL),
list(string="parameter b of SR relationship in RECRUITMENT", term=NULL),
list(string="parameter c of SR relationship in RECRUITMENT", term=NULL),
list(string="Delay for SS calculation in RECRUITMENT", term=NULL),
list(string="1st parameter of Noise in RECRUITMENT", term=NULL),
list(string="2nd parameter of Noise in RECRUITMENT", term=NULL),
#list(string="Constant value of Natural Mortality (MALES) in MORTALITY", term=NULL),
#list(string="Constant value of Natural Mortality (FEMALES) in MORTALITY", term=NULL),
list(string="Seed value of Total Mortality (MALES) in MORTALITY", term=NULL),
list(string="Seed value of Total Mortality (FEMALES) in MORTALITY", term=NULL),
list(string="min of Age range for F calculated (MALES) in MORTALITY", term=NULL),
list(string="min of Age range for F calculated (FEMALES) in MORTALITY", term=NULL),
list(string="max of Age range for F calculated (MALES) in MORTALITY", term=gtkEntryGetText(entryVBF_M_lifespan)),
list(string="max of Age range for F calculated (FEMALES) in MORTALITY", term=gtkEntryGetText(entryVBF_F_lifespan)),
list(string="min value in CALIBRATION", term=NULL),
list(string="max value in CALIBRATION", term=NULL),
list(string="min value of Noise in RECRUITMENT", term=NULL),
list(string="max value of Noise in RECRUITMENT", term=NULL))
} else {
single_values_to_check_simulation <- list()
}

single_values_to_check_fleetsegment <- list( list(string="Seed value of Production in FISHERY", term=NULL),
list(string="Seed value of Fishing effort in FISHERY", term=NULL),
list(string="Fleet name in FISHERY", term=NULL) ) 

single_values_to_check_forecast <- list( list(string="1st parameter of noise on recruitment in FORECAST", term=NULL),
list(string="2nd parameter of noise on recruitment in FORECAST", term=NULL),
#list(string="Number years for average in FORECAST", term=NULL),
list(string="Target F in FORECAST", term=NULL),
#list(string="First year in FORECAST", term=NULL),
list(string="Target year in FORECAST", term=NULL) ,
list(string="min value of Noise in RECRUITMENT for forecast", term=NULL),                                                         
list(string="max value of Noise in RECRUITMENT for forecast", term=NULL) )  