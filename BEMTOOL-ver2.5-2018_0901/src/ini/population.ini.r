# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ------------------------------------------------------------------------------
# INSTANCES OF OBJECTS
# ------------------------------------------------------------------------------

## vector of months
#MONTHS <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
## current year in the model (eg. 2012)
#years= c(1999, 2000, 2001, 2002)
#current_year <- years[1]


# ------------------------------------------------------------------------------
# Example of object of type Population with name POPULATION_1
# ------------------------------------------------------------------------------
# life span of POPULATION_1 inserted by the user in the graphical interface

ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m, ".AladymSimulation", sep=""),1])

# print(paste("ALADYM simulation:",ALADYM_flag), quote=FALSE)
#if (ALADYM_flag) {
cfg.n_ages.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),1]))
cfg.n_ages.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),2]))
#} else {                                                                                               
#cfg.n_ages.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".StockAssessmentTool", sep=""),5]))
#cfg.n_ages.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".StockAssessmentTool", sep=""),6]))
#}

# print(paste("life span:", cfg.n_ages.F) , quote=FALSE)

cfg.sexratio <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),3]))
cfg.a.F <- format(as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),4])) , scientific=F) 
cfg.b.F <- format( as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),5]))  , scientific=F)
cfg.a.M <-  format( as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),6]))  , scientific=F)
cfg.b.M <-  format( as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),7]))  , scientific=F)

cfg.t0.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),8]))
cfg.k.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),9]))
cfg.linf.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),10]))
cfg.t0.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),11]))
cfg.k.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),12]))
cfg.linf.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),13]))

cfg.l50.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),14]))
cfg.matrange.F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),15]))
cfg.l50.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),16]))
cfg.matrange.M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m, ".params", sep=""),17]))

lifespan_M <- cfg.n_ages.M 
lifespan_F <- cfg.n_ages.F 
# structure of maturity data frame to be inserted into POPULATION object:
# - in case of Aladym simulation it will have months in the columns
# - in case of skip of simulation it will have years in the columns
matvect <- data.frame(matrix(NA, nrow=2, ncol=(max(lifespan_M, lifespan_F))))
rownames(matvect) <- c("M", "F")
colnames(matvect) <- c(paste("age", c(0:(max(lifespan_M, lifespan_F)-1)), sep=""))

mortalityvect <- data.frame(matrix(NA, nrow=(max(lifespan_M, lifespan_F)), ncol=length(MONTHS)))
rownames(mortalityvect) <- c(paste("age", c(0:(max(lifespan_M, lifespan_F)-1)), sep=""))
colnames(mortalityvect) <- MONTHS

# structure of mortality data frame to be inserted into POPULATION object:
# - in case of Aladym simulation it will have months in the columns
# - in case of skip of simulation it will have years in the columns
#mortalityvect <- data.frame(matrix(0, nrow=2, ncol=max(lifespan_M, lifespan_F)+1) )
#rownames(mortalityvect) <- c("M", "F")
#colnames(mortalityvect) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
# structure of offspring monthly proportions vector
offspringprop <- data.frame(matrix(1/12, nrow=1, ncol=12))
colnames(offspringprop) <- MONTHS
# structure of lifespan data frame
lifespan_df <- data.frame(rbind(list(rowname="M", lifespan = lifespan_M), list(rowname= "F", lifespan = lifespan_F)), row.names=1)
# structure of growth data frame
growth_df <- data.frame(rbind(list(rowname="M", t0=cfg.t0.M , k=cfg.k.M , linf=cfg.linf.M ), list(rowname= "F", t0=cfg.t0.F , k=cfg.k.F , linf=cfg.linf.F )), row.names=1)
# structure of lengthweight_df data frame
lengthweight_df <- data.frame(rbind(list(rowname="M", a=cfg.a.M, b=cfg.b.M), list(rowname= "F", a=cfg.a.F, b=cfg.b.F )), row.names=1)
# structure of maturity parameters data frame
maturity_params_df <- data.frame(rbind(list(rowname="M", L50=cfg.l50.M , matrange=cfg.matrange.M ), list(rowname= "F",  L50=cfg.l50.F , matrange=cfg.matrange.F )), row.names=1) 
# structure of mortality constant data frame
mortality_constant_df <- data.frame(rbind(list(rowname="M", M=NA), list(rowname= "F",  M=NA)), row.names=1)

cfg.meditscode <-	as.character(cfg[rownames(cfg) == paste("casestudy.S", m, sep=""),2])

# Code for creation of POPULATION_1
new_bmtPopulation <- new(Class= "bmtPopulation",
                  GSA = BMT_GSA,
                  species = BMT_SPECIES[m],
                  meditscode = cfg.meditscode,
                  lifespan = lifespan_df,
                  growth = growth_df,
                  lengthweight = lengthweight_df,
                  maturity.params = maturity_params_df,
                  maturity.vect = matvect, 
                  M.cost = mortality_constant_df,
                  M.vect = list(M=mortalityvect, F=mortalityvect),
                  offspring.prop = offspringprop,
                  sexratio = cfg.sexratio
                  )
                  
print(paste("population:", BMT_SPECIES[m], "successfully created!"), quote=FALSE)
