# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# runEcon.r - Bemtool economic indicators in the simulation period
# Author: Paolo Accadia

# Reading options for functions from bmtcfg.csv and initialize the parameters matrices

n_fleet <- length(BMT_FLEETSEGMENTS)
m_stock <- length(BMT_SPECIES)

option <<- data.frame(matrix(0, nrow=1, ncol=8))    #Options selected by the model user on economic component
option_discard_price <<- c()    #Options selected by the model user on price of discard of economic component

mdmat1 <<- data.frame(matrix(0, nrow=n_fleet, ncol=12))  #Input of monthly data for vessels number and days at sea
mdmat2 <<- data.frame(matrix(0, nrow=n_fleet, ncol=12))

pmat1 <<- data.frame(matrix(0, nrow=m_stock, ncol=n_fleet))  #Input parameters for price simulation
pmat2 <<- data.frame(matrix(0, nrow=m_stock, ncol=n_fleet))
pmat3 <<- data.frame(matrix(0, nrow=m_stock, ncol=n_fleet))
pmat5 <<- data.frame(matrix(0, nrow=m_stock, ncol=n_fleet))
vcmat <<- data.frame(matrix(0, nrow=length(varcost_cfg_names), ncol=n_fleet))   #Input parameters for variable costs simulation
lcmat <<- data.frame(matrix(0, nrow=length(labcost_cfg_names), ncol=n_fleet))   #Input parameters for labour costs simulation  # added one row for sorting coefficient
fcmat <<- data.frame(matrix(0, nrow=length(fixcost_cfg_names), ncol=n_fleet))   #Input parameters for fixed costs simulation
ccmat <<- data.frame(matrix(0, nrow=length(capcost_cfg_names), ncol=n_fleet))   #Input parameters for capital costs simulation
fdmat <<- data.frame(matrix(0, nrow=length(fleedyn_cfg_names), ncol=n_fleet))   #Input parameters for fleet dynamics simulation
admat <<- data.frame(matrix(0, nrow=length(fleeact_cfg_names), ncol=n_fleet))   #Input parameters for activity dynamics simulation
tpmat <<- data.frame(matrix(0, nrow=length(techprog_cfg_names), ncol=n_fleet))    #Input parameters for technological progress simulation
eimat <<- data.frame(matrix(0, nrow=length(indicator_cfg_names), ncol=n_fleet))    #Input parameters for Economic indicators simulation

# ------------------------- time series supposed in the forecast  
pmat4 <<- data.frame(matrix(0, nrow=m_stock, ncol=foreperiod))    # price
vcvec <<- data.frame(matrix(0, nrow=1, ncol=foreperiod))          # fuel costs
tsmat1 <<- data.frame(matrix(0, nrow=n_fleet, ncol=foreperiod))   # taxes
tsmat2 <<- data.frame(matrix(0, nrow=n_fleet, ncol=foreperiod))   # subsidies
tsmat3 <<- data.frame(matrix(0, nrow=n_fleet, ncol=foreperiod))   # new equipment costs

for (n in 1:n_fleet) {

    for (nro in 1:length(varcost_cfg_names)) {
    vcmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.varCostFun.F", n,sep=""),nro])
    }
#    vcmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.varCostFun.F", n,sep=""),2])
#    vcmat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.varCostFun.F", n,sep=""),3])
#    vcmat[[4,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.varCostFun.F", n,sep=""),4])

    for (nro in 1:length(labcost_cfg_names)) {  
    lcmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),nro])
      }
#    lcmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),2])
#    lcmat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),3])
#    lcmat[[4,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),4])
#    lcmat[[5,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),5])
#    lcmat[[6,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.labCostFun.F", n,sep=""),6])

    for (nro in 1:length(fixcost_cfg_names)) {    
    fcmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fixCostFun.F", n,sep=""),nro])
      }
#    fcmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fixCostFun.F", n,sep=""),2])
#    fcmat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fixCostFun.F", n,sep=""),3])
  
      for (nro in 1:length(capcost_cfg_names)) {  
    ccmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.capCostFun.F", n,sep=""),nro])
      }
#    ccmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.capCostFun.F", n,sep=""),2])

    for (nro in 1:length(fleedyn_cfg_names)) {
    fdmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fleDynFun.F", n,sep=""),nro])
      }
#    fdmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fleDynFun.F", n,sep=""),2])
#    fdmat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.fleDynFun.F", n,sep=""),3])

    for (nro in 1:length(fleeact_cfg_names)) {
    admat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.actDynFun.F", n,sep=""),nro])
      }
#    admat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.actDynFun.F", n,sep=""),2])
#    admat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.actDynFun.F", n,sep=""),3])

    for (nro in 1:length(techprog_cfg_names)) {
    tpmat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.tecProgFun.F", n,sep=""),nro])
      }
#    tpmat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.tecProgFun.F", n,sep=""),2])
#    tpmat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.tecProgFun.F", n,sep=""),3])
  
      for (nro in 1:length(indicator_cfg_names)) {  
    eimat[[nro,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),nro])
      }
#    eimat[[2,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),2])
#    eimat[[3,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),3])
#    eimat[[4,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),4])
#    eimat[[5,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),5])
#    eimat[[6,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),6])
#    eimat[[7,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),7])
#    eimat[[8,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.econIndFun.F", n,sep=""),8])
    
    for (m in 1:m_stock) {
        pmat1[[m,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.priceFun.F", n, ".S", m,sep=""),1])
        pmat2[[m,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.priceFun.F", n, ".S", m,sep=""),2])
        pmat3[[m,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.priceFun.F", n, ".S", m,sep=""),3])
        pmat5[[m,n]] =	as.character(cfg[rownames(cfg) == paste("casestudy.priceFun.F", n, ".S", m,sep=""),4])
    }
}

colnames(option) <- c("priceFun", "varCostFun", "labourCostFun", "fixCostFun", "capCostFun", "fleDynFun", "actDynFun", "tecProgFun" )

option[[1]] = as.numeric( as.character(cfg[rownames(cfg) == "casestudy.priceFun",1]) )
option[[2]] = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.varCostFun",1])  )
option[[3]] = 1    # labour costs
option[[4]] = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.fixCostFun",1]) )
option[[5]] = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.capCostFun",1]) )
option[[6]] = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.fleDynFun",1]) )
option[[7]] = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.actDynFun",1]) )
option[[8]]  = as.numeric(as.character(cfg[rownames(cfg) == "casestudy.tecProgFun",1]) )

option_discard_price = as.numeric( as.character(cfg[rownames(cfg) == "casestudy.priceFun",2]) ) 


# import in weight
if (option[[1]]==2) {
    for (m in 1:m_stock) {
        for (ti in 1:foreperiod) {
            pmat4[[m,ti]] = as.character(cfg[rownames(cfg) == paste("casestudy.import.S", m,sep=""),ti])
        }
    }
}
# fuel price
if (option[[2]]==2 || option[[2]]==3) {
    for (ti in 1:foreperiod) {
        vcvec[[ti]] = as.character(cfg[rownames(cfg) == "casestudy.fuelprice",ti])
    }
}
# additional income and taxes
for (n in 1:n_fleet) {
    for (ti in 1:foreperiod) {
        tsmat1[[n,ti]] = as.character(cfg[rownames(cfg) == paste("casestudy.addincome.F", n,sep=""),ti])
        tsmat2[[n,ti]] = as.character(cfg[rownames(cfg) == paste("casestudy.addtaxes.F", n,sep=""),ti])
        tsmat3[[n,ti]] = as.character(cfg[rownames(cfg) == paste("casestudy.newequipment.F", n,sep=""),ti])
    }
}


#Fleetyear <<- setFleet(Fleetyear)