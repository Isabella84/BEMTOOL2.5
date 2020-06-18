# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# Code developped within STECF SGMED (January 2012) by Finlay Scott and modified to be incorporated in BEMTOOL framework
#............
#.......

STF<-function(cod, year_start,year_end,Ref_point,avg_y,Fsqsetting) {

#cod  =   XSAinfo[[m_spe]]$results$results
year_start
year_end
Ref_point
avg_y
Fsqsetting

#load("stock.Rdata")

# Assumptions:
# STF is 3 years
# F pattern is mean of last 3 years  or F of the last year 
# constant recruitment based on geometric mean of last 3 years
# stock and catch weights based on mean of last 3 years

stf.years <- 3
wts.nyears <- 1
fbar.nyears <- 1
#***** ENTER F01 *********

Fstatusquo <- c(fbar(cod)[,ac(year_end)]) 
reduction <<-  as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR4",2]) )  
F01 <-  Fstatusquo * (1 - reduction/100)

#F01 <- Ref_point     #2011

# projection years are from the last year we have F data for + 3 years
proj.years <- range(cod)[["maxyear"]] + (1:stf.years)

# the stf method extends the stock through time and
# makes assumptions about future weights and Fs etc. (see above)
if ((Fsqsetting=="last") ) {
n = 1
} else{
n = avg_y
}

Fmean = harvest(cod)[,as.character(year_end)] # data.frame(hke.stk@harvest[,ac(year_end)]@.Data)
Fbar = mean(Fmean[(range(cod)[6]+1):(range(cod)[7]+1),1])

if (Fsqsetting=="rescaled") {
for (i in 1:(nrow(harvest(cod)[,ac(year_end)])))  {
Fmean[i,1] = mean(harvest(cod)[i,ac((year_end-avg_y+1):(year_end))])
}
Fbar_mean = mean(Fmean[(range(cod)[6]+1):(range(cod)[7]+1),1])

#Fbar_last = mean(harvest(cod)[,ac(year_end)][(range(cod)[6]+1):(range(cod)[7]+1)])

for (i in (1:(nrow(harvest(cod)[,ac(year_end)]))))  {
harvest(cod)[i,ac(year_end)] = Fmean[i,1]* Fbar/ Fbar_mean
}
}


# So default F pattern is F status quo (Fsq)
cod.stf <- stf(cod, nyears=stf.years, wts.nyears=wts.nyears, fbar.nyears=fbar.nyears)
# You might want to rescale the harvest rates by hand to remove trends etc.





# F scenarios
# F01, Fsq * 0:2 by 0.1
# Get the Fsq for handy future reference
Fsq <- c(fbar(cod.stf)[,as.character(year_end)])    
Fscenarios <- c(F01,seq(from=0,to=2,by=0.1)*Fsq)
# Fmultipliers to be applied to Fsq
Fmults <- Fscenarios / c(Fsq)

# Going to use the 6th dimension of FLR to store the results
# from the various F scenarios
# So we extend the cod.stf object along the 6th dimension
cod.stf <- propagate(cod.stf,length(Fscenarios))

# Set up future harvest rates for each scenario
# harvest in the first projection year is Fsq
# harvest in the subsequent projection years is Fsq * multiplier
# After using the propagate() method above the first year of the harvest is
# already at Fsq
# So we only need to adjust the harvest rates in the subsequent years
# Use the iterations
harvest(cod.stf)[,ac(proj.years[-1])] <- sweep(harvest(cod.stf)[,ac(proj.years[-1])],6,Fmults,"*")
# reset units due to 'feature'
units(harvest(cod.stf)) <- "f"

# Check that fbar is correct for future years
as.data.frame(fbar(cod.stf)[,ac(proj.years)])
# The iterations store the Fscenarios
# First year of each iteration should be the same, Fsq

# We need to set up a stock-recruitment relationship (SRR)
#cod.sr <- as.FLSR(cod,model=geomean)

cod.sr <- FLSR(model=geomean)
params(cod.sr)['a',] <- prod(rec(cod)[,ac((year_end - avg_y+1):year_end)])^(1/avg_y)     



# 2. Now we fit the model (simple with a geometric mean)
#cod.sr <- sr(cod.sr)
# set the parameter in the FLSR object
#params(cod.sr)['a',] <- exp(mean(log(window(rec(cod),start=proj.years[1]-3,end=proj.years[1]-1))))

# PROJECTION
# Harvest rates already set so no target values needed in control
ctrl <- projectControl(data.frame(year=proj.years))
# All iterations (Fscenarios) done in parallel
res <- project(cod.stf, ctrl, cod.sr)
# Have a quick butchers and see if it's worked
#plot(window(res,start=2011,end=2014))

# Outputs
# Catches
# Landings
# F
# SSB (y+2)
# % Change in SSB (y+2 to y+1)
# % Catch change (y+1 to y-1)

# output data table
output <- data.frame( # F in 2011
                      "Fscenario"= c(fbar(cod.stf)[,ac(proj.years[2])]),
                      # Fsq multiplier in 2011
                      "Fmult" = Fmults,
                      # Catch and landings in 2011
                      "Catch_2013"=c(computeCatch(res)[,ac(proj.years[2])]),
                      "Landings_2013"=c(computeLandings(res)[,ac(proj.years[2])]),
                      "Catch_2014"=c(computeCatch(res)[,ac(proj.years[3])]),
                      "Landings_2014"=c(computeLandings(res)[,ac(proj.years[3])]),
                      # SSB
                      "SSB_2013"=c(ssb(res)[,ac(proj.years[2])]),
                      "SSB_2014"=c(ssb(res)[,ac(proj.years[3])]),
                      # Change in SSB in 2011 from SSB in 2010
                      "ChangeSSB_2014_2013"=100*c((ssb(res)[,ac(proj.years[3])]- ssb(res)[,ac(proj.years[2])]) / ssb(res)[,ac(proj.years[2])]),
                      # Change in catch in 2011 from catch in 2009
                      # i.e. (catch 2011 - catch 2009) / catch 2009
                      # (catch 2011 / catch 2009) - 1
                      "ChangeCatch_2013_2011"=100*c((computeCatch(res)[,ac(proj.years[2])]- computeCatch(res)[,ac(proj.years[1]-1)]) / computeCatch(res)[,ac(proj.years[1]-1)])
                      )

colnames(output)=c("Fbar","F_factor",paste("Catch_",year_end+2,sep=""),paste("Landings",year_end+2,sep=""),paste("Catch_",year_end+3,sep=""),paste("Landings",year_end+3,sep=""),paste("SSB_",year_end+2,sep=""),paste("SSB_",year_end+3,sep=""),paste("ChangeSSB_",year_end+3,"_",year_end+2,sep=""),paste("ChangeCatch",year_end+2,"_",year_end,sep=""))

# write.table(output,file=paste(STF_home_species_res, "/Short_term_forecast_table.csv", sep=""),row.names=F,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[STF_spe],"/", casestudy_name, " - Short_term_forecast_table ", harvest_rule_id,".csv", sep="")
write.table(output,file=save_path,row.names=F,sep=";")

}
