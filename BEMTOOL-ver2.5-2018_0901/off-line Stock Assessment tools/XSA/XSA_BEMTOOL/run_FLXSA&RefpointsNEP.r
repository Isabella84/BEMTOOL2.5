# BEMTOOL - Bio-Economic Model TOOLs
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2013
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.
# 
# 1. Set the working directory

# 2. Set variables to run FLXSA BEMTOOL code:

name_ind = "NEP18.IND"        # name of the text file containing the list of all the text files for running XSA
name_tun = "NEP18TUN.dat"     # name of the text file containing survey indices for tuning
catch_fleet = "catture_per_eta_NEP.csv"  # name of the file with catch by year, age and fleet segment to disaggregate the total F

year_start=2007
year_end = 2014
nb_fleets= 3         
minfbar <- 1
maxfbar <- 6
plusgroup <- 7

# 3. XSA settings
min.nse=0.3
fse=1
rage=1 
qage=4
shk.n=TRUE
shk.f=TRUE
shk.yrs=5
shk.ages=5
tsrange=20
tspower=3 
vpa=FALSE
#----

# 4. Reference points calculation
RP_calc = "Y" #"N"
model = "geomean"  #bevholt, ricker,  shepherd, segreg, geomean 

#-------------------- PLEASE, DON'T TOUCH THE CODE BELOW!


#pkg_dir <- paste(getwd(),"/packages/",sep="")
#pkg_list <- list("FLCore_2.5.0","FLAssess_2.5.0","FLXSA_2.5", "FLash_2.5.0", "FLBRP_2.5.0", "ggplotFL_0.1", "FLAdvice_1.0")
#lapply(pkg_list, function(x) install.packages(paste(pkg_dir,x,".zip",sep="")))

source(paste(getwd(),"/scripts/load_libraries.r",sep=""))
source(paste(getwd(),"/scripts/FLXSA_BEMTOOL.r",sep=""))

XSA_Results <-XSA(year_start,year_end,name_ind,name_tun,catch_fleet,nb_fleets)

#source(paste(getwd(),"/scripts/saveobject.r",sep=""))

#saveobject(XSA_Results,file=(tempFile <- paste(getwd(), "/Results/XSA_Results.dat", sep="")) )

dput(XSA_Results, paste(getwd(), "/Results/XSA_Results.dat", sep=""))

if (RP_calc=="Y"){
source(paste(getwd(),"/scripts/Ref_points_BEMTOOL.r",sep=""))
RP(model,XSA_Results)

}

