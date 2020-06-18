# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





setClass(Class="aldSimulation",
        representation=representation(
            time_slice_year = "numeric",
            start_year = "numeric",
            end_year = "numeric",
            presimulation_years = "numeric",
            seed_rundomization_runs = "numeric",
            reference_points_calculation = "logical", 
            CI_calculation = "logical",  
            CI_n_runs = "numeric",
            CI_error_type = "numeric",  #additive (1) /multiplicative (2)
            CI_error_source = "numeric",  #external file (1) /distribution (2)
            CI_error.externalvector = "data.frame", # external file  
            recruitment.noise = "data.frame", # distribution
            R.100runs = "data.frame",            # recruitment parameters  110 runs
            monthlyoffspring = "data.frame",
            Tr = "numeric", 
            stockr.relationship = "data.frame",
            #stockr.file = "character",
            stockr.vector = "data.frame",
            stockr.unit = "character",
            spawners.ss = "character",
            spawners.delayss = "numeric",
            recruitment.tuning = "logical",   
            recruitment.tuning.range = "list",
            naturalmortality.M.type = "character",
            naturalmortality.M.constant = "numeric",
            naturalmortality.M.tmax = "numeric",
            #naturalmortality.M.file = "character",
            naturalmortality.M.vector = "data.frame",
            naturalmortality.F.type = "character",
            naturalmortality.F.constant = "numeric",
            naturalmortality.F.tmax = "numeric",
            #naturalmortality.F.file = "character",
            naturalmortality.F.vector = "data.frame",
            #totalmortality.M.file = "character",
            totalmortality.M.vector = "data.frame", 
            #totalmortality.F.file = "character",
            totalmortality.F.vector = "data.frame",
            fishingmortality = "data.frame", 
            enteringMortality = "character",
            yearsForAverage = "numeric",
            monthlysurvivability = "data.frame",
            Ftype = "character",
            fishingmortality.overall.M = "data.frame",
            fishingmortality.overall.F = "data.frame",
            Fsplittingtype = "character" 
            ) )
