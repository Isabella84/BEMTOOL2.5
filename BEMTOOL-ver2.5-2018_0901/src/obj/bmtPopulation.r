# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ------------------------------------------------------------------------------
# Definition of object types used in BEMTOOL code (S4 classes and methods)
# - Population
# ------------------------------------------------------------------------------

# definition of object: bmtPopulation
setClass(Class="bmtPopulation",
        representation=representation(
            GSA = "character",
            species = "character",
            meditscode = "character",
            lifespan = "data.frame",
            growth = "data.frame",
            lengthweight = "data.frame",
            maturity.params = "data.frame",
            maturity.vect = "data.frame",
            M.cost = "data.frame",
            M.vect = "list",
            offspring.prop = "data.frame",
            sexratio = "numeric") )