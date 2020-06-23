# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ------------------------------------------------------------------------------
# Definition of object types used in BEMTOOL code (S4 classes and methods)
# - Fleet (list of Fleetsegment)
# ------------------------------------------------------------------------------

# definition of object: bmtFleet
setClass(Class="bmtFleet",
        representation=representation(
            country = "character",
            GSA = "character",
            fleetsegments = "list",
            import.weight = "data.frame" ) )
