# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




vboxMonthlySurvivability$packStart(gtkLabelNew("Monthly proportion of survivability"),FALSE, FALSE, 0)
monthly.survivability.sw <<- gtkScrolledWindowNew(NULL, NULL)
monthly.survivability.sw$setShadowType("etched-in")
monthly.survivability.sw$setPolicy("automatic", "automatic")
monthly.survivability.sw$SetUsize(100, 70)  
monthly.survivability <<- list()
monthly.survivabilityIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
monthly.survivability.create_model()
# create tree view
monthly.survivability.treeview <<- gtkTreeViewNewWithModel(monthly.survivability.model)
monthly.survivability.treeview$setRulesHint(TRUE)
monthly.survivability.treeview$getSelection()$setMode("single")
monthly.survivability.add_columns(monthly.survivability.treeview)
monthly.survivability.sw$add(monthly.survivability.treeview)      
vboxMonthlySurvivability$packStart(monthly.survivability.sw , TRUE, TRUE, 0)   
