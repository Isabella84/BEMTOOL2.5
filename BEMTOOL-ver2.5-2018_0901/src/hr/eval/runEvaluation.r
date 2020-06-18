
# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



runEvaluation<- function(w) {

SCENARIO_IDENTIFIER <<- "HR"

bmt_wnd_eval <- showMessage("Evaluation in progress...")
gtkWidgetSetSensitive(BMTmain_window, F)

print("Evaluation in progress...")
eval_scenarios_bio()
eval_scenarios_by_fleet()
eval_scenarios_eco()
eval_scenarios_bio_forecast()
eval_scenarios_by_fleet_forecast()
eval_scenarios_eco_forecast()

bmt_wnd_eval$destroy()
wnd <- showMessageOK("        Evaluation completed!        ")
gtkWidgetSetSensitive(BMTmain_window, T)

}