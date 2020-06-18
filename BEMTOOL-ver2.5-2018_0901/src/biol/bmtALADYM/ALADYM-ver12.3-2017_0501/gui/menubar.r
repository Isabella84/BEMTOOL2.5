# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ***************************************************** MENUBAR definition
actions <- list(
list("FileMenu", NULL, "_File"),  
list("Open", "gtk-open", "_Open File", "<control>O",
"Select a CSV file to load as a spreadsheet", open_cb),
list("Save", "gtk-save", "_Save", "<control>S",
"Save the current spreadsheet to a CSV file", save_cb),
list("Quit", "gtk-quit", "_Quit", "<control>Q",
"Quit the application", quit_cb)
)   
action_group <- gtkActionGroup("fileActions")
action_group$addActions(actions, main_window)
ui_manager$insertActionGroup(action_group, 0)

actions <- list(
list("RunMenu", NULL, "_Run"),  
list("RunSimulation", NULL, "_Run Simulation", "<control>R",
"Run simulation", run_simulation),
list("RunForecast", NULL, "_Run Forecast", "<control>F",
"Run forecast", run_forecast)
)   
action_group <- gtkActionGroup("runActions")
action_group$addActions(actions, main_window)
ui_manager$insertActionGroup(action_group, 0)

actions <- list(
list("HelpMenu", NULL, "_Help"),  
list("Help", NULL, "_Aladym Help", "<control>H",
"Aladym Help", run_help)
)   
action_group <- gtkActionGroup("helpActions")
action_group$addActions(actions, main_window)
ui_manager$insertActionGroup(action_group, 0)

merge <- ui_manager$newMergeId()         

ui_manager$addUi(merge.id = merge, path = "/", name = "menubar", action = NULL, type = "menubar", top = FALSE)
ui_manager$addUi(merge, "/menubar", name ="help", action = "HelpMenu", type = "menu", top = TRUE)
ui_manager$addUi(merge, "/menubar/help", name ="runhelp", action = "Help", type = "menuitem", top = FALSE)
ui_manager$addUi(merge, "/menubar", name ="run", action = "RunMenu", type = "menu", top = TRUE)
ui_manager$addUi(merge, "/menubar/run", name ="runsimulation", action = "RunSimulation", type = "menuitem", top = FALSE)
ui_manager$addUi(merge, "/menubar/run", name ="runforecast", action = "RunForecast", type = "menuitem", top = FALSE)
ui_manager$addUi(merge, "/menubar", name ="file", action = "FileMenu", type = "menu", top = TRUE)
ui_manager$addUi(merge, "/menubar/file", name ="open", action = "Open", type = "menuitem", top = FALSE)
ui_manager$addUi(merge, "/menubar/file", name ="save", action = "Save", type = "menuitem", top = FALSE)
ui_manager$addUi(merge, "/menubar/file", name ="quit", action = "Quit", type = "menuitem", top = FALSE)