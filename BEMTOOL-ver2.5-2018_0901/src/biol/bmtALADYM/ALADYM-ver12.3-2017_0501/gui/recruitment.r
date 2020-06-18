# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ********************************* BIOLOGICAL graphical elements

vboxRecruitment <- gtkVBox(homogeneous = FALSE, 5)
# ------------------------------- OFFSPRING for random runs
source(paste(ALADYM_home, "/gui/recruitment/recruitment.recruits.r", sep=""))   # construction of vboxOFFSPRING

vbox_recruits <- gtkVBox(homogeneous = FALSE, 5)              # vboxOFFSPRING ->  hbox_lengthweight ->  vbox_lengthweight -> frame_lengthweight
frame_recruits <- gtkFrame(" RECRUITS ")   
hbox_recruits <- gtkHBox(homogeneous = FALSE, 5)

hbox_recruits$packStart(vboxOFFSPRING, expand = TRUE, fill = TRUE, padding = 10)
vbox_recruits$packStart(hbox_recruits, expand = FALSE, fill = TRUE, padding = 10)
frame_recruits$add(vbox_recruits) 

h_frame_recruits <- gtkHBox(homogeneous = FALSE, 5)
h_frame_recruits$packStart(frame_recruits, expand = TRUE, fill = TRUE, padding = 10)		
vboxRecruitment$packStart(h_frame_recruits, expand = FALSE, fill = FALSE, padding = 5)

# ------------------------------------------ Stock-recruitment relationship

source(paste(ALADYM_home, "/gui/recruitment/recruitment.stockrecr.r", sep="")) 
vbox_stockrecr <- gtkVBox(homogeneous = FALSE, 5)              
frame_stockrecr <- gtkFrame(" STOCK-RECRUITMENT RELATIONSHIP ")   
hbox_stockrecr <- gtkHBox(homogeneous = FALSE, 5)

hbox_stockrecr$packStart(vboxSRrelationship, expand = TRUE, fill = TRUE, padding = 10)
vbox_stockrecr$packStart(hbox_stockrecr, expand = FALSE, fill = TRUE, padding = 10)
frame_stockrecr$add(vbox_stockrecr) 

h_frame_stockrecr <- gtkHBox(homogeneous = FALSE, 5)
h_frame_stockrecr$packStart(frame_stockrecr, expand = TRUE, fill = TRUE, padding = 10)		
vboxRecruitment$packStart(h_frame_stockrecr, expand = FALSE, fill = FALSE, padding = 5)

# ----------------------------------------------- spawners

source(paste(ALADYM_home, "/gui/recruitment/recruitment.spawners.r", sep=""))
vbox_spawners <- gtkVBox(homogeneous = FALSE, 5)              
frame_spawners <- gtkFrame(" SPAWNERS ")   
hbox_spawners <- gtkHBox(homogeneous = FALSE, 5)

hbox_spawners$packStart(vboxSpawners, expand = TRUE, fill = TRUE, padding = 10)
vbox_spawners$packStart(hbox_spawners, expand = FALSE, fill = TRUE, padding = 10)
frame_spawners$add(vbox_spawners) 

h_frame_spawners <- gtkHBox(homogeneous = FALSE, 5)
h_frame_spawners$packStart(frame_spawners, expand = TRUE, fill = TRUE, padding = 10)		
vboxRecruitment$packStart(h_frame_spawners, expand = FALSE, fill = FALSE, padding = 5)


source(paste(ALADYM_home, "/gui/recruitment/recruitment.others.r", sep=""))
vbox_recr_others <- gtkVBox(homogeneous = FALSE, 5)              
frame_recr_others <- gtkFrame(" OTHERS ")   
hbox_recr_others <- gtkHBox(homogeneous = FALSE, 5)

hbox_recr_others$packStart(vboxRecrOthers, expand = FALSE, fill = TRUE, padding = 10)
vbox_recr_others$packStart(hbox_recr_others, expand = FALSE, fill = TRUE, padding = 10)

frame_recr_others$add(vbox_recr_others) 
h_frame_recr_others <- gtkHBox(homogeneous = FALSE, 5)
h_frame_recr_others$packStart(frame_recr_others, expand = TRUE, fill = TRUE, padding = 10)		
vboxRecruitment$packStart(h_frame_recr_others, expand = FALSE, fill = FALSE, padding = 5)

