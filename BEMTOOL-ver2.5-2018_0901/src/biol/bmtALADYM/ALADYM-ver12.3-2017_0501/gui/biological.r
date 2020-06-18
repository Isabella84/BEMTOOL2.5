# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed  to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ********************************* BIOLOGICAL graphical elements

vboxBiological <- gtkVBox(homogeneous = FALSE, 5)

source(paste(ALADYM_home, "/gui/biological/biological.lengthweight.r", sep=""))
vbox_lengthweight <- gtkVBox(homogeneous = FALSE, 5)             
frame_lengthweight <- gtkFrame(" Lenght-weight relationship ")   
hbox_lengthweight <- gtkHBox(homogeneous = FALSE, 5)

hbox_lengthweight$packStart(tbl_AB, expand = FALSE, fill = TRUE, padding = 10)
vbox_lengthweight$packStart(hbox_lengthweight, expand = FALSE, fill = TRUE, padding = 10)
frame_lengthweight$add(vbox_lengthweight) 

h_frame_lengthweight <- gtkHBox(homogeneous = FALSE, 5)
h_frame_lengthweight$packStart(frame_lengthweight, expand = TRUE, fill = TRUE, padding = 5)	
#
# --------------------------------------------------- SEX ratio

source(paste(ALADYM_home, "/gui/biological/biological.sexratio.r", sep=""))
frame_sexratio <- gtkFrame(" Sex ratio ")   
frame_sexratio$add(vboxSEXRATIO)

hframe_sexratio <- gtkHBox(homogeneous = FALSE, 5)
hframe_sexratio$packStart(frame_sexratio, expand = TRUE, fill = TRUE, padding = 5)		
#

h_2frames <- gtkHBox(homogeneous = FALSE, 5)
v_2frames <- gtkVBox(homogeneous = FALSE, 5)
h_2frames$packStart(h_frame_lengthweight, expand = TRUE, fill = TRUE, padding = 5)	
h_2frames$packStart(hframe_sexratio, expand = TRUE, fill = TRUE, padding = 5)
v_2frames$packStart(h_2frames, expand = TRUE, fill = TRUE, padding = 0)	
	

vboxBiological$packStart(v_2frames, expand = FALSE, fill = FALSE, padding = 5)
	
#vboxBiological$packStart(h_frame_lengthweight, expand = FALSE, fill = FALSE, padding = 5)

# -------------------------------  Maturity Ogive

source(paste(ALADYM_home, "/gui/biological/biological.maturityogive.r", sep=""))
vbox_ogive <- gtkVBox(homogeneous = FALSE, 5)  # tbl_OGIVE ->  hbox_ogive ->  vbox_ogive -> frame_ogive
frame_ogive <- gtkFrame(" Maturity ogive ")   
hbox_ogive <- gtkHBox(homogeneous = FALSE, 5)

hbox_ogive$packStart(tbl_OGIVE, expand = FALSE, fill = TRUE, padding = 10)
vbox_ogive$packStart(hbox_ogive, expand = FALSE, fill = TRUE, padding = 10)
frame_ogive$add(vbox_ogive) 

h_frame_ogive <- gtkHBox(homogeneous = FALSE, 5)
h_frame_ogive$packStart(frame_ogive, expand = TRUE, fill = TRUE, padding = 10)		
vboxBiological$packStart(h_frame_ogive, expand = FALSE, fill = FALSE, padding = 5)

# --------------------------------------------- VonBertalanffy males parameters

source(paste(ALADYM_home, "/gui/biological/biological.vbf.r", sep=""))
vbox_vbf <- gtkVBox(homogeneous = FALSE, 5)  # tbl_VBF ->  hbox_vbf ->  vbox_vbf -> frame_vbf
frame_vbf <- gtkFrame(" Growth function ") 
hbox_vbf <- gtkHBox(homogeneous = FALSE, 5)

hbox_vbf$packStart(tbl_VBF, expand = FALSE, fill = TRUE, padding = 10)
vbox_vbf$packStart(hbox_vbf, expand = FALSE, fill = TRUE, padding = 10) 
frame_vbf$add(vbox_vbf)

h_frame_vbf <- gtkHBox(homogeneous = FALSE, 5)
h_frame_vbf$packStart(frame_vbf, expand = TRUE, fill = TRUE, padding = 10)		
vboxBiological$packStart(h_frame_vbf, expand = FALSE, fill = FALSE, padding = 5)





# ********************************* BIOLOGICAL graphical elements - end