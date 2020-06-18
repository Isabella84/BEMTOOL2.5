# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





showQuestionYN<-function(error_mess){
error_window <<- gtkWindow(show=FALSE)       
error_window["title"] <- "ALADYM v10.0"  
gtkWindowSetModal(error_window, TRUE)    
gtkWindowSetResizable(error_window, FALSE)
gtkWindowSetDeletable(error_window, FALSE)                
gtkWindowSetDefaultSize(error_window, 400, 20)
gtkWindowSetPosition(error_window, 3)   
  
no_button <- gtkButton("          No          ") 
#if (IN_BEMTOOL) { 
#no_button$AddCallback("clicked", no_button_event_VIT_paths)
#}
yes_button <- gtkButton("          Yes          ") 
#if (IN_BEMTOOL) {  
#yes_button$AddCallback("clicked", yes_button_event_VIT_paths)
#}
vbox <- gtkVBox()
hbox1 <- gtkHBox()
hbox1$PackStart(gtkLabel(error_mess), expand = TRUE, fill = FALSE, padding = 40)
hbox2 <- gtkHBox(homogeneous = TRUE)
hbox2$PackStart(no_button, expand = FALSE, fill = FALSE, padding = 100) 
hbox2$PackStart(ye_button, expand = FALSE, fill = FALSE, padding = 100) 
vbox$PackStart(hbox1, expand = TRUE, fill = FALSE, padding = 30)
vbox$PackStart(hbox2, expand = TRUE, fill = FALSE, padding = 20)
error_window$add(vbox)
error_window$show()

return(error_window)

}