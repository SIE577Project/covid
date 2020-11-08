library(shiny)# How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr)  

#---When running this app on your computer, you will likely have to change the file path. I'm not sure how it's going to work out, honestly, so let me know when you get
#---a chance to run it on your R console------
setwd("C:\\Users\\t_kna\\Documents\\R_COVID_APP")

Allergies <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Allergies.csv")
Careplans <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Careplans.csv")
Conditions <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Conditions.csv")
Devices <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Devices.csv")
Encounters <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Encounters.csv")
Imaging_Studies <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Imaging_Studies.csv")
Immunizations <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Immunizations.csv")
Medications <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Medications.csv")
Observations <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Observations.csv")
Organizations <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Organizations.csv")
Patients <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Patients.csv")
Payers <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Payers.csv")
Procedures <- read.csv("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\merged_Procedures.csv")



if (interactive ())
  ui <- fluidPage(
    useShinyjs(),
  
#####################  Navbar to display tab panels    ------------------------------------
  navbarPage("", id = "tabs",
             
###################MaintabPanel  ---------------------------------------
             tabPanel("Variable Selection",
             
            
             
  #App title
  titlePanel(strong(style = "color:blue", "Diagnostix Patient Aggregator")), 
  
  
  #Modify sidebarlayout to change side/main panel positions/sizing
  sidebarLayout(position = "left",
                
                #enter code here to add things to side panel
                
                #sidebarPanel title
                sidebarPanel((""), fluid = TRUE,
                             
                             #checkboxinputs 
                             #checkboxGroupInput(inputId  = 'variableCheckboxGroup', label = "Variable Categories for Research Project",
                              #                  choiceNames = c("Allergies", "Careplans", "Conditions", 
                               #                   "Devices", "Imaging Studies", "Medications", "Procedures",
                                #                  "Insurance", "Sex", "Age"),
                                 #               choiceValues = c(1:10)),
                             
                             #multi-variable selection for categories chosen in checkbox
                             #varSelectInput("AllergyVariables", "Types of Allergies:", allergies_by_variable_wider, multiple = TRUE),
                      
### new idea for variable category submission -- Pretty toggles for each category + corresponding 
### dropdown checklist for specific variables
#Toggle for Allergies
prettyToggle(
  inputId = "AllergyCategory",
  label_on = "Allergies",
  label_off = "Allergies",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  shape = "round",
  plain = TRUE,
  value = FALSE),
#Toggle for Careplans
prettyToggle(
  inputId = "CareplanCategory",
  label_on = "Careplans",
  label_off = "Careplans",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Conditions
prettyToggle(
  inputId = "ConditionCategory",
  label_on = "Conditions",
  label_off = "Conditions",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Devices
prettyToggle(
  inputId = "DeviceCategory",
  label_on = "Devices",
  label_off = "Devices",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Imaging Studies
prettyToggle(
  inputId = "ImagingCategory",
  label_on = "Imaging Studies",
  label_off = "Imaging Studies",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Medications
prettyToggle(
  inputId = "MedicationCategory",
  label_on = "Medications",
  label_off = "Medications",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Procedures
prettyToggle(
  inputId = "ProcedureCategory",
  label_on = "Procedures",
  label_off = "Procedures",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Insurance
prettyToggle(
  inputId = "PayerCategory",
  label_on = "Insurance",
  label_off = "Insurance",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Sex
prettyToggle(
  inputId = "SexCategory",
  label_on = "Sex",
  label_off = "Sex",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
#Toggle for Age
prettyToggle(
  inputId = "AgeCategory",
  label_on = "Age",
  label_off = "Age",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  value = FALSE),
                             
                             
                              #actionbutton for checkbox submission  
                              actionButton("submit_variable", "submit", icon = NULL, width = NULL)
               
                              ),
                
                #enter code here to add things to main panel
                mainPanel(textOutput("Variable"),
                          
                          #multi-variable selection for Allergies
                          varSelectInput("AllergyVariables", "Types of Allergies:", allergies_by_variable_wider[,2:16], multiple = TRUE),
                          
                          #multi-variable selection for Careplans
                          varSelectInput("CareplanVariables", "Types of Careplans:", careplans_wider, multiple = TRUE),
                          
                          #multi-variable selection for Conditions
                          varSelectInput("ConditionVariables", "Types of Conditions:", conditions_wider, multiple = TRUE),
                          
                          #multi-variable selection for Devices
                          varSelectInput("DeviceVariables", "Types of Devices:", devices_wider, multiple = TRUE),
                          
                          #multi-variable selection for Imaging Studies
                          varSelectInput("ImagingVariables", "Imaging Studies:", imagingstudies_wider, multiple = TRUE),
                          
                          #multi-variable selection for Medications
                          varSelectInput("MedicationVariables", "Medications:", medications_wider, multiple = TRUE),
                          
                          #multi-variable selection for Procedures
                          varSelectInput("ProcedureVariables", "Procedures:", procedures_wider, multiple = TRUE),
                          
                          #multi-variable selection for Payers
                          varSelectInput("PayerVariables", "Payers:", Payers_wider, multiple = TRUE)
                          ),),
             

  
  
##################### tabPanel for data display       ---------------
                tabPanel("Screening Results", "INSERT SCREENING RESULTS FOR CHECKBOX IN THIS FIELD"))
  ),)



server <- function(input, output, session) {
  
  #Hides tabs before submission button is hit
  hideTab("tabs", "Screening Results")
  
  #Hide varselect dropdowns for all variables
  hide("AllergyVariables")
  hide("CareplanVariables")
  hide("ConditionVariables")
  hide("DeviceVariables")
  hide("ImagingVariables")
  hide("MedicationVariables")
  hide("ProcedureVariables")
  hide("PayerVariables")
  
  #Triggers the tab reveal after hitting checkbox 'submit' button
  observeEvent(input$submit_variable, {
    
   
    
    
    if (input$AllergyCategory == TRUE) 
    show("AllergyVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("AllergyVariables")
    
    if (input$CareplanCategory == TRUE) 
      show("CareplanVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("CareplanVariables")
    
    if (input$ConditionCategory == TRUE) 
      show("ConditionVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("ConditionVariables")
    
    if (input$DeviceCategory == TRUE) 
      show("DeviceVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("DeviceVariables")
    
    if (input$ImagingCategory == TRUE) 
      show("ImagingVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("ImagingVariables")
    
    if (input$MedicationCategory == TRUE) 
      show("MedicationVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("MedicationVariables")
    
    if (input$ProcedureCategory == TRUE) 
      show("ProcedureVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("ProcedureVariables")
    
    if (input$PayerCategory == TRUE)
      show("PayerVariables", anim = TRUE, animType = "fade", time = 0.5)
    else hide("PayerVariables")
    
      
    #if (input$variableCheckboxGroup == 3) {
     # show("ConditionVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
    
      
    #if (input$variableCheckboxGroup == 4) {
     # show("DeviceVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
    
      
    #if (input$variableCheckboxGroup == 5) {
     # show("ImagingVariables", anim = TRUE, animType = "fade", time = 0.5) }
    }
  )
  
  
  
  
  #Reveals variableSelectInput for Allergies
  #observe({
   # 
    #variableselection <- input$variableCheckboxGroup
    #
    #Hides choices when allergy category not selected
    #if (is.null(variableselection))
     # variableselection <- character(0)
    #
    #Setting label and selection items
    #updateVarSelectInput(session, "variableCheckboxGroup",
     #                    label = "Types of Allergies:", 
      #                   
       #                  
        #                 data = allergies_by_variable_wider)
  
  #})
  
  
}

shinyApp(ui = ui, server = server)
