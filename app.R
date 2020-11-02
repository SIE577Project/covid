library(shiny)# How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr)  
library(tidyr)

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


##### To display variables in dropdown menus for each category, need to manipulate current tables so that the variables are the column headers
##### Removes extra columns Allergies----------------
allergies_by_variable <- Allergies %>% select(!c("START", "STOP", "ENCOUNTER", "CODE"))
###### Changes columns to different allergy variables for dropdown menu
allergies_by_variable_wider <- allergies_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

#### Careplans---------------
careplans_by_variable <- Careplans %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "Id", "REASONCODE", "REASONDESCRIPTION"))
####
careplans_wider <- careplans_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

#### Conditions-------------------
conditions_by_variable <- Conditions %>% select(!c("START", "STOP", "ENCOUNTER", "CODE"))
####
conditions_wider <- conditions_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

#### Devices-------------------
devices_by_variable <- Devices %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "UDI"))
####
devices_wider <- devices_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

#### Imaging Studies-------------------
imagingstudies_by_variable <- Imaging_Studies %>% select(!c("Id", "DATE", "ENCOUNTER", "BODYSITE_CODE", "MODALITY_CODE", "SOP_CODE", "SOP_DESCRIPTION"))
####
imaging_studies_combined <- imagingstudies_by_variable %>% unite(DESCRIPTION, BODYSITE_DESCRIPTION:MODALITY_DESCRIPTION, remove = TRUE, sep = "    ")
#####
imagingstudies_wider <- imaging_studies_combined %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)


#### The if (interactive) command allows you to run this app in the R console (instead of having to hit the 'run app' button. It seems like this is a faster way
#### of testing out any additions you make to the app. We will remove it before publishing the final app code. 

if (interactive ()){ui <- fluidPage(useShinyjs(),
  
#####################  Navbar to display tab panels    ------------------------------------
  navbarPage("Results", id = "tabs",
             
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
                             checkboxGroupInput(inputId  = 'variableCheckboxGroup', label = "Variable Categories for Research Project",
                                                choiceNames = c("Allergies", "Careplans", "Conditions", 
                                                  "Devices", "Imaging Studies", "Medications", "Procedures",
                                                  "Insurance", "Sex", "Age"),
                                                choiceValues = c(1:10)),
                             
                             #multi-variable selection for categories chosen in checkbox
                             #varSelectInput("AllergyVariables", "Types of Allergies:", allergies_by_variable_wider, multiple = TRUE),
                      
                        
                             #checkboxGroupInput(input = 'AllergyVariableCheckBox', "Allergy Variables", allergies_by_variable_wider),
                        ),
                              #actionbutton for checkbox submission -- triggers dropdown menu appearance
                              actionButton("submit_variable", "submit", icon = NULL, width = NULL)
               
                              ),
                
                #enter code here to add things to main panel
                mainPanel(textOutput("Variable"),
                          
                          #multi-variable selection for Allergy categories 
                          varSelectInput("AllergyVariables", "Types of Allergies:", allergies_by_variable_wider, multiple = TRUE),
                          
                          #multi-variable selection for Careplan categories 
                          varSelectInput("CareplanVariables", "Types of Careplans:", careplans_wider, multiple = TRUE),
                          
                          #multi-variable selection for Condition categories
                          varSelectInput("ConditionVariables", "Types of Conditions:", conditions_wider, multiple = TRUE),
                          
                          #multi-variable selection for Device categories
                          varSelectInput("DeviceVariables", "Types of Devices:", devices_wider, multiple = TRUE),
                          
                          #multi-variable selection for Imaging categories
                          varSelectInput("ImagingVariables", "Imaging Studies:", imagingstudies_wider, multiple = TRUE),
                          
                          ),),
             
  
  
  
##################### tabPanel for data display       ---------------
                tabPanel("Screening Results", "INSERT SCREENING RESULTS FOR CHECKBOX IN THIS FIELD"))
  )



server <- function(input, output, session) {
  
  #Hides tabs before submission button is hit ---- SUBMIT BUTTON CONNECTED TO DROPDOWN MENUS, we will add another submit button for the tab reveal
  hideTab("tabs", "Screening Results")
  
  #Hide varselect dropdowns for all variables on app startup
  hide("AllergyVariables")
  hide("CareplanVariables")
  hide("ConditionVariables")
  hide("DeviceVariables")
  hide("ImagingVariables")
  
  #Triggers the tab reveal after hitting checkbox 'submit' button -- *****If/else statements don't work and need to be fixed, or another method needs to be implemented******
  observeEvent(input$submit_variable, {
    if (input$variableCheckboxGroup == 1) {
    show("AllergyVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
    else
    
    if (input$variableCheckboxGroup == 2) {
      show("CareplanVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
    else
      
    if (input$variableCheckboxGroup == 3) {
      show("ConditionVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
    else
      
    if (input$variableCheckboxGroup == 4) {
     
       show("DeviceVariables", anim = TRUE, animType = "fade", time = 0.5) }
    
      
    if (input$variableCheckboxGroup == 5) {
      show("ImagingVariables", anim = TRUE, animType = "fade", time = 0.5) }
    }
  )
  
  
  
  ## This is a chunk of code that I was testing out for the dropdown menus and haven't deleted yet. Will likely delete, but waiting to finalize the dropdown menus
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

shinyApp(ui = ui, server = server)}
