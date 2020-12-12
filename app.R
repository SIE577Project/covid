library(tidyr)
library(shinyWidgets)
library(eeptools)
library(tidyverse)
#install.packages("shinyalert")
library(shinyalert)
library(shiny)# How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr)  
library(shinyjs)
library(tidyr)
#setwd("C:\\Users\\t_kna\\Documents\\R_COVID_APP")
Allergies <- read.csv("..BME577APP\\merged_Allergies.csv")
Careplans <- read.csv("..BME577APP\\merged_Careplans.csv")
Conditions <- read.csv("..BME577APP\\merged_Conditions.csv")
Devices <- read.csv("..BME577APP\\merged_Devices.csv")
Encounters_Organizations_Payers <- read.csv("..BME577APP\\Encounters_Organizations_Payers.csv")
Imaging_Studies <- read.csv("..BME577APP\\merged_Imaging_Studies.csv")
Immunizations <- read.csv("..BME577APP\\merged_Immunizations.csv")
Medications <- read.csv("..BME577APP\\merged_Medications.csv")
Observations <- read.csv("..BME577APP\\merged_Observations.csv")
Organizations <- read.csv("..BME577APP\\merged_Organizations.csv")
Patients <- read.csv("..BME577APP\\merged_Patients.csv")
Payers <- read.csv("..BME577APP\\merged_Payers.csv")
Procedures <- read.csv("..BME577APP\\merged_Procedures.csv")

#### Need to manipulate original tables to display variables in each category correctly on dropdown menus,  
####Removes extra columns with !c command telling it NOT to select those columns in new dataframe
####Allergies----------------
allergies_by_variable <- Allergies %>% select(!c("START", "STOP", "ENCOUNTER", "CODE"))
######Changes rows under the DESCRIPTION column to column headers with patients as dependent variable in new dataframe
allergies_by_variable_wider <- allergies_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Careplans----------------------
careplans_by_variable <- Careplans %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "Id", "REASONCODE", "REASONDESCRIPTION"))
####
careplans_wider <- careplans_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Conditions----------------------
conditions_by_variable <- Conditions %>% select(!c("START", "STOP", "ENCOUNTER", "CODE"))
####
conditions_wider <- conditions_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Devices--------------------------
devices_by_variable <- Devices %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "UDI"))
####
devices_wider <- devices_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Imaging Studies-------------------
imagingstudies_by_variable <- Imaging_Studies %>% select(!c("Id", "DATE", "ENCOUNTER", "BODYSITE_CODE", "MODALITY_CODE", "SOP_CODE", "SOP_DESCRIPTION"))
####Combined bodysite and modality columns together so it'd list specific images on the dropdown menu (e.g. Arm Digital Radiography)
imaging_studies_combined <- imagingstudies_by_variable %>% unite(DESCRIPTION, BODYSITE_DESCRIPTION:MODALITY_DESCRIPTION, remove = TRUE, sep = "    ")
#####
imagingstudies_wider <- imaging_studies_combined %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Medications-----------------------
medications_by_variable <- Medications %>% select(!c("START", "STOP", "PAYER", "ENCOUNTER", "CODE", "BASE_COST", "PAYER_COVERAGE", "DISPENSES", "TOTALCOST", "REASONCODE", "REASONDESCRIPTION"))
####
medications_wider <- medications_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Procedures------------------------
procedures_by_variable <- Procedures %>% select(!c("DATE", "ENCOUNTER", "CODE", "BASE_COST", "REASONCODE", "REASONDESCRIPTION"))
####
procedures_wider <- procedures_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)

####Encounter (probably insurance and organization stuff too)-------------------------
####
#Encounters_filtered <- Encounters_Organizations_Payers %>% select(!c("Id", "START", "STOP", "PROVIDER", "ENCOUNTERCLASS", "CODE", "DESCRIPTION", "BASE_ENCOUNTER_COST", "TOTAL_CLAIM_COST", "PAYER_COVERAGE", "REASONCODE"))
####

####Payers-----------------------------
Payers_filtered <- Payers %>% select(!c("ADDRESS", "CITY", "STATE_HEADQUARTERED", "ZIP", "PHONE", "AMOUNT_COVERED", "AMOUNT_UNCOVERED", "REVENUE", "COVERED_ENCOUNTERS", "UNCOVERED_ENCOUNTERS", "COVERED_MEDICATIONS", "UNCOVERED_MEDICATIONS", "COVERED_PROCEDURES", "UNCOVERED_PROCEDURES", "COVERED_IMMUNIZATIONS", "UNCOVERED_IMMUNIZATIONS", "UNIQUE_CUSTOMERS", "QOLS_AVG", "MEMBER_MONTHS"))
####
Payers_wider <- pivot_wider(Payers_filtered, names_from = "NAME", values_from = "Id")

####Patients--------------------------
Patients_filtered <- Patients %>% select(!c("SSN", "DRIVERS", "PASSPORT", "PREFIX", "FIRST", "LAST", "SUFFIX", "MAIDEN", "MARITAL", "BIRTHPLACE", "ADDRESS", "CITY", "STATE", "COUNTY", "ZIP", "HEALTHCARE_EXPENSES", "HEALTHCARE_COVERAGE"))
####
Patients_deadremoved <- Patients_filtered[ (Patients_filtered$DEATHDATE==""),]
Patients_wider_sex <- pivot_wider(Patients_filtered, names_from = "GENDER", values_from = "Id")
Patients_wider_ethnicity <- pivot_wider(Patients_filtered, names_from = c("ETHNICITY", "RACE"), values_from = "Id")

PatientsBirthdates <- as.integer(gsub("-\\d\\d-\\d\\d", "", Patients_deadremoved$BIRTHDATE))
PatientAge <- cbind(Patients_deadremoved[2], PatientsBirthdates)
PatientAge$Age <- as.integer(2020 - PatientAge$PatientsBirthdates)


shinyUI(fluidPage(
  useShinyjs(),
  useShinyalert(),
  
  
  #####################  Navbar to display tab panels    ------------------------------------
  navbarPage("", id = "MainTab",
             
             ###################MaintabPanel  ---------------------------------------
             tabPanel("Variable Selection",
                      
                      
                      
                      
                      #App title
                      titlePanel(strong(style = "color:blue", "Diagnostix Patient Aggregator")), 
                      
                      
                      #Modify sidebarlayout to change side/main panel positions/sizing
                      sidebarLayout(position = "left",
                                    
                                    #enter code here to add things to side panel
                                    
                                    #sidebarPanel title
                                    sidebarPanel((""), fluid = TRUE,
                                                 
                                                 
                                                 
                                                 ### For variable category submission -- Pretty toggles for each category + corresponding 
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
                                                 #Toggle for Race/Ethnicity
                                                 prettyToggle(
                                                   inputId = "RaceEthnicityCategory",
                                                   label_on = "Race & Ethnicity",
                                                   label_off = "Race & Ethnicity",
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
                                                 
                                                 
                                                 #actionbutton for Pretty Toggle choice submission  
                                                 actionButton("submit_categories", "Submit Categories", icon = NULL, width = NULL)
                                                 
                                                 
                                                 
                                    ),
                                    
                                    #enter code here to add things to main panel
                                    mainPanel(textOutput("Variable Categories"),
                                              
                                              
                                              
                                              #multi-variable selection for Allergies
                                              selectizeInput(inputId = "AllergyVariables", label = "Types of Allergies:", choices = allergies_by_variable_wider[,2:16], multiple = TRUE),
                                              
                                              #multi-variable selection for Careplans
                                              selectizeInput(inputId = "CareplanVariables", label = "Types of Careplans:", choices = careplans_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Conditions
                                              selectizeInput(inputId = "ConditionVariables", label = "Types of Conditions:", choices = conditions_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Devices
                                              selectizeInput(inputId = "DeviceVariables", label = "Types of Devices:", choices = devices_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Imaging Studies
                                              selectizeInput(inputId = "ImagingVariables", label = "Imaging Studies:", choices = imagingstudies_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Medications
                                              selectizeInput(inputId = "MedicationVariables", label = "Medications:", choices = medications_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Procedures
                                              selectizeInput(inputId = "ProcedureVariables", label = "Procedures:", choices = procedures_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Payers
                                              selectizeInput(inputId = "PayerVariables", label = "Payers:", choices = Payers_wider, multiple = TRUE),
                                              
                                              #multi-variable selection for Patient Sex
                                              selectizeInput(inputId = "PatientSex", label = "Sex:", choices = Patients_wider_sex[8:9], multiple = TRUE), 
                                              
                                              #mult-variable selection for Patient Race/Ethnicity
                                              selectizeInput(inputId = "PatientRaceEthnicity", label = "Race & Ethnicity:", choices = Patients_wider_ethnicity[7:16], multiple = TRUE), 
                                              
                                              #actionbutton for varSelectInput choice submission + reveal of second 'output' tab
                                              actionButton("submit_variables", "Submit Variables", icon  = NULL, width = NULL)
                                    )
                      )
                      
                      
             ),
             
             ##################### tabsetPanel for data display       ---------------
             tabPanel("Patient Results", "INSERT SCREENING RESULTS FOR CHECKBOX IN THIS FIELD"
                      
                      # dataTableOutput('FilteredPatients')
                      
             )
  )
)
)

server <- function(input, output, session) {
  
  #Hides tabs before submission button is hit
  hideTab( inputId = "MainTab", target = "Patient Results")
  
  #Hides varSelectInput submission button before first submit button is pressed
  hide("submit_variables")
  
  #Hide varselect dropdowns for all variables
  hide("AllergyVariables")
  hide("CareplanVariables")
  hide("ConditionVariables")
  hide("DeviceVariables")
  hide("ImagingVariables")
  hide("MedicationVariables")
  hide("ProcedureVariables")
  hide("PayerVariables")
  hide("PatientSex")
  hide("PatientRaceEthnicity")
  
  #Triggers the multiselect menu reveal after hitting PrettyToggle 'variable category' submit button
  observeEvent(input$submit_categories, {
    
    
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
    
    if (input$SexCategory == TRUE)
      show("PatientSex", anim = TRUE, animType = "fade", time = 0.5)
    else hide("PatientSex")
    
    if (input$RaceEthnicityCategory == TRUE)
      show("PatientRaceEthnicity", anim = TRUE, animType = "fade", time = 0.5)
    else hide("PatientRaceEthnicity")
    
    CategorySelectOutput <- as.logical(list(input$AllergyCategory, input$CareplanCategory, input$ConditionCategory, 
                                            input$DeviceCategory, input$ImagingCategory, input$MedicationCategory, input$ProcedureCategory,
                                            input$PayerCategory, input$SexCategory, input$RaceEthnicityCategory))
    
    
    if (any(CategorySelectOutput) == TRUE )
      show("submit_variables", anim = TRUE, animType = "fade", time = 0.5)
    else { hide("submit_variables") 
      shinyalert("Uhhh... excuse me??", text = "Please select at least one category", type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE, showConfirmButton = FALSE, timer = 0, animation = TRUE, size = "m", immediate = TRUE)
    }
    
  }
  )
  
  # Event observation following submission of selected variables  
  observeEvent(input$submit_variables,
               
               showTab(inputId = "MainTab", target = "Patient Results", select = TRUE), 
               
              # FilteredPatients <- data.frame(Patients[2])
               
               
               
               
  )
  
  #Table output
 # output$FilteredPatients <- renderDataTable(PatientDataTable(),
  #                                           options = list,
   #                                          searchDelay = 500,
    #                                         env)
  
  #Reactive function for creating patient table               
  
  #observeEvent(input$submit_variables,              
   #            PatientDataTable <- reactive({ 
    #             
     #            data.table::as.data.table( 
      #             select(allergies_by_variable_wider[,2:16], input$AllergyVariables) %>%
       #              select(careplans_wider , input$CareplanVariables) %>%
        #             select(conditions_wider , input$ConditionVariables) %>%
         #            select(devices_wider , input$DeviceVariables) %>%
          #           select(imagingstudies_wider , input$ImagingVariables) %>%
           #          select(medications_wider , input$MedicationVariables) %>%
            #         select(procedures_wider , input$ProcedureVariables) %>%
             #        select(Payers_wider , input$PayerVariables) %>%
              #       select(Patients_wider_sex[8:9] , input$PatientSex) %>%
               #      select(Patients_wider_ethnicity[7:16] , input$PatientRaceEthnicity) 
                # )
               #})
  #)
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
