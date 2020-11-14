library(tidyr)
library(shinyWidgets)
library(eeptools)

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
                          varSelectInput("PayerVariables", "Payers:", Payers_wider, multiple = TRUE),
                          
                          #multi-variable selection for Patient Sex
                          varSelectInput("PatientSex", "Sex:", Patients_wider_sex[8:9], multiple = TRUE), 
                          
                          #mult-variable selection for Patient Race/Ethnicity
                          varSelectInput("PatientRaceEthnicity", "Race & Ethnicity:", Patients_wider_ethnicity[7:16], multiple = TRUE)
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
  hide("PatientSex")
  hide("PatientRaceEthnicity")
  
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
    
    if (input$SexCategory == TRUE)
      show("PatientSex", anim = TRUE, animType = "fade", time = 0.5)
    else hide("PatientSex")
      
    if (input$RaceEthnicityCategory == TRUE)
      show("PatientRaceEthnicity", anim = TRUE, animType = "fade", time = 0.5)
    else hide("PatientRaceEthnicity")
    
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
