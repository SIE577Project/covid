library(tidyr)
library(shinyWidgets)
library(tidyverse)
library(shiny)# How we create the app.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr)  
library(tidyr)
library(DT)
library(ggplot2)
library(esquisse)
library(leaflet.extras)

## Reading data into environment
setwd("C:\\Users\\t_kna\\Documents\\R_COVID_APP")
Allergies <- read.csv("merged_Allergies.csv")
Careplans <- read.csv("merged_Careplans.csv")
Devices <- read.csv("merged_Devices.csv")
Imaging_Studies <- read.csv("merged_Imaging_Studies.csv")
Immunizations <- read.csv("merged_Immunizations.csv")
Observations <- read.csv("merged_Observations.csv")
Organizations <- read.csv("merged_Organizations.csv")
Patients <- read.csv("merged_Patients.csv")
## Medications and other large data files removed due to large size causing app to crash

#### Need to manipulate original tables to display variables in each category correctly on dropdown menus,  
####Removes extra columns with !c command telling it NOT to select those columns in new dataframe
####Allergies----------------
allergies_by_variable <- Allergies %>% select(!c("START", "STOP", "ENCOUNTER", "CODE"))
######Changes rows under the DESCRIPTION column to column headers with patients as dependent variable in new dataframe
allergies_by_variable_wider <- allergies_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)
#Changing name of Allergy col to avoid redundant "DESCRIPTION" name in data set columns
colnames(Allergies)[colnames(Allergies)=="DESCRIPTION"] <- "ALLERGY"
####Careplans----------------------
careplans_by_variable <- Careplans %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "Id", "REASONCODE", "REASONDESCRIPTION"))
####
careplans_wider <- careplans_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)
####
colnames(Careplans)[colnames(Careplans)=="DESCRIPTION"] <- "CAREPLAN"
####Devices--------------------------
devices_by_variable <- Devices %>% select(!c("START", "STOP", "ENCOUNTER", "CODE", "UDI"))
####
devices_wider <- devices_by_variable %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)
colnames(Devices)[colnames(Devices)=="DESCRIPTION"] <- "DEVICE"
####Imaging Studies-------------------
imagingstudies_by_variable <- Imaging_Studies %>% select(!c("Id", "DATE", "ENCOUNTER", "BODYSITE_CODE", "MODALITY_CODE", "SOP_CODE", "SOP_DESCRIPTION"))
####Combined bodysite and modality columns together so it'd list specific images on the dropdown menu (e.g. Arm Digital Radiography)
imaging_studies_combined <- imagingstudies_by_variable %>% unite(DESCRIPTION, BODYSITE_DESCRIPTION:MODALITY_DESCRIPTION, remove = TRUE, sep = "    ")
#####
imagingstudies_wider <- imaging_studies_combined %>% pivot_wider(names_from = DESCRIPTION, values_from = PATIENT)
colnames(Imaging_Studies)[colnames(Imaging_Studies)=="MODALITY_DESCRIPTION"] <- "IMAGING MODALITY"
####Patients--------------------------
Patients_filtered <- Patients %>% select(!c("SSN", "DRIVERS", "PASSPORT", "PREFIX", "FIRST", "LAST", "SUFFIX", "MAIDEN", "MARITAL", "BIRTHPLACE", "ADDRESS", "CITY", "STATE", "COUNTY", "ZIP", "HEALTHCARE_EXPENSES", "HEALTHCARE_COVERAGE"))
####
Patients_deadremoved <- Patients_filtered[ (Patients_filtered$DEATHDATE==""),]
#renaming patient ID col name for creation of master data frame
colnames(Patients_deadremoved)[colnames(Patients_deadremoved)=="Id"] <- "PATIENT"
Patients_wider_sex <- pivot_wider(Patients_filtered, names_from = "GENDER", values_from = "Id")
Patients_wider_ethnicity <- pivot_wider(Patients_filtered, names_from = c("ETHNICITY", "RACE"), values_from = "Id")
###Finding Patient Age and removing deceased patients
PatientsBirthdates <- as.integer(gsub("-\\d\\d-\\d\\d", "", Patients_deadremoved$BIRTHDATE))
PatientAge <- cbind(Patients_deadremoved[2], PatientsBirthdates)
PatientAge$Age <- as.integer(2020 - PatientAge$PatientsBirthdates)
#Creating master data frame by merging pertinent variable cols with living patient data set
Patients_deadremoved1 <- Patients_deadremoved %>% left_join(Allergies[,c("PATIENT", "ALLERGY")], by = "PATIENT") 
Patients_deadremoved2 <- Patients_deadremoved1 %>% left_join(Careplans[,c("PATIENT","CAREPLAN")], by = "PATIENT")
#Patients_deadremoved3 <- Patients_deadremoved2 %>% left_join(Conditions[,c("PATIENT","CONDITION")], by = "PATIENT")  
Patients_deadremoved3 <- Patients_deadremoved2 %>% left_join(Devices[,c("PATIENT","DEVICE")], by = "PATIENT") 
Patients_deadremoved4 <- Patients_deadremoved3 %>% left_join(Imaging_Studies[,c("PATIENT","IMAGING MODALITY")], by = "PATIENT") 
Patients_deadremoved5 <- Patients_deadremoved4 %>% left_join(PatientAge[,c("PATIENT","Age")], by = "PATIENT")
MasterDataFrame <- Patients_deadremoved5 %>%distinct(.keep_all = FALSE)
MasterDataFrame$ALLERGY <- as.character(MasterDataFrame$ALLERGY)
MasterDataFrame$CAREPLAN <- as.character(MasterDataFrame$CAREPLAN)
MasterDataFrame$DEVICE <- as.character(MasterDataFrame$DEVICE)
MasterDataFrame$`IMAGING MODALITY` <- as.character(MasterDataFrame$`IMAGING MODALITY`)
MasterDataFrame$RACE <-as.character(MasterDataFrame$RACE)
MasterDataFrame$ETHNICITY <-as.character(MasterDataFrame$ETHNICITY)
####Creating dataframe for filtered table 
USETHISFORDATAFILTER <- MasterDataFrame %>% select(!c("X", "PATIENT", "BIRTHDATE", "DEATHDATE"))
PatientLocation <- MasterDataFrame %>% select(c("LAT", "LON"))
MapLocation <-tibble("LAT" = 41, "LON" = -70)



if (interactive())
ui <- fluidPage(

    
    
    
#####################  Navbar to display tab panels    ------------------------------------
navbarPage("Page Select", id = "MainTab",
               
###################MaintabPanel  ---------------------------------------
tabPanel("Variable Selection",
                        
                        
                        
                        
#App title
titlePanel(em(style = "color:blue", position = "center", "Diagnostix Patient Aggregator")), 
                        
                        
#Modify sidebarlayout to change side/main panel positions/sizing
sidebarLayout(position = "left",
                                      
#enter code here to add things to side panel
                                      
#sidebarPanel title
sidebarPanel((""), fluid = TRUE,
                                                   
#actionbutton for varSelectInput choice submission + reveal of second 'output' tab
actionButton("submit_variables", "Submit Variables", icon  = NULL, width = NULL)
),#end of sidebar
                                      
#enter code here to add things to main panel
mainPanel(textOutput("Variable Categories"),

#Filter Module
filterDF_UI(id = "filtering", show_nrow = TRUE),
                                                
                                                
)# End of Main Panel
)# End of sidebarlayout
),# End of Variable Selection tab
               
##################### tabPanel for data table display       ---------------
tabPanel("Filtered Patients",
sidebarPanel(
#Panel for showmap button
panel(
actionButton(inputId = "showmap", label = "Show Patient Map")), #End of panel
width = "50%",
position = "left",
DT::dataTableOutput(outputId = "PatientTable")         
),
sidebarPanel( width = "50%",
position = "right",
                                      
                                      
)
),
               
##################### tabPanel for patient map display       ---------------              
tabPanel("MAPTAB", 
                        
leafletOutput( "mymap", width = "1000", height ="1000"), 
                        
actionButton("recalc", "Reset"),
                        
)#Close to MAPTAB
)#Close to Navbar page
)#Close to UI



server <- function(input, output, session) {
  
  
#Shows map tab when 'show patient map' button is pressed  
observeEvent(
input$showmap,
showTab(inputId = "MainTab", target = "MAPTAB", select = TRUE)
)
  
  
  
  
#Creates output for leaflet map  
output$mymap <- renderLeaflet({
leaflet() %>% 
addTiles()  %>%
setView(lng= -99, lat = 45, zoom = 5)  %>%
addCircles( data = MapLocation(), 
weight = 0.5,
radius = 1,
color = "black", 
)
}) 
  
#Overlays filtered patient data onto map when showmap button is pressed
observe({ 
input$showmap
    
leafletProxy("mymap", data = MapLocation()) %>%
clearShapes %>%
addCircles(
weight = 1,
radius = 1,
color = "black"
)
})

#MapLocation is data table with columns LAT and LON that is reactively overwritten by the res_filter$data_filtered() module used 
#to create the data table
MapLocation <- reactive({
PatientLocation <- res_filter$data_filtered()
})
  
  
#Hides tabs before submission button is hit
hideTab(inputId = "MainTab", target = "Filtered Patients")
hideTab(inputId = "MainTab", target  = "MAPTAB")  
  
  
#Shows tab containing patient data table after variables are submitted  
observeEvent(input$submit_variables,
               showTab(inputId = "MainTab", target = "Filtered Patients", select = TRUE), 
)
  
  
#PatientDataSet
data <- reactive ({USETHISFORDATAFILTER})
  
res_filter <- callModule(
module = filterDF,
id = "filtering",
data_table = data, 
picker = TRUE
)
  
  
  
#Creates data table that will be filtered  
output$PatientTable <- DT::renderDT({
res_filter$data_filtered()
}, options = list(pageLength = 10))
  
  
}

shinyApp(ui=ui, server=server)
