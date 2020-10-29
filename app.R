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


ui <- fluidPage(
  
  #App title
  titlePanel(strong(style = "color:blue", "Diagnostix Patient Aggregator")), 
  
  #Modify sidebarlayout to change side/main panel positions/sizing
  sidebarLayout(position = "left",
                
                #enter code here to add things to side panel
                sidebarPanel((""),
                             checkboxGroupInput("variable", "Variables for Research Project",
                                                c("Allergies", "Careplans", "Conditions", 
                                                  "Devices", "Imaging Studies", "Medications", "Procedures",
                                                  "Insurance", "Sex", "Age"),),
                             ),
                
                #enter code here to add things to main panel
                mainPanel(textOutput("Allergies"))
  
)
)

server <- function(input, output) {
  
  output$Allergies <- renderText({paste("", input$variable)})
}

shinyApp(ui = ui, server = server)
