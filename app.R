library(shiny)# How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr)  

Allergies <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\allergies.csv")
Careplans <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\careplans.csv")
Conditions <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\conditions.csv")
Devices <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\devices.csv")
Encounters <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\encounters.csv")
Imaging_Studies <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\imaging_studies.csv")
Immunizations <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\immunizations.csv")
Medications <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\medications.csv")
Observations <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\observations.csv")
Organizations <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\organizations.csv")
Patients <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\patients.csv")
Payers <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\payers.csv")
Procedures <- read.csv2("C:\\Users\\t_kna\\Documents\\R_COVID_APP\\data\\procedures.csv")


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
