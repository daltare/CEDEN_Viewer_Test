# CEDEN Web Services Application

# Load Libraries
    library(shiny)
    library(tidyverse)
    library(leaflet)
    library(httr)
    library(jsonlite)
    library(dplyr)
    library(urltools)
    library(tidyverse)
    library(shinycssloaders)

# Load the query function
    source('functions.R')

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Test CEDEN Data Viewer (w/ CEDEN Web Services Interface)"),
   
   # Sidebar with an input box 
   sidebarLayout(
      sidebarPanel(
         textInput('parameter',
                   'Analyte (NOTE: use /% as a wildcard):',
                   value = 'E. coli'),
         textInput('county',
                   'County:',
                   value = 'Sacramento'),
         numericInput('min_year',
                      'Start Year:',
                      value = 2014),
         numericInput('max_year',
                      'End Year:',
                      value = 2014),
         actionButton('refresh','Update')
      ),
      
      # Show the map
      mainPanel(
         withSpinner(leafletOutput('map', height = 700))
      )
   )
)

# Define server logic required to draw the map
server <- function(input, output) {

    observeEvent(input$refresh, {

        filter_string <- paste0('"filter":[{"county":"', input$county,'","parameter":"', input$parameter,'","sampleDateMin":"1/1/', input$min_year, '","sampleDateMax":"12/31/', input$max_year, '"}]')
        
        # check the connection to determine if it's coming from the CalEPA server or not - if not, remove the port number
            if (.Platform$OS.type == "windows") {
                ipmessage <- system("ipconfig", intern = TRUE)
            } else {
                ipmessage <- system("ifconfig", intern = TRUE)
            }    
        
            IP_check <- grepl('ca.epa.local', ipmessage[7])
            if (IP_check == TRUE) {
                base_URI = 'https://testcedenwebservices.waterboards.ca.gov:9267'
            }
            else {
                base_URI = 'https://testcedenwebservices.waterboards.ca.gov'
            }
      
        withProgress(message = paste0('Getting Data (', input$parameter, ' - ', input$county, ', ', input$min_year, '-',input$max_year, ')'), value = 1, {
            API_data_WQresults <- ceden_query(service = 'cedenwaterqualityresultslist', query_parameters = filter_string, userName = 'testInternal', password = 'p', base_URI = base_URI)            
        })

        if (API_data_WQresults!='No Data') {
            output$map <- renderLeaflet({
                leaflet(API_data_WQresults) %>%
                    addTiles() %>%
                    addCircleMarkers(
                        radius = 3, opacity = 0.5,
                        popup = ~paste('<b>', 'Analyte: ', '</b>', analyte,"<br/>",
                                       '<b>', 'Station: ', '</b>', stationName,"<br/>",
                                       '<b>', 'Sampling Agency: ', '</b>', sampleAgency,"<br/>",
                                       '<b>', 'Lab: ', '</b>', labAgency,"<br/>",
                                       '<b>', 'Sample Date: ', '</b>', substr(sampleDate,1,10),"<br/>",
                                       '<b>', 'Result Code: ', '</b>', resultQualCode,"<br/>",
                                       '<b>', 'Result: ', '</b>', result, unit),
                        clusterOptions = markerClusterOptions()
                        )
            })
        } else {
            output$map <- renderLeaflet({ 
                leaflet() %>% addTiles() %>% setView(lat = 38.3, lng = -119.0, zoom = 5) 
            })
                showNotification("No data returned", type = 'error')    
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

