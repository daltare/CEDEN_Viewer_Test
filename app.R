# CEDEN Web Services Application
# This is meant to test the functionality of the web services associated with the CEDEN database and
# demonstrate how they can be used to build dynamic tools and applications that access CEDEN data 
# programatically and in real-time.

# Load Libraries
    library(shiny)
    library(tidyverse)
    library(leaflet)
    library(shinycssloaders)
    # Load the CEDEN query function via the cedenTools package
        library(cedenTools) # this package can be installed with the following command: devtools::install_github('daltare/cedenTools')
    
# Load a list of CA counties
    counties_list <- read.csv('data/CA_Counties_List.csv')
    counties_list <- rbind(data.frame(County.Name = 'All Counties'), counties_list) # add an 'All Counties' option to the top of the list

# Define the user interface for the application ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Test CEDEN Data Viewer (with CEDEN Web Services)"),
   
   # Sidebar with an input boxes
   sidebarLayout(
      sidebarPanel(
          h3('Filters'),
         p(h6('NOTE: for a wildcard in any field use: /%')),
         textInput('parameter',
                   'Analyte:',
                   value = 'E. coli'),
         selectInput(inputId = 'county',
                     label = 'County:',
                     choices = counties_list$County.Name,
                     multiple = TRUE,
                     selected = 'Sacramento'),
         dateRangeInput(inputId = 'date_range',label = 'Date Range:', start = '2014-01-01',end = '2014-12-31'),
         actionButton('refresh','Update')
      ),
      
      # Show the map
      mainPanel(
         withSpinner(leafletOutput('map', height = 700))
      )
   )
)

# Define the server logic required to draw the map
server <- function(input, output) {
        
    # Check the connection to determine if it's coming from the CalEPA server or not - if not, remove the port number
    # Do this so that the app works both (1) locally and (2) on the shinyapps.io server without needing modification
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
    
    observeEvent(input$refresh, {
        # Cycle through all of the counties selected, and get the data that meets the query parameters
            for (i in 1:length(input$county)) {
                if ('All Counties' %in% input$county) {
                    county_input <- c('/%', 'All Counties') # the first element is for the filter string, and the second is for the progress message
                    i <- length(input$county) # set i to be the total number of counties, so only 1 query is performed and there isn't any duplicate data requested
                    
                } else {
                    county_input <- c(input$county[i], input$county[i]) # the first element is for the filter string, and the second is for the progress message
                }
                filter_string <- paste0('"filter":[{"county":"', county_input[1],'","parameter":"', input$parameter,'","sampleDateMin":', format(input$date_range[1], '%m/%d/%Y'), '","sampleDateMax":"', format(input$date_range[2], '%m/%d/%Y'), '"}]')
                withProgress(message = paste0('Getting Data (', input$parameter, ', ', county_input[2], ', ', input$date_range[1], ' - ',input$date_range[2], ')'), value = 1, {
                    API_data_WQresults <- ceden_query_csv(service = 'cedenwaterqualityresultslist', query_parameters = filter_string, userName = 'testInternal', password = 'p', base_URI = base_URI)
                })
                if (i == 1 | county_input == '/%') {
                    API_data_WQresults_FINAL <- API_data_WQresults
                } else {
                    API_data_WQresults_FINAL <- bind_rows(API_data_WQresults_FINAL, API_data_WQresults)    
                }
                if (i==length(input$county)) {
                    break()
                }
            }
        
        # If valid data is returned, draw the map
            if (names(API_data_WQresults_FINAL)[2] != 'HTTP.Code') {
                output$map <- renderLeaflet({
                    leaflet(API_data_WQresults_FINAL) %>%
                        addTiles() %>%
                        addCircleMarkers(
                            radius = 3, opacity = 0.5,
                            popup = ~paste('<b>', 'Analyte: ', '</b>', Analyte,"<br/>",
                                           '<b>', 'Station: ', '</b>', StationName,"<br/>",
                                           '<b>', 'Sampling Agency: ', '</b>', SampleAgency,"<br/>",
                                           '<b>', 'Lab: ', '</b>', LabAgency,"<br/>",
                                           '<b>', 'Sample Date: ', '</b>', substr(SampleDate,1,10),"<br/>",
                                           '<b>', 'Result Code: ', '</b>', ResultQualCode,"<br/>",
                                           '<b>', 'Result: ', '</b>', Result, Unit),
                            clusterOptions = markerClusterOptions()
                            )
                })
            } else { 
        # If no valid data, draw an empty map, and show an error message
            output$map <- renderLeaflet({ 
                leaflet() %>% addTiles() %>% setView(lat = 38.3, lng = -119.0, zoom = 5) 
            })
                showNotification(paste0("Error: ", API_data_WQresults_FINAL$Result[1], ' (', API_data_WQresults_FINAL$API.Message[1], ')'), type = 'error')    
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

