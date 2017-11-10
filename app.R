# CEDEN Web Services Application
# This is meant to test the functionality of the web services associated with the CEDEN database and
# demonstrate how they can be used to build dynamic tools and applications that access CEDEN data 
# programatically and in real-time.

# Load Libraries
    library(shiny)
    library(tidyverse)
    library(leaflet)
    library(shinycssloaders)
    library(cedenTools) # this package loads the CEDEN query function; it can be installed with the following command: devtools::install_github('daltare/cedenTools')
    library(DT)
    library(lubridate)
    
# Load a list of CA counties
    counties_list <- read.csv('data/CA_Counties_List.csv')
    counties_list <- rbind(data.frame(County.Name = 'All Counties'), counties_list) # add an 'All Counties' option to the top of the list

# Define the user interface for the application ----
ui <- fluidPage(
   # Application title
   titlePanel("CEDEN Water Quality Data Mapper"),
   
   # Sidebar with an input boxes
   sidebarLayout(
      sidebarPanel(
         h3('Filters:'),
         textInput('parameter',
                   'Analyte:',
                   value = 'E. coli'),
         p(h6('NOTE: The following string is treated as a wildcard: /%')),
         selectInput(inputId = 'county',
                     label = 'County:',
                     choices = counties_list$County.Name,
                     multiple = TRUE,
                     selected = 'Sacramento'),
         dateRangeInput(inputId = 'date_range',label = 'Date Range:', start = '2014-01-01',end = '2014-12-31'),
         checkboxInput(inputId = 'excludeQA', label = 'Exclude QA samples', value = TRUE),
         actionButton('refresh','Update Map'),
         # Line breaks and a horizontal line
            br(),
            hr(style="border: 2px solid darkgrey"),
         # Information about the application
            h3('Application Information:'),
            p('Data Source: ', a(href = 'http://www.ceden.org/', 'California Environmental Data Exchange Network (CEDEN)')), 
            p('Data Feed:', a(href = 'https://github.com/daltare/cedenTools', 'CEDEN Web Services')),
            # hr(style="border: 1px solid darkgrey"),
            p('For more information, contact: ', a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov')),
            actionButton(inputId = 'github', label = 'Code on GitHub', icon = icon('github', class = 'fa-1x'),
                      onclick ="window.open('https://github.com/daltare/CEDEN_Viewer_Test', '_blank')")
      ),

      # Show the map
      mainPanel(
         withSpinner(leafletOutput('map', height = 700)),
         # br(),
         hr(style="border: 1px solid darkgrey"),
         dataTableOutput('tabular.data')
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
    
    # Draw a blank map initially
        output$map <- renderLeaflet({ 
            leaflet() %>% addTiles() %>% setView(lat = 38.3, lng = -119.0, zoom = 5) 
        })
        
    observeEvent(input$refresh, {
        # Cycle through all of the counties selected, and get the data that meets the query parameters
            for (i in 1:length(input$county)) {
                if ('All Counties' %in% input$county) {
                    county_input <- c('/%', 'All Counties') # the first element is for the filter string, and the second is for the progress message
                    i <- length(input$county) # set i to be the total number of counties, so only 1 query is performed and there isn't any duplicate data requested
                } else {
                    county_input <- c(input$county[i], input$county[i]) # the first element is for the filter string, and the second is for the progress message
                }
                # If excluding QA samples, create that part of the query string
                    if (input$excludeQA) {QAstring <- ',"MatrixNot":"blankwater","ProgramNot":"Associated QA","StationCodeNot":"000NONPJ"'} else {QAstring <- ''}
                # Create the filter string
                    filter_string <- paste0('"filter":[{"county":"', county_input[1],'","parameter":"', input$parameter,'","sampleDateMin":', format(input$date_range[1], '%m/%d/%Y'), '","sampleDateMax":"', format(input$date_range[2], '%m/%d/%Y'), '"', QAstring, '}]')
                # Get the data with the ceden_query function (from the cedenTools package), and use a progress indicator to let the user know that it's processing the request
                    withProgress(message = paste0('Getting Data (', input$parameter, ', ', county_input[2], ', ', input$date_range[1], ' - ',input$date_range[2], ')'), value = 1, {
                        API_data_WQresults <- ceden_query_csv(service = 'cedenwaterqualityresultslist', query_parameters = filter_string, userName = 'testInternal', password = 'p', base_URI = base_URI, errorMessages_out = TRUE)
                    })
                # check whether the query returned any results (query_status); FALSE means there's no data, TRUE means there is data
                    if (names(API_data_WQresults)[1] == 'Result' & names(API_data_WQresults)[2] == 'HTTP.Code' & names(API_data_WQresults)[3] == 'API.Message') {
                        query_status <- FALSE
                    } else {
                        query_status <- TRUE
                    }
                # if it's the first county in the loop, or if 'All counties' selected, enter that as the final result
                    if (i == 1 | county_input[1] == '/%') {
                        API_data_WQresults_FINAL <- API_data_WQresults
                    } else {
                # otherwise, append the current query results to the final set of outputs
                        # get the status of API_data_WQresults_FINAL; FALSE means there's no data, TRUE means there is data
                            if (names(API_data_WQresults_FINAL)[1] == 'Result' & names(API_data_WQresults_FINAL)[2] == 'HTTP.Code' & names(API_data_WQresults_FINAL)[3] == 'API.Message') {
                                query_status_Final <- FALSE
                            } else {
                                query_status_Final <- TRUE
                            }
                        # append the current results to the final list or overwrite it, if needed
                        if (query_status_Final == FALSE & query_status == TRUE) {
                            API_data_WQresults_FINAL <- API_data_WQresults
                        } 
                        if (query_status_Final == TRUE & query_status == TRUE) {
                            API_data_WQresults_FINAL <- bind_rows(API_data_WQresults_FINAL, API_data_WQresults)
                        }
                        # if query_status == FALSE, don't do anything (leave API_data_WQresults_FINAL as they are)
                    }
                # don't do any more queries if it's the last i, including when 'All Counties' is queried
                    if (i==length(input$county)) {
                        break()
                    }
            }
        
        # If no valid data, draw an empty map, and show an error message
            if (names(API_data_WQresults_FINAL)[1] == 'Result' & names(API_data_WQresults_FINAL)[2] == 'HTTP.Code' & names(API_data_WQresults_FINAL)[3] == 'API.Message') {
                output$map <- renderLeaflet({ 
                    leaflet() %>% addTiles() %>% setView(lat = 38.3, lng = -119.0, zoom = 5) 
                })
                showNotification(paste0("Error: ", API_data_WQresults_FINAL$Result[1], ' (', API_data_WQresults_FINAL$API.Message[1], ')'), type = 'error')    
            } else { 
        # If valid data is returned, draw the map
                # Convert dates from string to date format
                    API_data_WQresults_FINAL$SampleDate <- as.Date(API_data_WQresults_FINAL$SampleDate, format = '%m/%d/%Y')
                output$map <- renderLeaflet({
                    leaflet(API_data_WQresults_FINAL) %>%
                        addTiles() %>%
                        addCircleMarkers(
                            radius = 3, opacity = 0.5,
                            popup = ~paste('<b>', 'Analyte: ', '</b>', Analyte,'<br/>',
                                           '<b>', 'Sample ID: ', '</b>', Id, '<br/>',
                                           '<b>', 'Matrix: ', '</b>', Matrix,'<br/>',
                                           '<b>', 'Station: ', '</b>', StationName,'<br/>',
                                           '<b>', 'Station Code: ', '</b>', StationCode,'<br/>',
                                           '<b>', 'Sampling Agency: ', '</b>', SampleAgency,'<br/>',
                                           '<b>', 'Program: ', '</b>', Program,'<br/>',
                                           '<b>', 'Lab: ', '</b>', LabAgency,'<br/>',
                                           '<b>', 'Sample Date: ', '</b>', SampleDate,'<br/>',
                                           '<b>', 'Result Code: ', '</b>', ResultQualCode,'<br/>',
                                           '<b>', 'Result: ', '</b>', Result, Unit,'<br/>'),
                            clusterOptions = markerClusterOptions()
                        )
                })
                
        # Create the tabular data table
                output$tabular.data = renderDataTable(
                    API_data_WQresults_FINAL, extensions = c('Buttons', 'Scroller'),
                    options = list(dom = 'Bfrtip', buttons =
                                       list('colvis', list(
                                           extend = 'collection',
                                           buttons = list(list(extend='csv',
                                                               filename = 'cedenData'),
                                                          list(extend='excel',
                                                               filename= 'cedenData')),
                                           text = 'Download Data'
                                       )),
                                   scrollX = TRUE,
                                   scrollY = 500, scroller = TRUE, deferRender = TRUE),
                    class = 'cell-border stripe', rownames = FALSE#,
                    # server = FALSE
                    # server=TRUE
                )
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

