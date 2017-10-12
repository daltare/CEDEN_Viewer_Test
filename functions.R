# Query Function
ceden_query <- function(service, query_parameters, base_URI = 'https://testcedenwebservices.waterboards.ca.gov:9267', userName = '', password = '') {
    
    # Check to see if the user has entered a username and password with the function. If not, get it from the user's environment variables.
        if (userName == '') {
            userName <- Sys.getenv('ceden_userName')
        }
        if (password == '') {
            password <- Sys.getenv('ceden_password')
        }
    
    # Authorization (send a POST request with the username and password)
        auth_Request <- paste0(base_URI, '/Auth/?provider=credentials&userName=', userName, '&password=', password) # build the string for the request
        auth_Response <- POST(auth_Request) # send the request
        if(auth_Response$status_code != 200) { # Make sure the authentication was successful. If not, stop the function, and report the HTTP errror code to the user.
            stop(paste0('Authentication not successful. HTTP error code: ', auth_Response$status_code))
        }
    
    # Query (send a GET request with the relevant parameters)
        query_formatted <- url_encode(paste0('{', query_parameters, '}')) # encode the query parameters into a format suitable for HTTP
        query_URI <- paste0(base_URI,'/', service, '/?queryParams=', query_formatted) # build the string for the request
        query_Response <- GET(query_URI) # send the request
        if(query_Response$status_code != 200) { # Make sure the query was successful. If not, stop the function, and return the HTTP error code to the user.
            stop(paste0('query not successful. HTTP error code: ', query_Response$status_code))
        }
    
    # Convert the results of the request from JSON into a readable format, and format it to an R dataframe
        query_Char <- rawToChar(query_Response$content)
        query_Content <- fromJSON(query_Char)
        query_Results <- query_Content$queryResults
        if (identical(query_Results, list())) { # check to see whether the query returned any data
            query_Results <- 'No Data'
        } else {
            query_Results <- query_Results %>% select(-metadata) # Drop the metadata columns that are included in the data
        }
        return(query_Results) # output the resulting dataframe
}
