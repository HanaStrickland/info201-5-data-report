source(" /api_key.R")

base_uri <- "https://api.propublica.org/congress/v1/"

# Get Data
response <- GET(paste0(base_uri, "bills/subjects/climate.json"), 
                add_headers("X-API-Key" = propublica_key))

climate_body <- content(response, "text")

parsed_climate_body <- fromJSON(climate_body) # turn climate_body string into a list

recent_climate_bills <- data.frame(parsed_climate_body$results, stringsAsFactors = FALSE) # Turn list into data frame

recent_climate_bills <- flatten(recent_climate_bills)
recent_climate_bills
