library(dplyr)
library(httr)
library(jsonlite)
#install.packages(knitr)
#library(knitr)


# Get Data
source("api_key.R")

subject <- "taxes"

base_uri <- "https://api.propublica.org/congress/v1/"

resource <- "bills/search.json"

query_params <- list(query = subject)

response <- GET(paste0(base_uri, resource), query = query_params,
                add_headers("X-API-Key" = propublica_key))

subject_body <- content(response, "text")


parsed_subject_body <- fromJSON(subject_body)


recent_subject_bills <- data.frame(parsed_subject_body$results, stringsAsFactors = FALSE) # Turn list into data frame

bills <- recent_subject_bills$bills

bills <- as.data.frame((bills))

bills <- flatten(bills)


# Filter Data
subject_df <- bills %>% 
  select(bill_id, short_title, sponsor_name, sponsor_state, sponsor_party, latest_major_action, latest_major_action_date, congressdotgov_url) %>% 
  mutate(sponsor = paste(sponsor_name, sponsor_state, sponsor_party, sep = ", ")) %>% 
  select(bill_id, short_title, sponsor, latest_major_action, latest_major_action_date, congressdotgov_url) 

subject_df <- subject_df[1:10, ] # Filters for 10 most recent bills

# Time: 2:40 
