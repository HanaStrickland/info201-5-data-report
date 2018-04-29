
# Get Bills on Topic
source("api_key.R")

topic <- "taxation"

base_uri <- "https://api.propublica.org/congress/v1/"

resource_topic <- "bills/search.json"

query_params_topic <- list(query = topic)

response_topic <- GET(paste0(base_uri, resource_topic), query = query_params_topic,
                add_headers("X-API-Key" = propublica_key))

topic_body <- content(response_topic, "text")


parsed_topic_body <- fromJSON(topic_body)


recent_topic_bills <- data.frame(parsed_topic_body$results, stringsAsFactors = FALSE) # Turn list into data frame

bills <- recent_topic_bills$bills

bills <- as.data.frame((bills))

bills <- flatten(bills)

## Filter Data
topic_df <- bills %>% 
  select(bill_id, short_title, sponsor_name, sponsor_state, sponsor_party, cosponsors, latest_major_action, congressdotgov_url) %>% 
  mutate(sponsor = paste(sponsor_name, sponsor_state, sponsor_party, cosponsors , sep = ", ")) %>%
  select(bill_id, short_title, sponsor, latest_major_action, congressdotgov_url) 

topic_df <- topic_df[1:10, ] # Filters for 10 most recent bills, Probublica default is chronological in decending order

# Get Representative Contact Info
representative_id <- "S000510"

resource_rep <- paste0("members/", representative_id, ".json")

response_rep <- GET(paste0(base_uri, resource_rep), 
                add_headers('X-API-Key' = propublica_key)) 

member_body <- content(response_rep, "text")

parsed_member_body <- fromJSON(member_body)

member <- as.data.frame(parsed_member_body$results)

member <- flatten(member)

roles_df <- as.data.frame(member$roles)

# Get Bills sponsored by rep

resource_sponsored <- paste0("members/", representative_id , "/bills/introduced.json")

response_sponsored <- GET(paste0(base_uri, resource_sponsored), 
                          add_headers('X-API-Key' = propublica_key)) 

sponsored_body <- content(response_sponsored, "text")

parsed_sponsored_body <- fromJSON(sponsored_body)

sponsored_bills <- as.data.frame(parsed_sponsored_body)

sponsored_bills <- flatten(sponsored_bills)

sponsored_bills <- as.data.frame(sponsored_bills$results.bills)

recent_sponsored_bills <- sponsored_bills[1:10, ]

recent_sponsored_bills <- 
  recent_sponsored_bills %>% 
  select(bill_id, short_title)

# Get bills cosponsored by rep
source("api_key.R")

cosponsored_resource <- paste0("members/",representative_id , "/bills/cosponsored.json")

response_cosponsored <- GET(paste0(base_uri, cosponsored_resource), 
                          add_headers('X-API-Key' = propublica_key)) 

cosponsored_body <- content(response_cosponsored, "text")

parsed_cosponsored_body <- fromJSON(cosponsored_body)

cosponsored_bills <- as.data.frame(parsed_cosponsored_body)

cosponsored_bills <- flatten(cosponsored_bills)

cosponsored_bills <- as.data.frame((cosponsored_bills$results.bills))

recent_cosponsored_bills <-  cosponsored_bills[1:10, ]

recent_cosponsored_bills <- 
  recent_cosponsored_bills %>% 
  select(bill_id, short_title)

# merge recent sponsored and cosponsored bills
sponsored_and_cosponsored <- full_join(recent_sponsored_bills, recent_cosponsored_bills, by = c("bill_id" = "bill_id", "short_title" = "short_title"))

