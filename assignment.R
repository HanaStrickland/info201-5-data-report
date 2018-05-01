### Get Bills ###

source("api_key.R") # source api_key

# package base uri into a a function for use multiple times
base_uri <- function() {
  get_base_uri <- "https://api.propublica.org/congress/v1/"
  get_base_uri
}

# Function gets data frame of all recent bills for chosen topic
bills_on_topic <- function() { 

  resource_topic <- "bills/search.json"

  query_params_topic <- list(query = topic) 

  response_topic <- GET(paste0(base_uri(), resource_topic),
    query = query_params_topic,
    add_headers("X-API-Key" = propublica_key)
  )

  topic_body <- content(response_topic, "text")

  parsed_topic_body <- fromJSON(topic_body)

  recent_topic_bills <- data.frame(parsed_topic_body$results, stringsAsFactors = FALSE) 

  bills <- recent_topic_bills$bills

  bills <- as.data.frame((bills))

  bills <- flatten(bills)

  bills
}

df_bills_on_topic <- bills_on_topic() 

# Function gets select columns for the ten most recent bills on chosen topic from above
bills_select_col <- function() {

  topic_df <- df_bills_on_topic %>%
    select(bill_id, short_title, sponsor_name, sponsor_state, sponsor_party, cosponsors, latest_major_action, congressdotgov_url) %>%
    mutate(sponsor = paste(sponsor_name, sponsor_state, sponsor_party, cosponsors, sep = ", ")) %>%
    select(bill_id, short_title, sponsor, latest_major_action, congressdotgov_url)

  topic_df <- topic_df[1:10, ] # Filters for 10 most recent bills, Probublica default is chronological in decending order
}

bills_select_df <- bills_select_col()

### Get Representative Contact Info ###

#Function api response, body, and parsed_body into a function for multiple use
get_parsed_body <- function(resource) {

  response <- GET(
    paste0(base_uri(), resource),
    add_headers("X-API-Key" = propublica_key))

  body <- content(response, "text")

  parsed_body <- fromJSON(body)
  parsed_body

}

get_rep_info <- function() {
  resource_rep <- paste0("members/", representative_id, ".json")

  parsed_member_body <- get_parsed_body(resource_rep)

  member <- as.data.frame(parsed_member_body$results)

  member <- flatten(member)
  member

}

member <- get_rep_info()




# Function gets more contact info nested in roles column 
get_rep_roles <- function(){
  
  member
  roles_df <- as.data.frame(member$roles)
  roles_df
}

rep_roles_df <- get_rep_roles()

### Get Bills sponsored by rep ###

resource_sponsored <- paste0("members/", representative_id, "/bills/introduced.json")

parsed_sponsored_body <- get_parsed_body(resource_sponsored)

sponsored_bills <- as.data.frame(parsed_sponsored_body)

sponsored_bills <- flatten(sponsored_bills)

sponsored_bills <- as.data.frame(sponsored_bills$results.bills)

recent_sponsored_bills <- sponsored_bills[1:10, ]

recent_sponsored_bills <-
  recent_sponsored_bills %>%
  select(bill_id, short_title)

### Get bills cosponsored by rep ###

cosponsored_resource <- paste0("members/", representative_id, "/bills/cosponsored.json")

parsed_cosponsored_body <- get_parsed_body(cosponsored_resource)

cosponsored_bills <- as.data.frame(parsed_cosponsored_body)

cosponsored_bills <- flatten(cosponsored_bills)

cosponsored_bills <- as.data.frame(cosponsored_bills$results.bills)

recent_cosponsored_bills <- cosponsored_bills[1:10, ]

recent_cosponsored_bills <-
  recent_cosponsored_bills %>%
  select(bill_id, short_title)

## merge recent sponsored and cosponsored bills
sponsored_and_cosponsored <- full_join(recent_sponsored_bills, recent_cosponsored_bills, by = c("bill_id" = "bill_id", "short_title" = "short_title"))

# Voted with majority of other party

### Get all recent votes ###


resource_recent_votes <- "house/votes/recent.json"

parsed_recent_votes_body <- get_parsed_body(resource_recent_votes)

recent_votes <- as.data.frame(parsed_recent_votes_body$results)

recent_votes <- flatten(recent_votes)

select_bills_majority_republican <-
  recent_votes %>%
  select(votes.roll_call, votes.bill.title, votes.republican.majority_position)

## Get chosen rep's voting record
resource_rep_votes <- paste0("members/", representative_id, "/votes.json")

parsed_rep_votes_body <- get_parsed_body(resource_rep_votes)

rep_votes <- as.data.frame(parsed_rep_votes_body$results)

rep_votes <- flatten(rep_votes)

rep_votes_df <- as.data.frame(rep_votes$votes)

rep_votes_df <- flatten(rep_votes_df)

rep_votes_df$roll_call <- as.integer(rep_votes_df$roll_call) # Need to convert into integer to match recent_votes


select_rep_votes <-
  rep_votes_df %>%
  select(roll_call, bill.title, position)

## Merge all votes and rep votes

compare_voting_record <- left_join(select_rep_votes, select_bills_majority_republican, by = c("roll_call" = "votes.roll_call"))

filter_yes_republican_majority_position <-
  compare_voting_record %>%
  filter(votes.republican.majority_position == "Yes")

num_yes_republican_majority_position <- nrow(filter_yes_republican_majority_position)

filter_yes_rep_vote <-
  filter_yes_republican_majority_position %>%
  filter(position == "Yes")

num_yes_rep_vote <- nrow(filter_yes_rep_vote)

pct_rep_vote_with_other_party <- num_yes_rep_vote / num_yes_republican_majority_position

pct_rep_vote_with_other_party <- round(100 * pct_rep_vote_with_other_party) # in percentage

