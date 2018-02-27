# TWILIO FUNCTIONS FOR SHINY
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(lubridate)
library(twilio)
Sys.setenv(TWILIO_SID = "")
Sys.setenv(TWILIO_TOKEN = "")
Sys.setenv(TWILIO_NOTIFY_TOKEN = "")

# Send twilio notification
notifyTwilio <- function(message, segment) {
  
  # Set this shit up
  marketing_token <- Sys.getenv("TWILIO_NOTIFY_TOKEN")
  url <- paste0("https://notify.twilio.com/v1/Services/", marketing_token, "/Notifications")
  body <- list(
    Segment = segment
    , Tag = 'phone'
    , Body = message
  )
  authentication <- authenticate(Sys.getenv('TWILIO_SID'), Sys.getenv('TWILIO_TOKEN'))
  
  # Notifiy response
  response <- POST(url, config = authentication, body = body)
  status <- http_status(response)
  return(status$message)
}

# Get twilio segments
getTwilioSegments <- function() {
  
  # Set this shit up
  marketing_token <- Sys.getenv("TWILIO_NOTIFY_TOKEN")
  url <- paste0("https://notify.twilio.com/v1/Services/", marketing_token, "/Segments")
  authentication <- authenticate(Sys.getenv('TWILIO_SID'), Sys.getenv('TWILIO_TOKEN'))
  
  # Notifiy response
  response <- GET(url, config = authentication, body = body)
  status <- http_status(response)
  print(status$message)
  segmentsList <- content(response, "parsed")$segments
  segments <- c()
  for (segment in segmentsList) {
    segments <- c(segment$unique_name, segments)
  }
  return(segments)
}

# Get twilio users
getTwilioUsers <- function() {
  
  # Set this shit up
  marketing_token <- Sys.getenv("TWILIO_NOTIFY_TOKEN")
  url <- paste0("https://notify.twilio.com/v1/Services/", marketing_token, "/Users")
  authentication <- authenticate(Sys.getenv('TWILIO_SID'), Sys.getenv('TWILIO_TOKEN'))
  
  # Notifiy response
  response <- GET(url, config = authentication)
  status <- http_status(response)
  print(status$message)
  content <- jsonlite::fromJSON(content(response, 'text'))
  users <- content$users
  nextPage <- content$meta$next_page_url
  
  # clean data frame
  users$segmentList <- as.character(users$segments[1])
  users$bindingsLink <- users$links$bindings
  users$segments <- NULL
  users$links <- NULL
  
  #loop through
  while(!is.null(nextPage)) {
    response <- GET(nextPage, config = authentication)
    content <- jsonlite::fromJSON(content(response, 'text'))
    userdf <- content$users
    userdf$segmentList <- as.character(userdf$segments[1])
    userdf$bindingsLink <- userdf$links$bindings
    userdf$segments <- NULL
    userdf$links <- NULL
    users <- bind_rows(users, userdf)
    nextPage <- content$meta$next_page_url
  }
  
  return(users)
}

# Get twilio messages
getTwilioMessages <- function(date = NULL, from = NULL, to = NULL) {
  url <-paste0("https://api.twilio.com/2010-04-01/Accounts/", Sys.getenv("TWILIO_SID"),"/Messages.json")
  authentication <- authenticate(Sys.getenv('TWILIO_SID'), Sys.getenv('TWILIO_TOKEN'))
  response <- GET(url, config = authentication, query = list(DateSent = date, To = to, From = from))
  status <- http_status(response)
  print(status$message)
  responseContent <- jsonlite::fromJSON(content(response, 'text'))
  messages <- responseContent$messages
  messages$subresource_uris <- NULL
  if(!is.null(messages)) {
    output <- messages %>%
      mutate(
        date_created = dmy_hms(date_created)
        , date_updated = dmy_hms(date_updated)
        , date_sent    = dmy_hms(date_sent)
      ) %>%
      select(
        sid
        , date_created
        , date_updated
        , date_sent
        , to
        , from
        , body
        , status
        , num_segments
        , num_media
        , price
      )
  } else {
    output <- NULL
  }
  
  return(output)
}