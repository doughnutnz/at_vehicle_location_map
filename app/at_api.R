######################################################################
# Connect to AT API and retrieve bus locations
#
# Author: Doug Hunt
# Created: 2018-09-17 15:52:55 
######################################################################

library(httr)
library(jsonlite)
library(jqr)
library(shiny)
library(dplyr)
library(leaflet)
library(assertthat)


# Global constants --------------------------------------------------------

AT_VEHICLE_LOCATIONS <- "https://api.at.govt.nz/v2/public/realtime/vehiclelocations"
AT_BUS_ROUTES <- "https://api.at.govt.nz/v2/gtfs/routes"
AT_API_ENV_VAR <- "OCP_APIM_SUBSCRIPTION_KEY"
BASE_ICON_URL <- "http://www.aeubal.co.nz/"
ICON_WIDTH = 12
ICON_HEIGHT = 12

# API key -----------------------------------------------------------------

# Return API key stored in environment variable
get_api_key <- function() {
  Sys.getenv(AT_API_ENV_VAR)
}

# Function to set up icon list to match bearing
make_icon_lst <- function(location_bearing, url_base){
  # basic checks on bearing n url params
  # assert_that(is.number(location_bearing))
  # assert_that(location_bearing <= 360)
  # assert_that(location_bearing >= -1)
  assert_that(is.character(url_base))
  
  icons(
    iconUrl = case_when(
      location_bearing < -1      ~ 
        paste(url_base,"alert-octagon.png", sep=""),
      location_bearing < 0      ~ 
          paste(url_base,"circ_only.png", sep=""),
        location_bearing <= 22.5  ~ 
          paste(url_base,"circ_up.png",sep=""), 
        location_bearing <= 67.5  ~ 
          paste(url_base,"circ_up_right.png",sep=""),
        location_bearing <= 112.5 ~ 
          paste(url_base,"circ_right.png",sep=""),
        location_bearing <= 157.5 ~ 
          paste(url_base,"circ_down_right.png",sep=""),
        location_bearing <= 202.5 ~ 
          paste(url_base,"circ_down.png",sep=""),
        location_bearing <= 247.5 ~ 
          paste(url_base,"circ_down_left.png",sep=""),
        location_bearing <= 292.5 ~ 
          paste(url_base,"circ_left.png",sep=""),
        location_bearing <= 337.5 ~ 
          paste(url_base,"circ_up_left.png",sep=""),
        location_bearing <= 360   ~ 
          paste(url_base,"circ_up.png",sep=""),
        location_bearing > 360   ~ 
          paste(url_base,"alert-octagon.png",sep="")
    ),
    iconWidth = ICON_WIDTH, iconHeight = ICON_HEIGHT,
    iconAnchorX = ICON_WIDTH / 2,
    iconAnchorY = ICON_HEIGHT /2
  )
}

# Set up our icons for the map ----------------------------------
# ArrowUpIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-up.png", sep=""),
#                           iconWidth=24,iconHeight=24,
#                           iconAnchorX=11,iconAnchorY=19)
# ArrowRtIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-right.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=04,iconAnchorY=11)
# ArrowDnIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-down.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=11,iconAnchorY=04)
# ArrowLfIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-left.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=19,iconAnchorY=11)
# ArrowUpRtIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-up-right.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=06,iconAnchorY=17)
# ArrowUpLfIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-up-left.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=17,iconAnchorY=17)
# ArrowDnRtIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-down-right.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=06,iconAnchorY=06)
# ArrowDnLfIcon <- makeIcon(iconUrl=paste(BaseIconURL, "arrow-down-left.png", sep=""),
#                         iconWidth=24,iconHeight=24,
#                         iconAnchorX=17,iconAnchorY=06)
# BusStoppedIcon <- makeIcon(iconUrl=paste(BaseIconURL, "bus-stop.png", sep=""),
#                           iconWidth=24,iconHeight=24,
#                           iconAnchorX=11,iconAnchorY=22)


# Bus locations -----------------------------------------------------------

unpack_jq <- function(s) {
  s %>% 
    as.character() %>% 
    paste(collapse = ",") %>% 
    paste("[", ., "]") %>% 
    fromJSON()
}

# Return dataframe of current snapshot of all bus positions
get_bus_locations <- function(key) {
  # Basic check on key parameter
  assert_that(is.character(key))
  
  # fetch data from API as a list so we can check URL status
  response_lst <- GET(AT_VEHICLE_LOCATIONS, add_headers(`Ocp-Apim-Subscription-Key` = key)) 
    
  # Stop if http gave unexpected status
  stop_for_status(response_lst, "get bus locations")
  #cat(response_lst$status_code)
    
  # Turn response list into text
  response <- response_lst %>% content(type = "text", encoding = "UTF-8")  
    
  # Test for Error status returned from API call
  if (response %>% 
        jq(".status") %>% 
        unpack_jq() == "Error") {
      cat("Error status from Bus Locations AT API call...\n")
      return(NA)
  }      
  else {
    
    # parse response into data.frame
    bind_cols(
      # unpack locations
      response %>% 
        jq(".response.entity[].vehicle.position") %>% 
        unpack_jq(),
  
      # unpack vehicle IDs
      response %>% 
        jq(".response.entity[].vehicle.vehicle") %>% 
        unpack_jq() %>% 
        rename(bus_id = id),
  
      # unpack trips
      response %>% 
        jq(".response.entity[].vehicle.trip") %>% 
        unpack_jq(),
  
      # unpack timestamp
      data_frame(
        timestamp = response %>% 
          jq(".response.entity[].vehicle.timestamp") %>% 
          unpack_jq() %>% 
          as.POSIXct(origin = "1970-01-01")
        ),
      
      # unpack occupancy_status
      data_frame(
        occupancy_status = response %>%
          jq(".response.entity[].vehicle.occupancy_status") %>%
          unpack_jq()
      )
    
    ) %>% 
  
      # put it all in a tibble data frame
      tbl_df() %>%
  
        # convert missing values in bearing to -1.0
        # in normal circumstances bearing should range from 0 to 360 (or 359)
        # we are going to use bearing ranges to modify our icon and so -1.0 is OK
        mutate(bearing = coalesce(bearing, "-1.0")) %>%
  
          # convert bearing to numeric
          mutate(bearing = as.numeric(as.character(bearing)))
  }
}


# Return data frame of all bus routes
get_bus_routes <- function(key) {
  # Basic check on key parameter
  assert_that(is.character(key))
  
  # fetch data from API
  response_lst <- GET(AT_BUS_ROUTES, add_headers(`Ocp-Apim-Subscription-Key` = key)) 
  
  # Stop if http gave unexpected status
  stop_for_status(response_lst, "get bus locations")
  #cat(response_lst$status_code)
  
  # Turn response list into text
  response <- response_lst %>% 
    content(type = "text", encoding = "UTF-8") 
  
  # parse response into data.frame
  response %>% 
    jq(".response[]") %>% 
    unpack_jq() %>% 
    tbl_df()
}
