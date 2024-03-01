# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(jsonlite)
#-----------------------------------------------------------------
# Problem 3 - Web Scraping to get Data
#-----------------------------------------------------------------

# I am interested in gathering data about college towns and the resulting 
# rent levels and homelessness levels. This is a scrape of a simple website
# listing many "college towns". 

url <- "https://listwithclever.com/research/best-college-towns-2021/"
webpage <- read_html(url)

table <- webpage %>%
  html_nodes(css = "#post-content > div.clever-table.border-top.border-bot.col-1-center.col-2-left.col-3-left > table") %>%
  html_table(fill = TRUE)

table <- as.data.frame(table[[1]])


#----------------------------------------------------------------
# Problem 4 
#----------------------------------------------------------------

## For this problem, I am going to get API data from HUD on fair market rents for every
## county in the state of Oklahoma and California

key <- Sys.getenv("USHUD_API_KEY")
endpoint <- "https://www.huduser.gov/hudapi/public/fmr/statedata/OK"

# Make the HTTP request with the Authorization header
response <- GET(url = endpoint, add_headers("Authorization" = paste("Bearer", key)))

str(response)

json_content <- rawToChar(response$content)
parsed_data <- jsonlite::fromJSON(json_content)
metro_df <- parsed_data$data$metroareas
counties_df <- parsed_data$data$counties

# I now have data frames with the major metro areas and every county in Oklahoma, 
# allowing me to look at the Fair Market Rent prices for 1-4 bedroom places in each 
# county. Doing this in every state of interest would allow me to look at 
# the regions of interest. (FMR is the 40th percentile for every region, not the median.)

# trying for California

endpointCA <- "https://www.huduser.gov/hudapi/public/fmr/statedata/CA"

# Make the HTTP request with the Authorization header
responseCA <- GET(url = endpointCA, add_headers("Authorization" = paste("Bearer", key)))

str(responseCA)

json_contentCA <- rawToChar(responseCA$content)
parsed_dataCA <- jsonlite::fromJSON(json_contentCA)
metro_dfCA <- parsed_dataCA$data$metroareas
counties_dfCA <- parsed_dataCA$data$counties

# this code could become easily loopable if I am interested in using FMR and not 
# median, though I would need to do more work to see why i would do FMR and not median






)