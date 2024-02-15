library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"

#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11) > tbody

webpage <- read_html(url)

table <- webpage %>%
    html_nodes(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11) > tbody") %>%
    html_table(fill = TRUE)

# The table is in the first element of the list
table <- table[[1]]

table2 <- read_html("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression") %>%
  html_nodes(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17)") %>%
  html_table(fill = TRUE)

table2 <- table2[[1]]

appended_table <- bind_rows(table, table2)