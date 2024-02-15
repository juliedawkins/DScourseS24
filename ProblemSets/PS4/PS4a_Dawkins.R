# loading up libraries

library(jsonlite)
library(tidyverse)

# download the data
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# view the data; it is ugly and long
system("cat dates.json")

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)
# the class of date is a character

# looking at the first 10 rows; in homework this says "n" rows, so I chose an arbitrary
# number
head(mydf, n = 10)
