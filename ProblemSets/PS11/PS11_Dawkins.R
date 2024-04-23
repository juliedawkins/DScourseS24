# creating college-level data

library(tidyverse)
library(readxl)
library(zoo)
library(tidycensus)

#-------------------------------------------------------------------------------------------
# Gathering county-level estimates for # of college students
#-------------------------------------------------------------------------------------------

process_data <- function(hd_file, effy_file) {
  # Load datasets
  inst <- read_csv(hd_file)
  educdist <- read_csv(effy_file)
  
  # Filter students exclusively enrolled in distance learning
  lev_variable <- grep("LEV", names(educdist), value = TRUE)
  tot_variable <- grep("^((?!^X).)*TOT", names(educdist), value = TRUE, perl = TRUE)
  # Find the variable containing "EXC" but not starting with "X"
  exc_variable <- grep("^((?!^X).)*EXC", names(educdist), value = TRUE, perl = TRUE)
  
  # Filter students exclusively enrolled in distance learning
  educdist <- educdist %>% 
    filter(!!as.name(lev_variable) == 1) %>%
    mutate(!!exc_variable := ifelse(is.na(.data[[exc_variable]]), 0, .data[[exc_variable]])) %>% 
    group_by(UNITID) %>% 
    mutate(TOTSTUDPRES = !!as.name(tot_variable) - !!as.name(exc_variable))
  
  # Join datasets
  data <- left_join(educdist, inst, by = "UNITID")
  
  # Select relevant columns
  data <- data %>% 
    select(UNITID, INSTNM, TOTSTUDPRES, CITY, STABBR, FIPS, INSTCAT, INSTSIZE, COUNTYCD, COUNTYNM)
  data$COUNTYCD <- as.character(data$COUNTYCD)
  data$COUNTYCD <- sprintf("%05s", data$COUNTYCD)
  
  # Group by county and calculate total student count
  data <- data %>% 
    group_by(COUNTYCD) %>% 
    mutate(TOTSTUDCNT = sum(TOTSTUDPRES)) %>% 
    filter(STABBR != "PR")
  
  # Prepare final dataset
  collegedata <- data %>% 
    select(COUNTYCD, COUNTYNM, TOTSTUDCNT) %>%
    distinct()
  
  return(collegedata)
}

hd_files <- c("HD2022.ZIP", "HD2021.ZIP", "HD2020.ZIP", "HD2019.ZIP", "HD2018.ZIP")
effy_files <- c("EFFY2022_DIST.csv", "effy2021_dist_rv.csv", "effy2020_dist_rv.csv", "ef2019a_dist_rv.csv", 
                "ef2018a_dist_rv.csv")

for (i in 1:length(hd_files)) {
  result <- process_data(hd_files[i], effy_files[i])
  # Extract year from file name
  year <- substr(hd_files[i], 3, 6)
  # Assign the result to a data frame with the year label
  assign(paste0("data_", year), result)
}

filtered_data <- list()

# Loop through data frames from data_2018 to data_2022
for (year in 2018:2022) {
  # Get the data frame corresponding to the current year
  current_data <- get(paste0("data_", year))
  
  # Add a variable "year" to the data frame
  current_data$year <- year
  
  # Store the filtered data frame in the list
  filtered_data[[year - 2017]] <- current_data
}
college_data <- do.call(rbind, filtered_data)
college_data <- college_data %>% 
  mutate(COLLEGE = ifelse(TOTSTUDCNT > 0, 1, 0))

rm(list = c("data_2018", "data_2019", "data_2020", "data_2021", "data_2022", "filtered_data",
            "result", "current_data"))

examine <- college_data %>% 
  group_by(COUNTYCD) %>% 
  filter(n() < 5) %>% 
  mutate(count = n())

#-------------------------------------------------------------------------------------------
# Gathering county level housing estimates
#-------------------------------------------------------------------------------------------

# reading in all data

housing17 <- read_excel("FY2017_50_rev.xlsx") %>% 
  mutate(year = 2017, 
         statefips = sprintf("%02s", as.character(State)), 
         countyfips = sprintf("%03s", as.character(county)), 
         COUNTYCD = paste0(statefips, countyfips)) %>% 
  rename(cntyname = countyname, 
         rent50_0 = Rent50_0, 
         rent50_1 = Rent50_1, 
         rent50_2 = Rent50_2, 
         rent50_3 = Rent50_3, 
         rent50_4 = Rent50_4)
housing17$COUNTYCD <- paste0(housing17$statefips, housing17$countyfips)
housing18 <- read_excel("FY2018_50_County_rev.xlsx") %>% 
  mutate(year = 2018,
         statefips = sprintf("%02s", as.character(state)), 
         countyfips = sprintf("%03s", as.character(county)), 
         COUNTYCD = paste0(statefips, countyfips))
housing19 <- read_excel("FY2019_50_County_rev.xlsx") %>% 
  mutate(year = 2019,
         statefips = sprintf("%02s", as.character(state)), 
         countyfips = sprintf("%03s", as.character(county)), 
         COUNTYCD = paste0(statefips, countyfips))
housing20 <- read_excel("FY2020_50_County_rev.xlsx") %>% 
  mutate(year = 2020,
         statefips = sprintf("%02s", as.character(state)), 
         countyfips = sprintf("%03s", as.character(county)), 
         COUNTYCD = paste0(statefips, countyfips))
housing21 <- read_excel("FY2021_50_County.xlsx") %>% 
  mutate(year = 2021,
         statefips = sprintf("%02s", as.character(state)), 
         countyfips = sprintf("%03s", as.character(county)), 
         COUNTYCD = paste0(statefips, countyfips))
housing22 <- read_excel("FY2022_FMR_50_county_rev.xlsx") %>% 
  mutate(year = 2022,
         statefips = sprintf("%02s", as.character(state_code)), 
         countyfips = sprintf("%03s", as.character(county_code)), 
         COUNTYCD = paste0(statefips, countyfips)) %>% 
  rename(rent50_0 = rent_50_0,
         rent50_1 = rent_50_1, 
         rent50_2 = rent_50_2, 
         rent50_3 = rent_50_3, 
         rent50_4 = rent_50_4)

housing <- bind_rows(housing17, housing18, housing19, housing20, housing21, housing22) %>% 
  select(cntyname, COUNTYCD, year, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4)

housing <- housing %>% 
  group_by(COUNTYCD, year) %>% 
  mutate(rent50_0 = mean(rent50_0), 
         rent50_1 = mean(rent50_1),
         rent50_2 = mean(rent50_2), 
         rent50_3 = mean(rent50_3), 
         rent50_4 = mean(rent50_4)) %>% 
  distinct(year, COUNTYCD, .keep_all = TRUE)

rm(list = c("housing17", "housing18", "housing19", "housing20", "housing21", "housing22"))

#-------------------------------------------------------------------------------------------
# Gathering county level group housing levels
#-------------------------------------------------------------------------------------------

key <- Sys.getenv('CENSUS_API_KEY')
census_api_key(key)

for (year in 2018:2022) {
  name <- paste0("countypop", year)
  data <- get_estimates(geography = "county",
                               variables = c("GQESTIMATES"),
                               output = "wide",
                               vintage = year)
  data$year <- year
  assign(name, data)
}

countyGQ <- bind_rows(countypop2018, countypop2019, countypop2020, countypop2021, countypop2022)

rm(list = c("countypop2018", "countypop2019", "countypop2020", "countypop2021", "countypop2022", "data"))


#-------------------------------------------------------------------------------------------
# Gathering county level controls; clean all this up
#-------------------------------------------------------------------------------------------


counties <- read_csv("county_all.csv.zip")
counties <- counties %>% 
  select(FIPS, year, cntyname, stabb, pop, total_pop, median_age, median_income, pct_white, 
         pct_black, pct_hisp, pct_asian, pct_natam, pct_race2, pct_female, pct_bach_deg_plus, 
         pct_moved_in_last_yr, unemp_rate, viol_crime_rate, child_pov_rate, ypll_rate) %>% 
  filter(year > 2016)

counties <- counties %>% 
  mutate(FIPS = sprintf("%05s", as.character(FIPS)))

df <- inner_join(counties, housing, by = c("FIPS" = "COUNTYCD", "year"))

# create averages for everything. 2017-2018 = 2018, 2018-2019 = 2019, etc. 

new_df <- df %>%
  group_by(FIPS, cntyname.y) %>%
  mutate(across(
    c(pop, total_pop, median_age, median_income, pct_white, pct_black, pct_hisp, pct_asian,
      pct_natam, pct_race2, pct_female, pct_bach_deg_plus, pct_moved_in_last_yr, unemp_rate,
      viol_crime_rate, child_pov_rate, ypll_rate, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4),
    ~ (. + lag(., 1)) / 2,
    .names = "avg_{.col}"
  )) %>%
  mutate(across(
    starts_with("avg_"),
    ~ coalesce(., lag(., 1)),
    .names = "{.col}"
  )) %>%
  ungroup() %>%
  select(FIPS, cntyname.y, year, starts_with("avg_")) %>% 
  filter(year > 2017)

# sense check; sense check was good 

#-------------------------------------------------------------------------------------------
# Combining all data and adding log variables
#-------------------------------------------------------------------------------------------

combined_data <- left_join(new_df, college_data, by = c("year", "FIPS" = "COUNTYCD"))

# now need to interpolate for the counties with schools that were not in every part of my combined data
combined_data <- combined_data %>% mutate(COLLEGE = ifelse(is.na(COLLEGE), 0, 1))

# linearly interpolate where possible

combined_data <-combined_data %>%
  group_by(FIPS) %>%
  mutate(COLLEGE_ANY_YEAR = max(COLLEGE == 1, na.rm = TRUE)) %>%
  mutate(TOTSTUDCNT = if_else(COLLEGE_ANY_YEAR == 1, 
                              na.approx(TOTSTUDCNT, na.rm = FALSE),
                              TOTSTUDCNT)) %>%
  ungroup()

combined_data <- combined_data %>%
  group_by(FIPS) %>%
  mutate(
    COLLEGE_ANY_YEAR = max(COLLEGE == 1, na.rm = TRUE),
    TOTSTUDCNT = if_else(
      COLLEGE_ANY_YEAR == 1 & is.na(TOTSTUDCNT),
      mean(TOTSTUDCNT, na.rm = TRUE),
      TOTSTUDCNT
    )
  ) %>%
  ungroup()

combined_data <- left_join(combined_data, countyGQ, by = c("year", "FIPS" = "GEOID"))

combined_data <- combined_data %>% 
  mutate(logpop = log(avg_pop),
         log4br = log(avg_rent50_4), 
         log3br = log(avg_rent50_3), 
         log2br = log(avg_rent50_2), 
         log1br = log(avg_rent50_1),
         logstudpop = log(TOTSTUDCNT),
         loggq = log(GQESTIMATES)
  ) %>% 
  select(-c(COUNTYNM, NAME, COLLEGE, ))

combined_data <- combined_data %>% 
  rename(COLLEGE = COLLEGE_ANY_YEAR,
         cntyname = cntyname.y) %>% 
  mutate(TOTSTUDCNT = ifelse(is.na(TOTSTUDCNT), 0, TOTSTUDCNT), 
         PCTSTUD = TOTSTUDCNT/avg_pop)

write_csv(combined_data, "finaldata.csv")

