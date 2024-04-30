# creating college-level data

library(tidyverse)
library(readxl)
library(zoo)
library(tidycensus)
library(modelsummary)
setwd("~/Desktop/data")

###############################################################

# INITIAL IMPORTING AND CLEANING OF ALL DATA

###############################################################


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
college_data$year <- as.character(college_data$year)

rm(list = c("data_2018", "data_2019", "data_2020", "data_2021", "data_2022", "filtered_data",
            "result", "current_data"))

#-------------------------------------------------------------------------------------------
# Gathering county level median rent
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

rent <- bind_rows(housing17, housing18, housing19, housing20, housing21, housing22) %>% 
  select(cntyname, COUNTYCD, year, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4)

rent <- rent %>% 
  group_by(COUNTYCD, year) %>% 
  mutate(rent50_0 = mean(rent50_0), 
         rent50_1 = mean(rent50_1),
         rent50_2 = mean(rent50_2), 
         rent50_3 = mean(rent50_3), 
         rent50_4 = mean(rent50_4)) %>% 
  distinct(year, COUNTYCD, .keep_all = TRUE)

rm(list = c("housing17", "housing18", "housing19", "housing20", "housing21", "housing22"))
rent$year <- as.character(rent$year)

#-------------------------------------------------------------------------------------------
# Getting square milage data 
#-------------------------------------------------------------------------------------------

sqmi <- read.delim("2023_Gaz_counties_national.txt")
sqmi <- sqmi[c("GEOID", "NAME","ALAND_SQMI")]
sqmi$GEOID <- sprintf("%05s", as.character(sqmi$GEOID))

#-------------------------------------------------------------------------------------------
# Getting housing unit data 
#-------------------------------------------------------------------------------------------
# creating fips to convert into fips codes 
state_fips <- data.frame(
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  fips_code = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", 
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", 
                "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", 
                "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
)

housing10 <- read_excel("CO-EST2019-ANNHU.xlsx")
names(housing10) <- c("area", "census", "estimatesbase", "2010", "2011", "2012", "2013", "2014", 
                      "2015", "2016", "2017", "2018", "2019")
housing10 <- housing10 %>% 
  select(area, `2017`, `2018`, `2019`)
housing10 <- housing10 %>% 
  mutate(area = gsub("\\.", "", area)) %>% 
  na.omit()
housing10 <- housing10 %>%
  separate(area, into = c("county", "state"), sep = ",\\s*", remove = FALSE)
housing10 <- housing10 %>%
  left_join(state_fips, by = c("state" = "state_name"))
housing_long <- housing10 %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "year", 
               values_to = "housing_units")

housing20 <- read_excel("CO-EST2022-HU.xlsx")
names(housing20) <- c("area", "2020est", "2020", "2021", "2022")
housing20 <- housing20 %>% 
  mutate(area = gsub("\\.", "", area)) %>% 
  select(-`2020est`) %>% 
  mutate(`2020` = as.numeric(`2020`)) %>% 
  na.omit()
housing20 <- housing20 %>%
  separate(area, into = c("county", "state"), sep = ",\\s*", remove = FALSE)
housing20 <- housing20 %>%
  left_join(state_fips, by = c("state" = "state_name"))
housing_long2 <- housing20 %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "year", 
               values_to = "housing_units")

housingunits <- bind_rows(housing_long, housing_long2)
housingunits$county <- trimws(housingunits$county)
# all of the below changes to county names are necessary for later merging
housingunits <- housingunits %>% 
  mutate(county = gsub("St ", "St. ", county), 
         county = gsub("Ste ", "Ste. ", county),
         county = ifelse(county == "Doña Ana County", "Dona Ana County", county),
         county = ifelse(county == "Petersburg Borough", "Petersburg Census Area", county), 
         county = ifelse(county == "LaSalle County", "La Salle County", county), 
         county = ifelse(county == "LaSalle Parish", "La Salle Parish", county),
         fips_code = ifelse(county == "District of Columbia", "11", fips_code))

rm(list = c("housing10", "housing20", "housing_long", "housing_long2", "state_fips"))

#-------------------------------------------------------------------------------------------
# Gathering county level controls by binding county data, housing units, group quarter estimates, 
# square milage, and housing prices
#-------------------------------------------------------------------------------------------

# read in counties 
counties <- read_csv("county_all.csv.zip")
counties <- counties %>% 
  select(FIPS, year, cntyname, stabb, pop, total_pop, median_age, median_income, pct_white, 
         pct_black, pct_hisp, pct_asian, pct_natam, pct_race2, pct_female, pct_bach_deg_plus, 
         pct_moved_in_last_yr, unemp_rate, viol_crime_rate, child_pov_rate, ypll_rate) %>% 
  filter(year > 2016)

counties <- counties %>% 
  mutate(FIPS = sprintf("%05s", as.character(FIPS)))
counties <- counties %>% 
  mutate(fips_code = substr(FIPS, 1, 2), 
         year = as.character(year))


# join counties and housing prices
df1 <- inner_join(counties, rent, by = c("FIPS" = "COUNTYCD", "year"))
df1 <- df1 %>% 
  mutate(cntyname.y = ifelse(cntyname.y == "Petersburg Borough", "Petersburg Census Area", cntyname.y))


# join counties, housing prices, and numbers of units
df <- left_join(df1, housingunits, by = c("cntyname.y" = "county", "fips_code", "year"))
# do mean interpolation for the 11 or so counties that didn't match in 2020-2022
df <- df %>% 
  group_by(FIPS) %>% 
  mutate(
    housing_units = ifelse(is.na(housing_units), mean(housing_units, na.rm = TRUE), housing_units)
  )

# lastly, join this data frame with information on square mileage to get avg population density
df <- left_join(df, sqmi, by = c("FIPS" = "GEOID"))

# this code does two things. in the first section, it averages across all values because i want
# 2018 to actually be the average of 2017 and 2018 because 2018 for the college data represents the 
# 2017-2018 school year. therefore, 2018, 2019, 2020, & 2021 all become averages for the year and the year 
# prior. in the second mutate function, it fills in all of the "NA"s for 2022 by taking the value of 2021. 
# this may have errors but it solely effects the list of controls so I am not terribly concerned about 
# it

new_df <- df %>%
  group_by(FIPS, cntyname.y) %>%
  mutate(across(
    c(pop, total_pop, median_age, median_income, pct_white, pct_black, pct_hisp, pct_asian,
      pct_natam, pct_race2, pct_female, pct_bach_deg_plus, pct_moved_in_last_yr, unemp_rate,
      viol_crime_rate, child_pov_rate, ypll_rate, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4,
      housing_units, ALAND_SQMI),
    ~ (. + lag(., 1)) / 2
  )) %>%
  mutate(across(
    c(pop, total_pop, median_age, median_income, pct_white, pct_black, pct_hisp, pct_asian,
      pct_natam, pct_race2, pct_female, pct_bach_deg_plus, pct_moved_in_last_yr, unemp_rate,
      viol_crime_rate, child_pov_rate, ypll_rate, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4,
      housing_units, ALAND_SQMI),
    ~ coalesce(., lag(., 1))
  )) %>%
  ungroup() %>%
  select(FIPS, cntyname.y, year, everything()) %>%
  filter(year > 2017)

# sense check; sense check was good 

# lastly, I am going to clean this up by removing unnecessary columns and creating my population density variable
new_df <- new_df %>% 
  mutate(popdens = pop/ALAND_SQMI)
new_df <- new_df %>% 
  select(FIPS, year, cntyname.y, stabb, pop, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4, 
         housing_units, median_age, median_income, pct_white, pct_black, pct_hisp, 
         pct_asian, pct_natam, pct_race2, pct_female, pct_bach_deg_plus, pct_moved_in_last_yr, 
         unemp_rate, viol_crime_rate, child_pov_rate, popdens, ypll_rate)

#-------------------------------------------------------------------------------------------
# Combining college data with controls and adding log variables
#-------------------------------------------------------------------------------------------

combined_data <- left_join(new_df, college_data, by = c("year", "FIPS" = "COUNTYCD"))

# now need to interpolate for the counties with schools that were not in every part of my combined data
combined_data <- combined_data %>%  
  mutate(COLLEGE = ifelse(is.na(TOTSTUDCNT) | TOTSTUDCNT == 0, 0, COLLEGE))
    
# linearly interpolate where possible

combined_data <-combined_data %>%
  group_by(FIPS) %>%
  mutate(COLLEGE_ANY_YEAR = max(COLLEGE == 1, na.rm = TRUE)) %>%
  mutate(TOTSTUDCNT = if_else(COLLEGE_ANY_YEAR == 1, 
                              na.approx(TOTSTUDCNT, na.rm = FALSE),
                              TOTSTUDCNT)) %>%
  ungroup()

# fill in with mean where linear interpolation is not possible

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

combined_data <- combined_data %>% 
  mutate(logpop = log(pop),
         log4br = log(rent50_4), 
         log3br = log(rent50_3), 
         log2br = log(rent50_2), 
         log1br = log(rent50_1),
         logstudpop = log(TOTSTUDCNT),
         logunits = log(housing_units),
         totstudcnt = ifelse(is.na(TOTSTUDCNT), 0, TOTSTUDCNT),
         pctstud = TOTSTUDCNT/pop,
         pctstud = ifelse(is.na(pctstud), 0, pctstud)
  ) %>% 
  select(-c(COUNTYNM,COLLEGE, TOTSTUDCNT)) %>% 
  rename(college = COLLEGE_ANY_YEAR, 
         county = cntyname.y)

combined_data <- combined_data %>%
  group_by(FIPS) %>%
  mutate(collegecounty = ifelse(any(pctstud > 0.1), 1, 0)) %>%
  ungroup()

test <- combined_data %>% 
  filter(is.na(pop))

# i have like five regions where i have majority nas, so I am going to remove them
combined_data <- combined_data %>% 
  filter(!is.na(pop))

write_csv(combined_data, "finaldata.csv")


###############################################################

# CREATING SOME TABLES & VISUALS

###############################################################
df <- read_csv("~/Desktop/data/data/finaldata.csv")

# summary statistics table
df %>% 
  select(FIPS, pop, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4, housing_units, 
         totstudcnt, collegecounty, pctstud) %>% 
  datasummary_skim()

df %>% 
  select(FIPS, pop, rent50_0, rent50_1, rent50_2, rent50_3, rent50_4, housing_units, 
         totstudcnt, collegecounty, pctstud) %>% 
  summary()

df %>% 
  group_by(college == 1) %>% 
  summarise(mean = mean(totstudcnt)) %>% 
  print()

  
table <- df %>%
  group_by(college) %>% 
  summarise(Median_Rent_Avg = mean(rent50_4))
print(table)

ggplot(data = data.frame(df$rent50_4), aes(x = rent50_4)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of rent50_4",
       x = "rent50_4",
       y = "Count") +
  theme_minimal()

# lastly, create a visual that shows the trend for average % diff above or below the 
# average rent in the rest of the state

df <- df %>%
  group_by(stabb, year) %>%
  mutate(avg4br = mean(rent50_4[collegecounty == 0], na.rm = TRUE)) # Calculate the mean for non-college counties

df <- df %>% mutate(pctdiff = if_else(
  condition = collegecounty == 1,
  true = (rent50_4 - avg4br) / avg4br * 100,
  false = NA_real_
)) 

# exclusively looking at college counties, i.e., colleges whose pop is
# > 10% students

dfcol <- df %>% 
  filter(!(is.na(pctdiff)))

totstud <- ggplot(data = dfcol, aes(x = totstudcnt, y = pctdiff)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  labs(
       x = "Total Student Count",
       y = "Percentage Difference") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white"),
    text = element_text(family = "Times New Roman")
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave("totstud.png", plot = totstud, width = 6, height = 6, bg = "white")


pctstud <- ggplot(data = dfcol, aes(x = pctstud, y = pctdiff)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  labs(
       x = "Students as % of Total Population",
       y = "Percentage Difference") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white"),
    text = element_text(family = "Times New Roman")
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave("pctstud.png", plot = pctstud, width = 6, height = 6, bg = "white")


ggplot(data = dfcol, aes(x = pctstud, y = pctdiff)) + 
  geom_point()

ggplot(data = dfcol, aes(x = totstudcnt, y = rent50_4)) + 
  geom_point()


###############################################################

# RUNNING MODELS

###############################################################
library(plm)

df <- read_csv("finaldata.csv")

# i do get 5 observations where logstudpop is -inf because the number of college students is 0.
# i am going to fix those

df <- df %>% 
  mutate(logstudpop = ifelse(!is.finite(logstudpop), NA_real_, logstudpop), 
        college = ifelse(totstudcnt == 0, 0, 1))

# isolating to just college counties to see how factors affect it
collegeonly <- df %>% 
  filter(college == 1)

# isolating to "college counties" 

collegecounties <- df %>% 
  filter(collegecounty == 1)

# pooled OLS models
# model 1: being a college county

est1 <- lm(log4br ~ college + logpop + logunits + popdens + median_income + 
             unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
             pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + as.factor(year) + as.factor(stabb), 
           data = df)
summary(est1)

# model 2: looking at college counties; >10%

est2 <- lm(log4br ~ collegecounty + logpop + logunits + popdens + median_income + 
             unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
             pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + as.factor(year) + as.factor(stabb), 
           data = df)
summary(est2)

est3 <- lm(log4br ~ logstudpop + pctstud + logpop + logunits + popdens + median_income + 
             unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
             pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + as.factor(year) + as.factor(stabb), 
           data = collegeonly)
summary(est3)

est4 <- lm(log4br ~ logstudpop + pctstud + logpop + logunits + popdens + median_income + 
                     unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
                     pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + as.factor(year) + as.factor(stabb), 
                   data = collegecounties)
summary(est4)

modelsummary(list("Model 1" = est1, "Model 2" = est2, "Model 3 (Any Students)" = 
                    est3, "Model 4 (10% Student Counties" = est4), stars = TRUE, output = "latex")

# fixed effects models
# keeping all data
est5 <- plm(log4br ~ totstudcnt + pctstud + logpop + logunits + popdens + median_income + 
              unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
              pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + factor(year),
             index = "FIPS", model = "within", data = df)
summary(est5)


est6 <- plm(log4br ~ logstudpop + pctstud + logpop + logunits + popdens + median_income + 
              unemp_rate + median_age + viol_crime_rate + pct_moved_in_last_yr + 
              pct_bach_deg_plus + pct_white + pct_black + pct_hisp + pct_asian + factor(year), 
            index= "FIPS", model = "within", data = collegeonly)
summary(est6)


est7 <- plm(log4br ~ logstudpop + pctstud + logunits + logpop + popdens + median_income + unemp_rate + 
              median_age + viol_crime_rate + pct_moved_in_last_yr + 
              pct_bach_deg_plus + pct_white + pct_black + pct_hisp + 
              pct_asian + pct_moved_in_last_yr + factor(year),
             index = "FIPS", model = "within", data = collegecounties)
summary(est7)

modelsummary(list("Model 5 (All Counties)" = est5, "Model 6 (Any Students)" = est6, 
                  "Model 7 (10% Student Counties)" = est7), stars = TRUE, output = "latex")

