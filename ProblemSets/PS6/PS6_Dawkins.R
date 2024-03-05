## To examine/visualize data, I am going to use similar data to 
## what I did in PS5

# load in packages
library(rvest)
library(dplyr)
library(httr)
library(jsonlite)
library(fuzzyjoin)
library(stringdist)
library(ggplot2)
library(gridExtra)

#---------------------------------------------------------------------
# Downloading Data
#---------------------------------------------------------------------

# first, I am going to manually add a dataframe with major college towns. # this data is from the 
# following Bloomberg article: https://www.bloomberg.com/news/articles/2016-09-08/america-s-biggest-college-towns?embedded-checkout=true 
# and I could not for the life of me figure out how to scrape it using a webscraper or an API. 

# it is not a long table so I am going to manually put it together. 

# I manually added Norman, OK; all data is from 2016

college_town_data <- data.frame(
  College_Town = c("Norman, OK", "Ithaca, NY", "State College, PA", "Bloomington, IN", "Lawrence, KS", "Blacksburg, VA",
                   "College Station-Bryan, TX", "Columbia, MO", "Champaign-Urbana, IL", "Ann Arbor, MI", "Gainesville, FL"),
  Major_University = c("University of Oklahoma", "Cornell University", "Penn State", "Indiana University", "University of Kansas", "Virginia Tech",
                       "Texas A&M", "University of Missouri", "University of Illinois", "University of Michigan", "University of Florida"),
  Enrollment = c(27937, 33451, 47823, 44564, 29512, 45150, 60137, 41057, 51660, 76448, 58453),
  Population = c(122081, 104606, 158728, 164233, 116559, 181555, 242884, 172703, 237199, 356823, 273365),
  Percentage = c(22.9, 32.0, 30.1, 27.1, 25.3, 24.9, 24.8, 23.8, 21.8, 21.4, 21.38)
)

# generating statecode variable

college_town_data$statecode <- substr(college_town_data$College_Town, 
                                  nchar(college_town_data$College_Town) - 1, 
                                  nchar(college_town_data$College_Town))

# create key to get FMR data; I get 2017 data because it is the earliest available and 
# is closest to the 2016 time frame of the table I am using

key <- Sys.getenv("USHUD_API_KEY")

# list of states
states <- c("NY", "PA", "IN", "KS", "VA", "TX", "MO", "IL", "FL", "OK", "MI")

# Initialize an empty list to store FMR data for each state

fmr_data <- data.frame()

# Iterate over each state and fetch FMR data
for (state in states) {
  # Construct endpoint for the current state
  endpoint <- paste0("https://www.huduser.gov/hudapi/public/fmr/statedata/", state, "?year=2017")
  
  # Make the HTTP request with the Authorization header
  response <- GET(url = endpoint, add_headers("Authorization" = paste("Bearer", key)))
  
  # Parse JSON response
  parsed_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
  # Extract relevant FMR data and combine with existing data
  fmr_data <- bind_rows(fmr_data, parsed_data$data$metroareas)
}

#---------------------------------------------------------------------
# Cleaning Data
#---------------------------------------------------------------------

fmr_data$metro_name <- gsub("HUD Metro FMR Area|MSA", "", fmr_data$metro_name)
fmr_data$metro_name <- trimws(fmr_data$metro_name)

# I now have a data set with all of the major metro areas of each state of interest

# this fuzzy join will let me directly compare changes in

data <- stringdist_left_join(fmr_data, college_town_data, 
      by = c("metro_name" = "College_Town"), method = "jw", max_dist = 0.15) %>% 
      filter(if_else(is.na(College_Town), TRUE, statecode.x == statecode.y))

only_college <- data %>% 
  filter(College_Town != "NA")
not_matched <- anti_join(college_town_data, only_college)

# only 9 states are matched. this is because Norman is considered a part of the OKC
# metro area and because Virginia Tech, in Blacksburg, VA, is part of the Blacksburg-Christianburg-
# Radford metro area. Because these are more complex regional dynamics, I will exclude 
# them from analysis for now

# each state has very different incomes, so what is important is if it has on average 
# a higher rent than the average of the rest of the state

# create average rent in other metro areas excluding the college town
data <- data %>%
  group_by(statecode.x) %>%
  mutate(avg_1_bedroom_rent = ifelse(any(!is.na(College_Town)), 
                                     mean(`One-Bedroom`, na.rm = TRUE), NA_real_)) %>%
  ungroup()

# create variable that shows percentage difference between rent of the college town
# and the average 4-bedroom in the state
data <- data %>%
  # Calculate the percent difference between avg_4_bedroom_rent and Four-Bedroom when College_Town is not NA
  mutate(percent_difference_1 = ifelse(!is.na(College_Town), 
                                       ((`One-Bedroom` - avg_1_bedroom_rent) / avg_1_bedroom_rent) * 100, 
                                       NA_real_))

# isolate to the data set I want, which is just my college towns
# now, this data has information on rent state-wide and rent for the city itself

df_colleges <- data %>% 
  filter(College_Town!="NA")

#---------------------------------------------------------------------
# Visualization
#---------------------------------------------------------------------

# first, I will create a bar chart
df_colleges$color <- ifelse(df_colleges$percent_difference_1 < 0, "Lower than Avg Metro Rent", 
                                                                  "Higher than Avg Metro Rent")

#///////////////////////////////////////////////
# Plot 1: Bar Chart
#///////////////////////////////////////////////
bar_plot <- ggplot(df_colleges, aes(x = Major_University, y = percent_difference_1, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#8B0000", "#006400")) +  # Set the fill colors for below and above 0%
  labs(title = "Percent Difference in Rent for a One-Bedroom Apartment",
       x = "Major University",
       y = "Percent Difference") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(-20, 40, by = 2), labels = paste0(seq(-20, 40, by = 2), "%")) + 
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.line = element_line(color = "black")) 

ggsave("bar_plot.png", bar_plot, width = 10, height = 6, bg = "white")

#///////////////////////////////////////////////
# Plot 2: Rent vs. Enrollment Numbers
#///////////////////////////////////////////////

rent_plot <- ggplot(df_colleges, aes(x = Enrollment, y = percent_difference_1, label = Major_University)) +
  geom_point() +
  geom_text(data = subset(df_colleges, Major_University != "University of Michigan"), # UMich needs own code to label
            aes(x = Enrollment, y = percent_difference_1, label = Major_University),
            nudge_x = 5500, nudge_y = 0, size = 3) +
  geom_text(data = subset(df_colleges, Major_University == "University of Michigan"), 
            aes(x = Enrollment, y = percent_difference_1, label = Major_University),
            nudge_x = -700, nudge_y = 0, size = 3, hjust = 1) +
  labs(title = "Relationship between Enrollment and One-Bedroom Average Rent",
       x = "Enrollment (Total # of Students)",
       y = "Percent Above or Below State Average Rent") +
  scale_y_continuous(breaks = seq(-20, 40, by = 2), labels = paste0(seq(-20, 40, by = 2), "%")) + 
  scale_x_continuous(breaks = seq(25000, 80000, by = 5000), labels = paste0(seq(25000, 80000, by = 5000))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),  # Remove grid lines
        axis.line = element_line(color = "black"))  # Make x and y axes dark

ggsave("enrollment-rent.png", rent_plot, width = 10, height = 6, bg = "white")

#///////////////////////////////////////////////
# Plot 3: Rent vs. Student Population Percentage
#///////////////////////////////////////////////

percent_plot <- ggplot(df_colleges, aes(x = Percentage, y = percent_difference_1, label = Major_University)) +
  geom_point() +
  geom_text(nudge_x = 0.4, nudge_y = 0.8, size = 3) +
  labs(title = "Relationship between Student % of Population and One-Bedroom Average Rent",
       x = "Percentage of Population",
       y = "Percent Above or Below State Average Rent") +
  scale_y_continuous(breaks = seq(-20, 40, by = 2), labels = paste0(seq(-20, 40, by = 2), "%")) +
  scale_x_continuous(breaks = seq(20, 40, by = 2), labels = paste0(seq(20, 40, by = 2), "%")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),  # Remove grid lines
        axis.line = element_line(color = "black"))  

ggsave("percentage-rent.png", percent_plot, width = 10, height = 6, bg = "white")
