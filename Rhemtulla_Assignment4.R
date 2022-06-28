# MSC2011H - ASSIGNMENT 4
# IMRAN RHEMTULLA
# JUNE , 2022

library(lubridate) # used for recognition of time strings
library(dplyr) # used for multiple functions involved in data manipulation
library(tidyr) # used for drop_na function

# Read the dataset as a dataframe

ufo <- read.csv("ufo_subset.csv", header = TRUE, sep = ",")

# Remove spaces from column names that may be present
names(ufo) <- make.names(names(ufo), unique = TRUE)

# Convert missing entries for variables into NAs
ufo$country[ufo$country == ""] <- NA
ufo$shape[ufo$shape == ""] <- NA

ufo <- ufo %>%
  # Remove rows that contain NA for country
  drop_na(country) %>%  
  # Remove rows that contain NA for shape
  drop_na(shape) %>%
  # Convert datetime and date.posted into appropriate time formats
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(date.posted = ymd(date.posted)) %>%
  # Remove rows that have been noted to be a potential hoax by the NUFORC
  filter(!(grepl("NUFORC Note", comments) & grepl("hoax", comments, ignore.case = TRUE))) %>%
  # Create a new column that reports the time between sighting and posting date in days
  mutate(report_delay = round(as.numeric(difftime(date.posted, datetime, units = "days")),0)) %>%
  # Filter report delay for reporting that occurred beforeb they were sighted
  filter(report_delay > 0)

# Create a table with average report_delay per country
country_delay <- ufo %>%
  group_by(country) %>% 
  summarise(average_delay = mean(report_delay, na.rm = TRUE))

country_delay

# Check data quality of duration(seconds column)
class(ufo$duration..seconds.)
summary(ufo$duration..seconds.)

# The range of the data is extremely large, with unrealistic sighting times (ie 0.02) that can mathematically be considered as outliers
# Removal of sightings that were less than 0.5 second or longer than 30 days to remove non-feasible sightings
ufo <- ufo %>%
  filter(duration..seconds. >= 0.5) %>%
  filter(duration..seconds. <= 2592000 )

# Create a histogram using the duration(seconds) column
hist(log(ufo$duration..seconds.), xlab = "Log(Sighting Duration in Seconds)", ylab = "Frequency", main = "Histogram of UFO Sighting Duration in Seconds")
