# MSC2011H - ASSIGNMENT 4
# IMRAN RHEMTULLA
# JUNE , 2022

library(lubridate) # used for recognition of time strings
library(dplyr) # used for multiple functions involved in data manipulation
library(tidyr) # used for drop_na function

# Read the dataset as a dataframe

ufo <- read.csv("ufo_subset.csv", header = TRUE, sep = ",") #Good! I imported the file by clicking on it in my working directory and it automatically imported using the readr package and read_csv function (for table)

# Remove spaces from column names that may be present
names(ufo) <- make.names(names(ufo), unique = TRUE) #Great! The readr package that I used automatically removes spaces :)

# Convert missing entries for variables into NAs
ufo$country[ufo$country == ""] <- NA
ufo$shape[ufo$shape == ""] <- NA
#Excellent! I have the same code!

ufo <- ufo %>%
  # Remove rows that contain NA for country
  drop_na(country) %>%  #Good! I used filter(!is.na(country))
  # Remove rows that contain NA for shape
  drop_na(shape) %>% #Good! I used filter(!is.na(shape))
  # Convert datetime and date.posted into appropriate time formats
  mutate(datetime = ymd_hms(datetime)) %>% #wow! I didn't know there's a ymd_hms function so I used mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H%M%S"))
  mutate(date.posted = ymd(date.posted)) %>% #For this one I used mutate(date.posted = as.POSIXct(date.posted, format = "%Y-%m-%d")). 
  # Remove rows that have been noted to be a potential hoax by the NUFORC
  filter(!(grepl("NUFORC Note", comments) & grepl("hoax", comments, ignore.case = TRUE))) %>% #Didn't know we can combine conditions in filter hehe! Would it be better to put ignore.case = TRUE in the first grepl too? Just in case ;) 
  # Create a new column that reports the time between sighting and posting date in days
  mutate(report_delay = round(as.numeric(difftime(date.posted, datetime, units = "days")),0)) %>% # It is a great idea to use round() here hehe! This is how I did it: mutate(report_delay = as.Date(date.posted) - as.Date(datetime))
  # Filter report delay for reporting that occurred beforeb they were sighted
  filter(report_delay > 0) #For this condition I used filter(report_delay >= 0) because we are filtering out rows where the sighting was reported before it happened, so I added an = sign

# Create a table with average report_delay per country
country_delay <- ufo %>%
  group_by(country) %>% 
  summarise(average_delay = mean(report_delay, na.rm = TRUE)) #yay! I have the same thing!

country_delay #For some reasons the table does not show days as units, but it might not be necessary. We have the same countries but our average delays are a bit different.

# Check data quality of duration(seconds column)
class(ufo$duration..seconds.)
summary(ufo$duration..seconds.)
# I used final_solution$duration..seconds.[final_solution$duration..seconds. == ""] <- NA to make sure if there is any NA but there isn't. 
# I had to use final_solution$duration..seconds. <- as.numeric(final_solution$duration..seconds.) because my duration(seconds) was not in the numeric format

# The range of the data is extremely large, with unrealistic sighting times (ie 0.02) that can mathematically be considered as outliers
# Removal of sightings that were less than 0.5 second or longer than 30 days to remove non-feasible sightings
ufo <- ufo %>%
  filter(duration..seconds. >= 0.5) %>%
  filter(duration..seconds. <= 2592000 )
#For this one I am not quite sure about my answer, but I removed potential outliers > Q3+1.5*IQR. I also changed all numbers into decimal numbers with 2 decimal places, as shown below:
# final_solution <- final_solution %>%
#   filter(duration..seconds. <= 1432.5) %>%
#   mutate(duration..seconds. = sprintf("%1.2f", duration..seconds.))

# Create a histogram using the duration(seconds) column
hist(log(ufo$duration..seconds.), xlab = "Log(Sighting Duration in Seconds)", ylab = "Frequency", main = "Histogram of UFO Sighting Duration in Seconds")
# For this one I had to use log10 because of my data points. I also had to use as.numeric() to apply log10 to the time. 
# hist(log10(as.numeric(final_solution$duration..seconds.)), main= "Histogram of Duration of UFO Sighting",xlab = "Log10(Duration(seconds))",ylab="Frequency", xlim = c(-2, 4), ylim = c(0, 8000))
# min(log(ufo$duration..seconds.)) this is less than 0 so it would be more helpful to extend the x axis to the left :) The y-axis can also be extended so it covers the mode. You can use xlim and ylim arguments in hist()

#Overall, the code has great structure, style, logic, performance, test coverage, design, readability, and functionality. It also fulfills all requirements.




