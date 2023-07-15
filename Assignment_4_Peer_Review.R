Version: 1.0 

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

# Assignment 4
# R Coding
# Caroline Rodrigues

# Peer Review by: Alanna Olteanu

# Save ufo data file to your working directory
# Visually inspect and compare your data frame to the original csv 
ufo.data <- read.csv("ufo_subset.csv")
View(ufo.data)

# AO: This is the appropriate code to import the data to RStudio.

# Find rows where shape information is missing and enter "unknown"
missing_shape <- which(ufo.data$shape =="")
ufo.data$shape[missing_shape] <- "unknown"
# Check to see if this worked
View(ufo.data)
# This worked! 

# AO: This works to replace the NA values with "unknown". 
# AO: An alternate code could be to use the mutate and ifelse function which looks like:
ufo_data <- ufo_subset %>%
  mutate(shape = ifelse(shape == "", "unknown", shape))
View(ufo_modified)

# Remove the rows that do not have Country information
ufo.data1 <- ufo.data[!ufo.data$country == "", ]
View(ufo.data1)

# AO: This is also an efficient code which allows the rows without Country information to be remouved. Another alternative is:
ufo_data1 <- ufo_data %>%
  filter(!country == "")
View(ufo_data1)
# AO: Using the filter function accomplishes the same goal.

# Convert Datetime and Date_posted columns into appropriate formats
# I have chosen to keep formats consistent to Y-m-d
ufo.data2 <- ufo.data1
ufo.data2$date_posted <- as.Date(ufo.data1$date_posted, format = "%d-%m-%Y")
View(ufo.data2)

# AO: This is a great! It is very simple and accomplishes the goal of changing the format of the date.

# Identify possible hoax reports
ufo.data3 <- ufo.data2
# Create a new boolean column "is_hoax"
# Populate this column with TRUE if the sighting is a possible hoax, FALSE otherwise
ufo.data3$is_Hoax <- grepl("\\bhoax\\b", ufo.data2$comment, ignore.case = TRUE)
View(ufo.data3)

# AO: While the existing comments explain the purpose of creating the is_Hoax column, it would be helpful to provide additional comments to explain the regular expression pattern \\bhoax\\b and its role in identifying possible hoaxes.
# AO: Another alternate to the code above is: 
# Creating a new column "is_hoax" and initialize with FALSE
ufo_data3 <- ufo_data2 %>%
  mutate(is_hoax = FALSE)
# Defining keywords indicating possible hoax reports
hoax_keywords <- c("fake", "hoax", "prank", "fabricated", "fraud")
# Filtering the rows based on comment keywords and update "is_hoax" column
ufo_data3$is_hoax <- grepl(paste0("\\b(", paste(hoax_keywords, collapse = "|"), ")\\b"), ufo_modified$comments, ignore.case = TRUE)
View(ufo_data3)

# To create a table finding hoax per country
# First, find total number of hoax. This will act as our denominator
# Load the dplyr package if you haven't already
library(dplyr)
hoax_total <- ufo.data3 %>%
  filter(is_Hoax == TRUE) %>%
  summarize(total = n())

# AO: Instead of importing the dplyr package here, it could have been done at the beginning of the code for simplicity.

# Use pipes to simplify navigating through different groups
percent_hoax_per_country <- ufo.data3 %>%
  group_by(country) %>% # group each country together to identify how many hoax / country
  summarize(hoax_count = sum(is_Hoax == TRUE)) %>%
  mutate(percent_hoax = hoax_count / hoax_total$total * 100) # divide hoax count per country by the total amount of hoaxes
# View the table reporting hoax per country
View(percent_hoax_per_country)

# AO: Great table! Used pipes correctly to see the number of hoax per country.

# Add another column to the dataset (report_delay) 
# populate with the time difference in days, between the date of the sighting and the date it was reported.
# I do this by subtracting 
ufo.data4 <- ufo.data3
ufo.data4$report_delay <- as.numeric(difftime(ufo.data3$date_posted, ufo.data3$datetime, units = "days"))
# Populate this information into a table
View(ufo.data4)

# AO: Code above works as intended!

# Remove the rows where the sighting was reported before it happened.
ufo.data5 <- ufo.data4[!ufo.data4$report_delay <0, ] # to do this, I can remove any negative value acquired in the difference of dates
View(ufo.data5)

# AO: Again, great code and works well! Another alternative is using the %>% as shown below:
ufo_data5 <- ufo_data4 %>%
  filter(report_delay >= 0)
View(ufo_data5)

# Report average delay per country
# To do this we will need some functions from the dplyr package. 
# If you do not already have this loaded, use the function below
library(dplyr)

# AO: Not necessary to load dplyr twice.

country_delay <- ufo.data5 %>%
  group_by(country) %>%
  summarize(report_delay = mean(report_delay))

# Populate this information into a table
View(country_delay)

# AO: Table works as intended and shows the average report_delay. Great job!

# Review and edit the duration seconds column

# Formatting is different between identical values (ie. 2 vs 2.0)
# To fix, I made sure all values are reported to 2 decimal places
ufo.data5$duration.seconds <- format(ufo.data5$duration.seconds, nsmall = 2)
View(ufo.data5)

# AO: When formatting the data, I would look a more variables than decimals. It is also important to check for missingness of data, and perhaps implementing a more realistic range. 
# AO: This can look like:
# AO: Checking for NA by using the sum function
missing_values <- sum(is.na(ufo_data5$duration.seconds))
missing_values
ufo_data5 <- ufo_modified[!ufo_data5$duration.seconds == "", ]
# AO: Checking range and removing numbers that are not within it
ufo_data5 <- ufo_modified[ufo_data5$duration.seconds >= 10 & ufo_data5$duration.seconds <= 1000, ]
View(ufo_data5)

# Create a histogram using the duration seconds column
# First, I make sure the column is numeric
ufo.data6 <- ufo.data5
duration_numeric <- (as.numeric(ufo.data6$duration.seconds))

# AO: Very important step listed above! Great catch.

# Create histogram and add title 
hist(duration_numeric, main = paste("Duration of UFO Sitings"))

# AO: When creating a histogram with the hist function, it is a good practice to set appropriate labels for the axes (x-axis and y-axis) to provide better context for the data being plotted.

# add 'log' base 10 to better view the distribution 
hist(log(duration_numeric), main = paste("Duration of UFO Sitings"))

# AO: Great idea to make it log base 10 to better view data.
# AO; Overall, amazing code that works to accomplish all the criteria listed for Assignment 4.