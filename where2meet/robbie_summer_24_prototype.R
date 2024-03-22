#this works and matches an example output we would have. not sure how to expand it to accomdate more than 2 date ranges/locations though...


library(dplyr)
library(tidyr)

# Sample dataframe with column names
df <- data.frame(
  Name = c("John Doe"),
  Start_Date_1 = as.Date("2024-06-01"),
  End_Date_1 = as.Date("2024-08-18"),
  Start_Date_2 = NA,
  End_Date_2 = NA,
  City_of_1st_location = c("Venice"),
  Country_of_1st_location = c("Italy"),
  City_of_2nd_location = NA,
  Country_of_2nd_location = NA
)

# Function to expand date ranges
expand_date_ranges <- function(start_date, end_date, city, country) {
  date_range <- seq(start_date, end_date, by = "day")
  data.frame(
    Date = date_range,
    City = city,
    Country = country
  )
}

# Expand the date ranges for the first location
df_location_1 <- expand_date_ranges(df$Start_Date_1, df$End_Date_1, df$City_of_1st_location, df$Country_of_1st_location)

# Expand the date ranges for the second location if applicable
if (!is.na(df$Start_Date_2) && !is.na(df$End_Date_2)) {
  df_location_2 <- expand_date_ranges(df$Start_Date_2, df$End_Date_2, df$City_of_2nd_location, df$Country_of_2nd_location)
} else {
  df_location_2 <- NULL
}

# Combine the expanded dataframes
df_expanded <- bind_rows(df_location_1, df_location_2)

# Merge with original dataframe to retain other columns
df_final <- merge(df[, -c(2:5)], df_expanded, by = NULL)
