library(tidyverse)
library(tidymodels)
library(skimr)
library(nycflights13)

# what do flights look like?
glimpse(flights)
?flights
skim(flights)

# make the model
flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, 
                       "late", 
                       "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) 
    # in the recipe below
    date = as.Date(time_hour)
  ) %>% 
  # Include  weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, 
         dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is 
  # better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate(across(where(is.character), as.factor))


# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible 
# when random numbers are used 
set.seed(555)

# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, 
                            prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# create the recipe
flights_rec <- 
  recipe(arr_delay ~ ., 
         data = train_data) %>% 
  update_role(flight, 
              time_hour, 
              new_role = "ID") %>% 
  step_date(date, 
            features = c("dow", "month")) %>% 
  step_holiday(date, 
               holidays = timeDate::listHolidays("US")) %>%
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_predictors())

# Perform the recipe with prep()
flights_prep <- 
  flights_rec %>% 
  prep() # creates new dataframe with values

# Obtain data with juice()
df_flight_prep <- juice(flights_prep)

# Save data as rds for future use
write_rds(df_flight_prep, "df_flight_prep.rds", compress = "gz")

