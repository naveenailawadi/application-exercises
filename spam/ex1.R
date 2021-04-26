library(openintro)
library(tidymodels)
library(tidyverse)

# check out the data
glimpse(email)
?email

# spam is a double --> make it a boolean
email <- email %>% 
  mutate(spam = factor(spam))

## use a logistic regression
spam_fit = logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(spam ~ num_char, data = email, family="binomial")

tidy(spam_fit)

# Fix random numbers by setting the seed 
# Enables analysis to be reproducible when random numbers are used 
set.seed(1116)

# Put 80% of the data into the training set 
email_split <- initial_split(email, prop = 0.80)

# Create data frames for the two sets:
train_data <- training(email_split)
test_data  <- testing(email_split)

# build the model
email_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(spam ~ ., data = train_data, family = "binomial")

# predict outcome on the testing dataset
predict(email_fit, test_data)

# look at the probabilities
email_pred = predict(email_fit, test_data, type = "prob") %>% 
  bind_cols(test_data %>%  select(spam, time))

email_pred %>%  
  arrange(desc(.pred_1)) %>% 
  print(n = 10)

# what does that curve look like?
email_pred %>%
  roc_curve(
    truth = spam,
    .pred_1,
    event_level = "second"
  ) %>%
  autoplot()

# feature engineering
train_data %>%
  mutate(
    date = date(time),
    dow  = wday(time),
    month = month(time)
  ) %>%
  select(time, date, dow, month) %>%
  sample_n(size = 5) # shuffle to show a variety

# build a recipe
email_rec <- recipe(
  spam ~ .,          # formula using EVERYTHING (.)
  data = train_data  # data to use for cataloguing names and types of variables
)
summary(email_rec)

# get rid of the time (redundant)
email_rec <- email_rec %>%
  step_date(time, features = c("dow", "month")) %>%
  step_rm(time)

# discretize the variables --> assign the stuff to categorical variables (continuous to categorical)
email_rec <- email_rec %>%
  step_cut(cc, attach, dollar, breaks = c(0, 1)) %>%
  step_cut(inherit, password, breaks = c(0, 1, 5, 10, 20))

# make dummy variables --> giving every factor the same weight (set of 0s and 1s)
  # transforming to 0-1
email_rec <- email_rec %>% 
  step_dummy(all_nominal(), -all_outcomes())

# remove zero variance variables
email_rec <- email_rec %>% 
  step_zv(all_predictors())



