library(tidyverse)
library(fpp3)
library(kableExtra)

# Read csv
alc.cons <- read.csv("Alcohol_consumption.csv")

## Data cleaning ##
head(alc.cons)

# Change the column names 
names(alc.cons) <- c("Quarter", "Alcohol")

# Remove the first row 
alc.cons[1,]
alc.cons <- alc.cons[-1,]
row.names(alc.cons) <- NULL

# 1983 Q3 to 1985 Q1 have no data entry
# row 164 to 184 are table information
# Remove these rows 
alc.cons <- alc.cons[-c(1:7, 164:184),]

# Save the dataframe to a new object
alc.df <- alc.cons

alc.df <- alc.df %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

alc.df$Alcohol <- as.numeric(alc.df$Alcohol)

alc.df %>%
  autoplot(Alcohol) +
  theme_minimal() +
  ylab("Alcohol available for consumption (litres)")

# Creating a training set containing all quarters before 2021
alc.train <- alc.df %>%
  filter(Quarter < yearquarter("2021 Q1"))

lambda <- alc.train %>%
  features(Alcohol, features = guerrero) %>%
  pull(lambda_guerrero)

lambda

alc.train <- alc.train %>%
  mutate(Alcohol_bc = box_cox(Alcohol, lambda))

alc.train %>%
  features(Alcohol_bc, feat_stl) %>%
  select(seasonal_strength_year)

alc.train <- alc.train %>%
  mutate(Alcohol_sd = difference(Alcohol_bc, lag = 4))

alc.train %>%
  features(Alcohol_sd, feat_stl) %>%
  select(seasonal_strength_year)

alc.train %>%
  features(Alcohol_bc, unitroot_nsdiffs)

alc.train %>%
  ggplot(mapping = aes(y = Alcohol_sd, x = Quarter)) +
  geom_line() +
  labs(y = "Box-Cox transformed and seasonally differenced Litres", 
       title = "Time plot of Box-Cox transformed and seasonally differenced Litres") +
  theme_light()

# Performing KPSS unit root test
alc.train %>%
  features(Alcohol_sd, unitroot_kpss)

# Verifying the order of first differencing
alc.train %>%
  features(Alcohol_sd, unitroot_ndiffs)



alc.train %>%
  gg_tsdisplay(Alcohol_sd, plot_type = "partial", lag_max = 16) +
  labs(title = "Transformed and seasonally differenced time series")

# Candidate models are ARIMA(0,0,0)(1,1,0)4 and ARIMA(0,0,0)(0,1,1)4

alc.fit <- alc.train %>%
  model(arima000110 = ARIMA(box_cox(Alcohol, lambda) ~ pdq(0,0,0) + PDQ(1,1,0)),
        arima000011 = ARIMA(box_cox(Alcohol, lambda) ~ pdq(0,0,0) + PDQ(0,1,1)),
        auto = ARIMA(box_cox(Alcohol, lambda), stepwise = FALSE))

alc.fit %>%
  pivot_longer(everything(), names_to = "Model_name", values_to = "Orders")

glance(alc.fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)

alc.fit %>%
  select(auto) %>%
  report()


### Diagnostic Checks###
alc.fit %>%
  select(auto) %>%
  gg_tsresiduals()

augment(alc.fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag = 8, dof = 3)


### Forecasting ###

# Forecast h = 13 quarters ahead
alc.fc <- alc.fit %>%
  forecast(h = 13)

# Plot the point-forecasts with the full data set.
alc.df %>%
  ggplot(mapping = aes(x = Quarter, y = Alcohol)) +
  geom_line() +
  geom_line(data = alc.fc, aes(y = .mean, colour = .model)) +
  labs(title = "Point forecasts of 3 models against the full data set")

# Compute test set accuracy
accuracy(alc.fc, alc.df) %>%
  select(.model, MASE)


### ETS model ###
# Fit automatic ETS model on the training set
alc.ets <- alc.train %>%
  model(ETS(Alcohol))

report(alc.ets)

# Forecast h = 13 quarters ahead
alc.ets.fc <- alc.ets %>%
  forecast(h = 13)

# Compute test set accuracy of ETS model
accuracy(alc.ets.fc, alc.df) %>%
  select(.model, MASE)

# Compute test set accuracy of ARIMA model
accuracy(alc.fc, alc.df) %>%
  select(.model, MASE) %>%
  filter(.model == "auto")
