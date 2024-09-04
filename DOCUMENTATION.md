# Detailed Documentation of Alcohol available for consumption in NZ Analysis

The following packages are used for this analysis:
```
library(tidyverse)
library(fpp3)
library(kableExtra)
```

```
# Read csv
alc.cons <- read.csv("Alcohol_consumption.csv")
```

### Data cleaning
```
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
```

### Plot the data 
```
alc.df %>%
  autoplot(Alcohol) +
  theme_minimal() +
  ylab("Alcohol available for consumption (litres)")
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Plot1.png" alt="Time-series plot of data">

```
# Creating a training set containing all quarters before 2021
alc.train <- alc.df %>%
  filter(Quarter < yearquarter("2021 Q1"))
```

### Box-Cox transformation 
```
# Finding the optimal lambda value
lambda <- alc.train %>%
  features(Alcohol, features = guerrero) %>%
  pull(lambda_guerrero)
```
The optimal &lambda; value for a Box-Cox transformation of my training set is 0.2939.

```
# Performing Box-Cox transformation in training data
alc.train <- alc.train %>%
  mutate(Alcohol_bc = box_cox(Alcohol, lambda))
```

### Seasonal differencing
Seasonal differencing is applied to the Box-Cox transformed data to make it stationary. <br>
```
# Calculating the seasonal strength
alc.train %>%
  features(Alcohol_bc, feat_stl) %>%
  select(seasonal_strength_year)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Seasonal_Strength.png" alt="Seasonal strength">
The seasonal strength, 𝐹𝑠, on the Box-Cox transformed training data is 0.958 which is greater than 0.64. The seasonal strength indicates the presence of strong seasonality in our Box-Cox transformed training data and therefore suggests for one seasonal difference.
<br>
<br>

```
# Applying one seasonal difference
alc.train <- alc.train %>%
  mutate(Alcohol_sd = difference(Alcohol_bc, lag = 4))

# Calculating the seasonal strength
alc.train %>%
  features(Alcohol_sd, feat_stl) %>%
  select(seasonal_strength_year)
```

<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Seasonal_Strength.2.png" alt="Seasonal strength">

```
# Verifying the order of seasonal differencing
alc.train %>%
  features(Alcohol_bc, unitroot_nsdiffs)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Seasonal_Strength_Order.png" alt="Seasonal strength order">

The seasonal strength after one seasonal difference is 0.0616 which is smaller than 0.64. The new seasonal strength indicates that we no longer have a strong seasonality present in our training data and therefore we do not need to apply any more seasonal differences.

The ‘unitroot_nsdiffs’ feature verifies that we need first order seasonal differencing and therefore D = 1.

```
# Plotting seasonally differenced transformed training data
alc.train %>%
  ggplot(mapping = aes(y = Alcohol_sd, x = Quarter)) +
  geom_line() +
  labs(y = "Box-Cox transformed and seasonally differenced Litres", 
       title = "Time plot of Box-Cox transformed and seasonally differenced Litres") +
  theme_light()
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Plot2.png" alt="Plot of seasonally differenced transformed training data">

The plot shows that this time series may have a slightly elevated mean in the middle (between 2000 and 2010) but overall the mean seems to be reasonably constant. Since the mean in the middle part seems to be only very slightly elevated, I believe that this time series has constant mean over time. However we should perform KPSS unit root test to make sure we have stationary time series.

```
# Performing KPSS unit root test
alc.train %>%
  features(Alcohol_sd, unitroot_kpss)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/KPSS_unit_root_test.png" alt="KPSS unit root test">
The null hypothesis of the KPSS unit root test is that the time series is stationary and non-seasonal. The KPSS unit root test returned a p-value of 0.1, hence we have no evidence against the null hypothesis. We will accept the null hypothesis and therefore this time series is stationary.
<br>
<br>

```
# Verifying the order of first differencing
alc.train %>%
  features(Alcohol_sd, unitroot_ndiffs)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/First_differencing_order.png" alt="order of first differencing">
The ‘unitroot_ndiffs’ feature verifies that we do not need to apply any first differences and therefore d = 0.

### Plot the ACF and PACF plots 

```
alc.train %>%
  gg_tsdisplay(Alcohol_sd, plot_type = "partial", lag_max = 16) +
  labs(title = "Transformed and seasonally differenced time series")
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Plot3.png" alt="ACF and PACF plots">


Both the ACF and PACF plots show no significant spikes before lag 4 which is the seasonal frequency, hence the order of non-seasonal AR and MA are zero. Therefore p = 0 and q = 0.

In the ACF plot, there is only one significant spike at lag 4 indicating that the seasonal part of MA has order 1. Therefore Q = 1 for one of the candidate model.

In the PACF plot, there is only one significant spike at lag 4 indicating that the seasonal part of AR has order 1. Therefore P = 1 for one of the candidate model.

We already know that the d = 0 and D = 1.

Therefore the two candidate models are ARIMA(0,0,0)(1,1,0)<sub>4</sub> and ARIMA(0,0,0)(0,1,1)<sub>4</sub>.

### Model fitting 
We will fit the two candidate models as well as the automatic full reserach model.

```
alc.fit <- alc.train %>%
  model(arima000110 = ARIMA(box_cox(Alcohol, lambda) ~ pdq(0,0,0) + PDQ(1,1,0)),
        arima000011 = ARIMA(box_cox(Alcohol, lambda) ~ pdq(0,0,0) + PDQ(0,1,1)),
        auto = ARIMA(box_cox(Alcohol, lambda), stepwise = FALSE))

alc.fit %>%
  pivot_longer(everything(), names_to = "Model_name", values_to = "Orders")
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/models.png" alt="fitted models">

```
# Compare the models using AICc
glance(alc.fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/model_comparison.png" alt="model comparison">

Since we have the same differencing orders for all 3 candidate models, we can compare these models using the AICc. The automatic full search model has the smallest AICc value and therefore we will choose this model as the best model. The automatic full search model is ARIMA(1,0,1)(1,1,0)<sub>4</sub>

```
# Report the chosen model's parameter estimates
alc.fit %>%
  select(auto) %>%
  report()
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/automatic_full_research_model.png" alt="model report">

### Diagnostic check

```
alc.fit %>%
  select(auto) %>%
  gg_tsresiduals()
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Plot4.png" alt="residual diagnostic check">

```
# Perform Ljung-Box test
augment(alc.fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag = 8, dof = 3)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Ljung-Box_test.png" alt="Ljung-Box test">

The assumptions of the ARIMA models are the normality of innovation residuals, the constant variance of innovation residuals, and the independence of the innovation residuals.

The histogram shows that our innovation residuals are roughly normal and therefore satisfies the normality assumption.

The time plot of the innovation residuals appears to have mean zero and reasonably constant variance indicating that the constant variance assumption is met.

The ACF plot does not have any significant spike. We also performed a Ljung-Box test. The null hypothesis of the Ljung-Box test is that our innovation residuals are consistent with iid white noise, in another word, our innovation residuals are independent and does not have any autocorrelation. The test returned an insignificant p-value, hence we have no evidence against the null hypothesis. We accept the null hypothesis that our innovation residuals are independent and does not have any autocorrelation and therefore the independence assumption is satisfied.

Therefore we have satisfied all the assumptions of the ARIMA model.

### Forecasting 

```
# Forecast h = 13 quarters ahead
alc.fc <- alc.fit %>%
  forecast(h = 13)

# Plot the point-forecasts with the full data set
alc.df %>%
  ggplot(mapping = aes(x = Quarter, y = Alcohol)) +
  geom_line() +
  geom_line(data = alc.fc, aes(y = .mean, colour = .model)) +
  labs(title = "Point forecasts of 3 models against the full data set")
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Plot5.png" alt="point forecast vs full data set">

```
# Compute test set accuracy
accuracy(alc.fc, alc.df) %>%
  select(.model, MASE)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/MASE.png" alt="MASE">

The automatic full research model,  ARIMA(1,0,1)(1,1,0)<sub>4</sub>, has the lowest MASE and therefore has the best forecasts.

### ETS model

```
# Fit automatic ETS model on the training set
alc.ets <- alc.train %>%
  model(ETS(Alcohol))

report(alc.ets)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/ETS_model.png" alt="ETS model report">

```
# Forecast h = 13 quarters ahead
alc.ets.fc <- alc.ets %>%
  forecast(h = 13)

# Compute test set accuracy of ETS model
accuracy(alc.ets.fc, alc.df) %>%
  select(.model, MASE)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/ETS_MASE.png" alt="ETS model MASE">

```
# Compute test set accuracy of ARIMA model
accuracy(alc.fc, alc.df) %>%
  select(.model, MASE) %>%
  filter(.model == "auto")
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/ARIMA_MASE.png" alt="ARIMA model MASE">

The automatic ARIMA model has a smaller MASE and therefore has better forecasts over the three year test set compared to the automatic ETS model.
