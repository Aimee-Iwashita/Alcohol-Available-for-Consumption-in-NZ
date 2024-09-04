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
