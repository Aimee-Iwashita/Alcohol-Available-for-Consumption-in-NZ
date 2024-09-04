# Detailed Documentation of Alcohol available for consumption in Aotearoa Analysis

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
Finding the optimal &lambda; value:
```
lambda <- alc.train %>%
  features(Alcohol, features = guerrero) %>%
  pull(lambda_guerrero)
```
The optimal &lambda; value for a Box-Cox transformation of my training set is 0.2939.

Performing Box-Cox transformation in training data:
```
alc.train <- alc.train %>%
  mutate(Alcohol_bc = box_cox(Alcohol, lambda))
```

### Seasonal differencing
Seasonal differencing is applied to the Box-Cox transformed data to make it stationary. <br>
Calculating the seasonal strength:
```
alc.train %>%
  features(Alcohol_bc, feat_stl) %>%
  select(seasonal_strength_year)
```
<img src="https://github.com/Aimee-Iwashita/Alcohol-Available-for-Consumption-in-NZ/blob/main/images/Seasonal_Strength.png" alt="Seasonal strength">
The seasonal strength, 𝐹𝑠, on the Box-Cox transformed training data is 0.958 which is greater than 0.64. The seasonal strength indicates the presence of strong seasonality in our Box-Cox transformed training data and therefore suggests for one seasonal difference.
<br>
<br>

Applying one seasonal difference:

```
alc.train <- alc.train %>%
  mutate(Alcohol_sd = difference(Alcohol_bc, lag = 4))
```

Calculating the seasonal strength:
```
alc.train %>%
  features(Alcohol_sd, feat_stl) %>%
  select(seasonal_strength_year)
```


