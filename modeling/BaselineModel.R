# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

library(tibble)
library(tsibble)
library(forecast)
library(lubridate)
library(feasts)
library(ggfortify)
library(tseries)

library(patchwork)
library(sandwich)
library(lmtest)
library(stringr)
library(smooth)
library(fable)
library(gridExtra)
library(seasonal)

# Load in and wrangle data

caiso_data = read.csv("~/Documents/MIDS_Fall2023/Capstone/CAISO_data.csv")

set.seed(10)
test_caiso = caiso_data[1:100, ]

test_caiso <- test_caiso %>%
  mutate(date = as.Date(substr(timestamp, 1, 10), format = "%Y-%m-%d")) %>%
  group_by(date) %>%
  mutate(total_demand = sum(load_MW)) %>%
  filter(row_number()==1) %>%
  as_tibble(test_caiso) %>%
  dplyr::select(date, total_demand)

# do full data now
use_data <- caiso_data %>%
  mutate(date = as.Date(substr(timestamp, 1, 10), format = "%Y-%m-%d")) %>%
  group_by(date) %>%
  mutate(total_demand = sum(load_MW)) %>%
  filter(row_number()==1) %>%
  as_tibble(caiso_data, index = date) %>%
  dplyr::select(date, total_demand)

autoplot(as.ts(use_data$total_demand))
# there is an outlier for the first and last observations - dropped for now likely due to how the data loads

# Train-Test Split 
set.seed(1)

train <- use_data[2:round(length(use_data$total_demand) * 0.75), ] %>%
  as_tsibble(index=date)

h <-length(train$total_demand)

test <- use_data[(h + 2):length(use_data$total_demand)-1, ] %>%
  as_tsibble(index=date)


# Do analysis on Train now

time_plot <- train %>%
  ggplot() +
  aes(x=yearmonth(date),y=total_demand) +
  geom_line() +
  labs(
    title = 'Daily Electricity Demand',
    x = 'Month and Year',
    y = 'Daily Demand') +
  theme(legend.position = c(.2,.8))

overall_acf <- train %>% 
  ACF(y=total_demand, lag_max = 100) %>% 
  autoplot()

overall_pacf <- train %>% 
  PACF(y=total_demand, lag_max = 100) %>% 
  autoplot()

hist <- train %>%
  ggplot() +
  geom_histogram(aes(x=total_demand), bins=30) +
  labs(
    x = "Daily Demand") +
  theme(legend.position = c(.2,.8))

(time_plot + hist) / 
  (overall_acf + overall_pacf)

# Do a formal test of stationary data - ADF test
adf.test(train$total_demand)


# Take first difference and redo time plot at day level
train <- train %>%
  mutate(first_diff = c('NA', diff(train$total_demand))) 

autoplot(as.ts(as.integer(train$first_diff))) # here we see that the mean is clearly 0 
                                              # but the variance is still interesting to see (seasonality)


# Take first difference and redo the four panel plot
time_plot <- train %>%
  ggplot() +
  aes(x=yearmonth(date),y=as.integer(first_diff)) +
  geom_line() +
  labs(
    title = 'Differenced Daily Electricity Demand',
    x = 'Month and Year',
    y = 'First Difference') +
  theme(legend.position = c(.2,.8))

overall_acf <- train %>% 
  ACF(y=as.integer(first_diff), lag_max = 100) %>% 
  autoplot()

overall_pacf <- train %>% 
  PACF(y=as.integer(first_diff), lag_max = 150) %>% 
  autoplot()

hist <- train %>%
  ggplot() +
  geom_histogram(aes(x=as.integer(first_diff)), bins=30) +
  labs(
    x = "Differenced Daily Demand") +
  theme(legend.position = c(.2,.8))

(time_plot + hist) / 
  (overall_acf + overall_pacf)

# From the ACF plot we see that seems to be an AR term definitely, seen by the oscillating nature of the ACF spikes,
# furthermore, there seems to be a seasonal AR term since the PACF has highly significant spikes (oscillating again) 
# until you go past day 70/80 (roughly length of a season) at which point the spikes becomes noise (less than 5% go beyond 
# the blue line) indicating that it could be due to chance. This also indicates perhaps we will see a seasonal AR term and/or
# difference which would also be negative since 

# Does not seem like an MA term would be suitable for an SARIMA model since usually models with 
#  any significant MA terms taper to 0 in the ACF over time 

# Do ADF test on the differenced data
adf.test(train$first_diff, k = 7)


# Confirm via KPSS unit roots required function 

train |>
  features(total_demand, unitroot_ndiffs) # runs KPSS hyp tests until it gets significant p-value

# Lets take a look at the STL decomposition of the differenced data now

first_diff_data <- train[2:length(train$total_demand),] %>%
  mutate(first_diff = as.numeric(first_diff)) %>%
  dplyr::select("date", "first_diff")

p7 <- as_tsibble(first_diff_data) |>
  model(stl = STL(first_diff)) |>
  components() |>
  autoplot() +
  labs(title="STL Decomposition")

p8 <- as_tsibble(first_diff_data) |>
  model(stl = STL(first_diff)) |>
  components() |>
  ACF(remainder) |>
  autoplot() +
  labs(title="Residuals STL Decomposition")

p7 # residuals look like white noise after removing the yearly and weekly trend but still have the issue of differing variance

p8 # still see pattern in the ACF of residuals indicating we cannot just deseasonalize the data

# Take a shot at a SARIMA model that will be integrated once
# FIRST USING stepwise search

baseline_model_stepwise_search <- auto.arima(
  first_diff_data,
  d = 1,
  max.p = 30,
  max.q = 30,
  max.P = 10,
  max.Q = 10,
  max.order = 20, # change to higher value if you change stepwise to be FALSE instead of TRUE
  max.d = 2,
  max.D = 2,
  start.p = 1,
  start.q = 1,
  start.P = 1,
  start.Q = 1,
  ic = "bic", # change to only BIC and see how model differs 
  stepwise = TRUE, # change to false and see how model differs - IF FALSE: make max.ORDER higher, parallel = TRUE if taking a long time
  nmodels = 5000, # max number of models to try
  trace = TRUE, # changed to TRUE - allows us to see all ARIMA models considered
  allowdrift = TRUE, # allow drift or now
  biasadj = FALSE,
  parallel = FALSE, # change to TRUE if change stepwise to FALSE to run in parallel on multi-core machine
  num.cores = 12, # specify number of cores in your machine
)

baseline_model_stepwise_search

# DO grid search now and see which model we get - RAN FOR OVER FOUR HOURS - pulled the plug

baseline_model_grid_search <- auto.arima(
  first_diff_data,
  d = 1,
  max.p = 15,
  max.q = 10,
  max.P = 10,
  max.Q = 10,
  max.order = 30, # change to higher value if you change stepwise to be FALSE instead of TRUE
  max.d = 2,
  max.D = 1,
  start.p = 1,
  start.q = 1,
  start.P = 1,
  start.Q = 1,
  ic = "bic", # change to only BIC and see how model differs 
  stepwise = FALSE, # change to false and see how model differs - IF FALSE: make max.ORDER higher, parallel = TRUE if taking a long time
  nmodels = 1000, # max number of models to try
  trace = TRUE, # changed to TRUE - allows us to see all ARIMA models considered
  allowdrift = TRUE, # allow drift or now
  biasadj = FALSE,
  parallel = TRUE, # change to TRUE if change stepwise to FALSE to run in parallel on multi-core machine
  num.cores = 12, # specify number of cores in your machine
)

baseline_model_grid_search

# use BIC since BIC has a stronger penalty for including  additional variables to the model compared to AIC

# we get quite an interesting result 

            # model.fit.all <- train %>%
            #   model(ts.model = ARIMA(total_demand ~ 0 + pdq(0:10,0:2,0:10) + PDQ(0:10,0:2,0:10), ic="bic", greedy=F, stepwise=F))
            
            # model.fit.all$ts.model
            
            # model.fit.all %>% coef()                                                

# THEN TRY - dynamic Harmonic regression since 
# "When there are long seasonal periods, a dynamic regression with Fourier terms is often better than other models we have considered in this book.
# For example, daily data can have annual seasonality of length 365, weekly data has seasonal period of approximately 52
# auto.sarima(birth,xreg = fourier(birth,K= 6),iter = 500,chains = 1)"
# REFER TO FIRST PARAGRAPH of Section 9.5 in FPP

sarima_harmonic_reg <- auto.sarima(first_diff_data, xreg = fourier(first_diff_data, K= 6), iter = 5000, chains = 4)
sarima_harmonic_reg 
################################################
################################################

### RESIDUAL ANALYSIS AND FORECASTING

# DO four panel plot of the residuals 

baseline_train_resids <- residuals(baseline_model_stepwise_search)
first_diff_data$residuals <- baseline_train_resids

resid_hist <- ggplot(data = first_diff_data,
                                  aes(x = residuals)) + geom_histogram(bins = 15) +
  xlab("Residual Value") + ylab("Frequency") + ggtitle("Histogram of Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

resid_timeplot <- ggplot(data = first_diff_data,
                                      aes(x = date, y = residuals)) + geom_line() +
  xlab("Time") + ylab("Residual Value") + ggtitle("Residuals Time Plot") +
  theme(plot.title = element_text(hjust = 0.5))

resid_acf <- first_diff_data |>
  ACF(residuals, lag_max = 100) |>
  autoplot() + labs(title = "ACF Plot")

resid_lag <- first_diff_data |>
  gg_lag(residuals, geom = "point", color = 1) + labs(x = "lag(Resid, n)") +
  ylab("Residual Value") + ggtitle("Residual Lags")

(resid_hist | resid_timeplot) /
  (resid_acf | resid_lag) + plot_annotation(title = "Inspecting Model Residuals")

### FORECASTS FOR TRAIN DATA

train_fitted_data <- train[2:length(train$first_diff), c("date", "first_diff")] %>%
  mutate(fitted_vals = baseline_model_stepwise_search$fitted) %>%
  mutate(first_diff = as.numeric(first_diff)) %>%
  as_tsibble(index = date)


p = ggplot() + 
  geom_line(data = train_fitted_data, aes(x = date, y = first_diff), color = "black", alpha = 0.8) +
  geom_line(data = train_fitted_data, aes(x = date, y = fitted_vals), color = "red", alpha = 0.2) +
  xlab('Date') +
  ylab('Daily Energy Demand')

p

## LOSS METRICS

print(paste('TRAIN MSE:', MSE(baseline_model_stepwise_search$residuals, forecast = baseline_model_stepwise_search$fitted)))
print(paste('TRAIN RMSE:', RMSE(baseline_model_stepwise_search$residuals, forecast = baseline_model_stepwise_search$fitted)))
print(paste('TRAIN MAE:', MAE(baseline_model_stepwise_search$residuals, forecast = baseline_model_stepwise_search$fitted)))
print(paste('TRAIN MPE:', MPE(baseline_model_stepwise_search$residuals, forecast = baseline_model_stepwise_search$fitted)))
print(paste('TRAIN MAPE:', MAPE(baseline_model_stepwise_search$residuals, forecast = baseline_model_stepwise_search$fitted)))
b
### FORECASTS FOR TEST DATA 
# add differenced data to test_data

test <- use_data[(h + 2):length(use_data$total_demand)-1, ] %>%
  as_tsibble(index=date)

test <- test %>%
  mutate(first_diff = c('NA', diff(test$total_demand))) %>%
  slice(2:length(test$total_demand)) %>%
  dplyr::select("date", "first_diff") %>%
  mutate(first_diff = as.numeric(first_diff)) %>%
  as_tsibble(index = date)

forecast(baseline_model_stepwise_search, h = 15) %>% autoplot()

# forecast_dates <- test %>%
#   dplyr::select("date") %>%
#   as_tsibble(index = date)
# 
# forecast(baseline_model_stepwise_search, forecast_dates)
# 
# baseline_model_stepwise_search %>% forecast(new_data = test$date)
# 
# forecast_dates %>%
#   as_tsibble() %>%
#   model(baseline_model_stepwise_search) %>%
#   forecast()




