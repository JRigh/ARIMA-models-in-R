#---------------------------------------------
# SARIMA model in R: simulation and estimation
#---------------------------------------------

library(forecast)
library(tidyverse)

set.seed(2023)

# Simulate the process
arima_data <- simulate.Arima(model = list(ar = c(0.8), ma = c(0.6), 
                                          seasonal = list(order = c(1, 1, 0), period = 12),
                                          include.mean = FALSE), n = 120)

# Convert the data to a time series object
arima_ts <- ts(arima_data, start = c(2000, 1), frequency = 12)

# Plot the simulated data
ggplot() +
  geom_line(aes(x = index(arima_ts), y = arima_ts), color = "blue") +
  labs(title = "Simulated Seasonal ARIMA Process",
       x = "Time", y = "Value")

# Estimate the parameters of the ARIMA model
arima_fit <- auto.arima(arima_ts)

# Print the model summary
summary(arima_fit)

# Forecast the future values of the time series
arima_fc <- forecast(arima_fit, h = 24)

# Plot the forecasted values
ggplot() +
  geom_line(aes(x = index(arima_ts), y = arima_ts), color = "blue") +
  geom_line(aes(x = index(arima_fc$mean), y = arima_fc$mean), color = "red") +
  geom_ribbon(aes(x = index(arima_fc$mean), ymin = arima_fc$lower[, 2], ymax = arima_fc$upper[, 2]), alpha = 0.2) +
  labs(title = 'Forecasts',
       subtitle = 'Simulated data',
       y="forecasted series", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----