#------------------
# ARIMA models in R
#------------------

library(tidyverse)
library(forecast)
library(tseries)

# 1. ARMA(1,1) simulations
set.seed(2023)
n <- 100
sd <- 1
data1 <- arima.sim(model = list(ar = c(0.2), ma = c(0.8), order = c(1, 0, 1)), n = n, sd = sd)
data2 <- arima.sim(model = list(ma = c(0.2), ar = c(0.8), order = c(1, 0, 1)), n = n, sd = sd)

par(mfrow = c(3, 2))
plot(data1, main = "ARMA(1,1) AR=0.2, MA = 0.8")
plot(data2, main = "ARMA(1,1) AR=0.8, MA = 0.2")
acf(data1, main = "ARMA(1,1) AR=0.2, MA = 0.8 ACF")
acf(data2, main = "ARMA(1,1) AR=0.8, MA = 0.2 ACF")
pacf(data1, main = "ARMA(1,1) AR=0.2, MA = 0.8 PACF")
pacf(data2, main = "ARMA(1,1) AR=0.8, MA = 0.2 PACF")

# 2. fit an ARMA model
auto.arima(data1)
auto.arima(data2)

# 3. inverse of unit root
set.seed(2023)
n <- 1000
sd <- 1
data3 <- arima.sim(model = list(ar = c(0.5), ma = c(0.4), order = c(1, 0, 1)), n = n, sd = sd)
fit <- Arima(data3,order=c(1,0,1))
autoplot(fit)

# 4. practical example: Airpassengers data
plot(AirPassengers)
class(AirPassengers)
# [1] "ts"

AirPassengers
#      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 1949 112 118 132 129 121 135 148 148 136 119 104 118
# 1950 115 126 141 135 125 149 170 170 158 133 114 140
# 1951 145 150 178 163 172 178 199 199 184 162 146 166
# 1952 171 180 193 181 183 218 230 242 209 191 172 194

# 5. Augmented Dickey-Fuller test for stationarity
adf.test(diff(AirPassengers), alternative="stationary", k=0)

# Augmented Dickey-Fuller Test
# 
# data:  diff(AirPassengers)
# Dickey-Fuller = -8.5472, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary
# 
# Message d'avis :
# Dans adf.test(diff(AirPassengers), alternative = "stationary", k = 0) :
#   p-value smaller than printed p-value

# the data appear to be stationary after differencing of order 1.

# 6. ACF and PACF of stationary series
par(mfrow = c(1,2))
acf(diff(AirPassengers), plot = TRUE, type = 'correlation',main='ACF Plot', col='red')
pacf(diff(AirPassengers), plot = TRUE, main="PACF Plot", col='red')


# 7. ARIMA model estimation
library(forecast)
set.seed(2023) 
summary(fit <- auto.arima(AirPassengers))
# Series: AirPassengers 
# ARIMA(2,1,1)(0,1,0)[12] 
# 
# Coefficients:
#          ar1     ar2      ma1
#       0.5960  0.2143  -0.9819
# s.e.  0.0888  0.0880   0.0292
# 
# sigma^2 estimated as 132.3:  log likelihood=-504.92
# AIC=1017.85   AICc=1018.17   BIC=1029.35
# 
# Training set error measures:
#   ME     RMSE     MAE      MPE     MAPE     MASE        ACF1
# Training set 1.3423 10.84619 7.86754 0.420698 2.800458 0.245628 -0.00124847
checkresiduals(fit)

# 8. ARIMA model forecasting
fit %>% forecast(level = c(95), h=12) %>% autoplot()
forecast <- forecast(fit, level = c(95), h=12)
forecast
#          Point Forecast    Lo 95    Hi 95
# Jan 1961       445.6349 423.0851 468.1847
# Feb 1961       420.3950 393.9304 446.8596
# Mar 1961       449.1983 419.4892 478.9074
# Apr 1961       491.8399 460.0092 523.6707
# May 1961       503.3945 469.9953 536.7937
# Jun 1961       566.8624 532.3007 601.4242
# Jul 1961       654.2602 618.8122 689.7081
# Aug 1961       638.5975 602.4630 674.7320
# Sep 1961       540.8837 504.2081 577.5594
# Oct 1961       494.1266 457.0177 531.2356
# Nov 1961       423.3327 385.8715 460.7939
# Dec 1961       465.5076 427.7556 503.2596

#----
# end
#----
