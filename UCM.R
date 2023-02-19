library(forecast)
library(xts)
library(ggplot2)
library(urca)
library(lubridate)
library(dplyr)
library(KFAS)


########## PREPROCESSING ##########

rm(list=ls())
dt <- read.csv('C:/Users/nicco/OneDrive/Desktop/Data Science - MAGISTRALE/Anno 2/Semestre 1/SDMTSA/Progetto/data2022_train.csv') 

# Check NA values
any(is.na(dt))

# Transforming into datetime object
dt$X <- as.POSIXct(dt$X, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract the hour information from the datetime column
dt$hour <- format(dt$X, "%H")

# Aggregate the data by hour and calculate the mean
agg_dt <- aggregate(dt$y, list(hour=cut(as.POSIXct(dt$X), "hour")), mean)

# Rename the columns for better readability
colnames(agg_dt) <- c("date", "mean")

# Convert into xts
xts_dt <- xts(agg_dt[, 2], order.by = as.POSIXct(agg_dt$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
colnames(xts_dt) <- c("y")

# Dividing into training and validation
y <- xts_dt$y

y_ <- seq(from = end(y) + 60*60, 
          to = end(y) + 30*60*60*24, 
          by = "hour")
y <- merge(y, y_)

# Plotting time series graph
plot(as.numeric(xts_dt$y[1:8016]), type = 'l', main = 'Hourly time series', xlab = 'Observation', ylab = 'Energy consumption')
abline(v = 7296, col = "blue", lwd = 2)



########## FORECASTING ##########

y <- xts_dt$y
y0 <- y
y0[7297:8016] <- NA

mod <- SSModel(as.numeric(y0) ~ 
                  SSMtrend(2, list(NA, NA)) + 
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, NA, "trigonometric", harmonics = 1:6),
                H = NA)

vy <- var(as.numeric(y0), na.rm = TRUE)

mod$P1inf[] <- 0
diag(mod$P1) <- vy * 10
mod$a1[1] <- mean(y0[1:672])

pars <- c(
  logVarEta = log(vy / 100), 
  logVarZeta  = log(vy / 10000),
  logVarOm168 = log(vy / 10000),
  logVarOm24 = log(vy / 1000),
  logVarEps   = log(vy / 100) 
)

updtfn <- function(pars, model) {
  nq <- nrow(model$Q[, , 1])
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$Q[3, 3, 1] <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  diag(model$Q[4:nq, 4:nq, 1]) <- exp(pars[3])
  model
}

fit <- fitSSM(mod, pars, updtfn, control = list(maxit = 2000))
fit$optim.out

kfs <- KFS(fit$model, smoothing = c("state", "signal", "disturbance")) 

# Prediction
plot(as.numeric(y[7297:8016]))
lines(kfs$muhat[7297:8016], col = "red")
mae <- mean(abs(y[7297:8016] - kfs$muhat[7297:8016]))

forecast_10 = list()
for(i in 1:719){
  forecast_10 <- append(forecast_10, seq(from = (unname(unlist(kfs$muhat[7297:8016])))[i], to = (unname(unlist(kfs$muhat[7297:8016])))[i+1], length.out = 7)[-7])
}

forecast_10 <- c(forecast_10, rep(tail(forecast_10, n = 1), times = 6))
start_date <- as.POSIXct("2017-11-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2017-11-30 23:50:00", tz = "UTC")
date_sequence <- seq(from = start_date, to = end_date, by = "10 min")

xts_test <- xts(unname(unlist(forecast_10)), order.by = date_sequence)
colnames(xts_test) <- c("mean")

val = dt$y[43777:48096]

plot(date_sequence, val, type = 'l', main = 'UCM', ylab = 'Power consumption', xlab = 'Date sequence')
lines(date_sequence, xts_test$mean, col = 'red')

(val - xts_test$mean) |> abs() |> mean()

write.csv(xts_test$mean, "C:/Users/nicco/OneDrive/Desktop/UCM.csv")