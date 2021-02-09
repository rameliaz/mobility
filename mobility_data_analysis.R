# DATA ANALYSIS of Mobility Study
## Read README File for study description

## Importing data from Googlesheet
library(googlesheets4)

ds <- read_sheet("https://docs.google.com/spreadsheets/d/1rGNEqBPTZcPJ5VYdfBKF-uQJfVjA5-8EG05cKLXNwGk/edit#gid=641080921", range="analisis")

## Correlations between variables

library(GGally)

ggpairs(as.data.frame(ds[,4:10]))

## NOTE: Since parks, retail, grocery, station, workplaces, and residential are substantially correlated to one another, we combine those all to a single variable = avrg_mobility

## Combining mobility variables into single variable
library(tidyverse)

ds <- ds %>% 
  rowwise() %>% 
  mutate(avrg_mobility=mean(c(parks, retail, grocery, station, workplaces, residential)))

ggpairs(as.data.frame(ds[,10:12]))


## Creating ts object
library(forecast)
library(lubridate)

ds <- ts(ds, start=decimal_date(as.Date("2020-02-15")), frequency = 365)


## Performing Time Series Linear Regression
reg.tm.0 <- tslm(daily_cases ~ 1, data=ds)

reg.tm.1 <- tslm(daily_cases ~
                   avrg_mobility, data=ds)

summary(reg.tm.1)

reg.tm.2 <- tslm(daily_cases ~
                 avrg_mobility + testing, data=ds)

summary(reg.tm.2)

### Model Diagnostics
checkresiduals(reg.tm.2)
anova(reg.tm.1,reg.tm.2)
anova(reg.tm.0,reg.tm.1)

resid <-  as.data.frame(ds)
resid[, "residuals"] <- as.numeric(residuals(reg.tm.2))
ggplot(resid, aes(x=avrg_mobility, y=residuals)) +
  geom_point()
ggplot(resid, aes(x=testing, y=residuals)) +
  geom_point()

cbind(Fitted = fitted(reg.tm.2),
      Residuals=residuals(reg.tm.2)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

### NOTES: apparently the model violates all regression assumptions, so that we need to use other strategy here. Since the number of cases (our dependent variable) seems to be exponential and additionally, both predictors (testing and mobility) are lagged, Dynamic Regression with Lagged Predictors will be instead used.

## Creating reproducible example

reprex::reprex(input="mobility_data_analysis.R", venue="gh")


## Dynamic Regression with Lagged Predictors [Testing]

test <- cbind(
  testlag6 = stats::lag(ds[,"testing"],-6),
  testlag8 = stats::lag(ds[,"testing"],-8),
  testlag14 = stats::lag(ds[,"testing"],-14),
  testlag20 = stats::lag(ds[,"testing"],-20)) %>%
  head(NROW(test))

# Restrict data so models use same fitting period
fit1 <- auto.arima(ds[120:150,10], xreg=test[120:150,1],
                   stationary=TRUE)
fit2 <- auto.arima(ds[120:150,10], xreg=test[120:150,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(ds[120:150,10], xreg=test[120:150,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(ds[120:150,10], xreg=test[120:150,1:4],
                   stationary=TRUE)

c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])


(fit.0 <- auto.arima(ds[,10], xreg=test[,1],
                   stationary=TRUE)) ## Fitting the model
(1-pnorm(abs(fit.0$coef)/sqrt(diag(fit.0$var.coef))))*2 ## Calculating p-values
checkresiduals(fit.0)

## Dynamic Regression with Lagged Predictors [Mobility and Testing]

lag <- cbind(
  mobilelag6 = stats::lag(ds[,"avrg_mobility"],-6),
  mobilelag8 = stats::lag(ds[,"avrg_mobility"],-8),
  mobilelag14 = stats::lag(ds[,"avrg_mobility"],-14),
  mobilelag20 = stats::lag(ds[,"avrg_mobility"],-20),
  testlag = stats::lag(ds[,"testing"],-6)) %>%          ## Binding test lag
  head(NROW(ds))

# Restrict data so models use same fitting period
fit1 <- auto.arima(ds[120:150,10], xreg=lag[120:150,1],
                   stationary=TRUE)
fit2 <- auto.arima(ds[120:150,10], xreg=lag[120:150,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(ds[120:150,10], xreg=lag[120:150,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(ds[120:150,10], xreg=lag[120:150,1:4],
                   stationary=TRUE)

c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

(fit.1 <- auto.arima(ds[,10], xreg=lag[,c(2,5)],
                   stationary=TRUE)) ## Fitting the model
(1-pnorm(abs(fit.1$coef)/sqrt(diag(fit.1$var.coef))))*2 ## Calculating p-values
checkresiduals(fit.1)

c(fit.0[["aicc"]],fit.1["aicc"])


## Data visualization
library(ggplot2)

autoplot(ds[,3:8], facet=T) +
  ylab("Percent Change from Baseline")

ds %>% 
  as.data.frame() %>% 
  ggplot(aes(x=avrg_mobility, y=daily_cases)) +
  ylab("Jumlah Kasus Terkonfirmasi Harian") +
  xlab("% Perubahan Mobilitas dari Baseline") +
  geom_point() +
  geom_smooth(method="lm", se=F)

ds %>% 
  as.data.frame() %>% 
  ggplot(aes(x=testing, y=daily_cases)) +
  ylab("Jumlah Kasus Terkonfirmasi Harian") +
  xlab("Total Jumlah Tes") +
  geom_point() +
  geom_smooth(method="lm", se=F)

## With facets
autoplot(ds[,c("parks", "retail", "grocery", "station", "workplaces", "residential")], facets=T) +
  ylab("% Perubahan Mobilitas dari Baseline") + xlab("Tanggal")

## Without facets
autoplot(ds[,c("parks", "retail", "grocery", "station", "workplaces", "residential")]) +
  ylab("% Perubahan Mobilitas dari Baseline") + xlab("Tanggal")
