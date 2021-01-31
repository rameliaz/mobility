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

ds <- ts(ds, start=decimal_date(as.Date("2020-02-15")), frequency = 347)


## Performing Time Series Linear Regression
library(car)

reg.tm.0 <- tslm(kasus_harian ~ 1, data=ds)

reg.tm.1 <- tslm(kasus_harian ~
                   avrg_mobility, data=ds)

summary(reg.tm.1)

reg.tm.2 <- tslm(kasus_harian ~
                 avrg_mobility + jumlah_tes, confint=T, data=ds)

summary(reg.tm.2)
confint(reg.tm.2, level=0.95)
checkresiduals(reg.tm.2)
plot(reg.tm)
naive(ds, bootstrap=T)

arima <- auto.arima(ds[,10], xreg=ds[,11:12])
summary(arima)
cbind("Regression Errors" = residuals(arima, type="regression"),
      "ARIMA errors" = residuals(arima, type="innovation")) %>%
  autoplot(facets=TRUE)
checkresiduals(arima)


## Data visualization
library(ggplot2)

autoplot(ds[,3:8], facet=T) +
  ylab("Percent Change from Baseline")

ds %>% 
  as.data.frame() %>% 
  ggplot(aes(x=avrg_mobility, y=kasus_harian)) +
  ylab("Jumlah Kasus Terkonfirmasi Harian") +
  xlab("% Perubahan Mobilitas dari Baseline") +
  geom_point() +
  geom_smooth(method="lm", se=F)

ds %>% 
  as.data.frame() %>% 
  ggplot(aes(x=jumlah_tes, y=kasus_harian)) +
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
