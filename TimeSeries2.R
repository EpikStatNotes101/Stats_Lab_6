library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(tseries)

#import 
getwd()
dat = read_excel("./dataset_2/DATA _ASSINGNMENT 2.xlsx")

dat
dat_m = dat[c("Year", "Area")]
dat_m["year_"] = dat_m$Year %>% substr(1,4) %>% as.numeric()
dat_m = dat_m[c("year_", "Area")]


#plot the raw data 
plot.default(dat_m$year_, dat_m$Area, type = "l")

period = 5
#ma 7 day (trend)
for (i in 1:(nrow(dat_m) - period + 1))
{
  dat_m[i + period - 1, "MA_7"] = mean(dat_m$Area[i:(i + period - 1)])
}
#row.names(dat_m) = dat_m$year_
View(dat_m)
#plot trend

plot.default(dat_m$year_, dat_m$MA_7, type = "l")

#detrend
dat_m["detrended"] = dat_m$Area / dat_m$MA_7

plot.default(dat_m$year_, dat_m$detrended, type = "l")

#Irregular component 
I_component = dat_m$detrended[!is.na(dat_m$detrended)]
  
#ADF test
adf.test(I_component)

#ACF plot
acf(I_component)
pacf(I_component)
