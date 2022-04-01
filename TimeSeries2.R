library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)

#import 
getwd()
dat <- read_excel("./dataset_2/DATA _ASSINGNMENT 2.xlsx")

dat

dat_m["year_"] <- dat_m$Year %>% substr(1,4) %>% as.numeric()
dat_m = dat_m[c("year_", "Area")]


#plot the raw data 
plot.default(dat_m$year_, dat_m$Area, type = "l")

#ma 7 day (trend)
for (i in 1:(nrow(dat_m)-6))
{
  print(i)
  
  print(i+6)
  dat_m[i+6, "MA_7"] <- mean(dat_m$Area[i:(i+6)])
}
row.names(dat_m) <- dat_m$year_
View(dat_m)
#plot trend

plot.default(dat_m$year_, dat_m$MA_7, type = "l")

#detrend
dat_m["detrended"] = dat_m$Area / dat_m$MA_7

plot.default(dat_m$year_, dat_m$detrended, type = "l")
