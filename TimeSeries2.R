library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)

#import 
getwd()
dat <- read_excel("./dataset_2/DATA _ASSINGNMENT 2.xlsx")

dat
dat_m = dat[c("Year", "Area")]

dat_m["year_"] <- dat_m$Year %>% substr(1,4) %>% as.numeric()

#plot the raw data 
plot.default(dat_m$year_, dat_m$Area, type = "l")