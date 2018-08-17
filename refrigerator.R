# Libraries we'll use ####
library(tidyverse)
library(lubridate)
library(qcc)

# The data ####
rf_df <- structure(list(Date = c("16/06/18", "29/06/18", "01/07/18", "06/07/18",
                                 "08/07/18", "13/07/18", "15/07/18", "21/07/18", "22/07/18", "27/07/18",
                                 "03/08/18", "05/08/18", "10/08/18"), 
                        kWh = c(0, 6.32, 7.97, 12.07,
                                13.9, 0.4, 2.09, 6.42, 7.77, 1.29, 6.73, 8.57, 12.42), 
                        Comment = c(NA,
                                    "PC", NA, NA, NA, "PC", NA, NA, NA, "PC", NA, NA, NA)), 
                   .Names = c("Date",
                              "kWh", "Comment"), 
                   class = c("tbl_df", "tbl", "data.frame"), 
                   row.names = c(NA,
                                 -13L), 
                   spec = structure(list(cols = structure(list(Date = structure(list(), 
                                                                                class = c("collector_character",
                                                                                          "collector")), 
                                                               kWh = structure(list(), class = c("collector_double",
                                                                                                 "collector")), 
                                                               Comment = structure(list(), class = c("collector_character",
                                                                                                     "collector"))), 
                                                          .Names = c("Date", "kWh", "Comment")), 
                                         default = structure(list(), 
                                                             class = c("collector_guess",
                                                                       "collector"))), 
                                    .Names = c("cols", "default"), 
                                    class = "col_spec"))

# Preliminary EDA ####
rf_df %>%
  mutate(index = row_number()) %>% 
  ggplot(aes(x = index, y = kWh)) +
  geom_point()

# Wrangle the data ####
## Fix the dates so we can do something useful
## Calculate the kWh used between each observation
## Calculate the days between observations
rf_df <- rf_df %>%
  mutate(Date = dmy(Date)) %>% 
  mutate(diff = kWh - lag(kWh),
         days = Date - lag(Date)) 

## Do to power outages, lines with "PC" have invalid $diff values, 
## so convert to NA
## Had some trouble doing this in dplyr...
rf_df$diff[rf_df$Comment == "PC" & !is.na(rf_df$Comment)] <- NA

## Now drop the Comment column, and drop any rows with NA
## This leaves us with useful observations of 
## kWh used between dates. Calculate the average daily
## kWh consumption during this period
rf_df <- rf_df %>% 
  select(-Comment) %>% 
  na.omit() %>% 
  mutate(daily_kWh = diff / as.integer(days))

# Analysis ####
## Basic time series plot of the data
rf_df %>% 
  ggplot(aes(x = Date, y = daily_kWh)) +
  geom_point()

## Use qcc to plot the individual values and the moving range
rf_x <- qcc(data = rf_df$daily_kWh, type = "xbar.one")
rf_daily_m <- matrix(c(rf_df$daily_kWh, lag(rf_df$daily_kWh)), byrow = FALSE, ncol = 2)
rf_r <- qcc(data = rf_daily_m, type = "R", sizes = 2)
# sd(rf_df$daily_kWh)
# 3*sd(rf_df$daily_kWh)       
# sd(rf_df$daily_kWh) + 3*sd(rf_df$daily_kWh)       

## Compare mean and standard deviation estimates using the 
## moving range vs the global standard deviation
rf_x$center
mean(rf_df$daily_kWh) # same!
rf_r$center
sd(rf_df$daily_kWh) # a bit smaller!

## Get the mean and standard deviation from these plots
rf_daily_mean <- rf_x$center
rf_daily_sd <- rf_r$center

## Use Monte Carlo simulation with these values to simulate
## 1000 years of operation of this refigerator (or, alternately,
## 1000 identical refrigerators operating for a year)
rf_annual <- replicate(expr = rnorm(n = 365, 
                                    mean = rf_daily_mean, 
                                    sd = rf_daily_sd) %>% 
                         sum(), 
                       n = 1000)

# Data Story ####
## How big do we want our prediction intervals?
sigmas <- 2
## Population mean and standard deviation, based on the moving range
rf_annual_mean <- mean(rf_annual)
rf_annual_sd <- sd(rf_annual)
## Prediction interval lower and upper
lcl <- rf_annual_mean - sigmas * rf_annual_sd
ucl <- rf_annual_mean + sigmas * rf_annual_sd
## Percent of cases covered by prediction interval
pi_percent_data <- paste0(format(x = (pnorm(sigmas) - pnorm(-sigmas)) * 100, digits = 2, nsmall = 0), "%")


print(paste("Estimated mean annual consumption:", round(mean(rf_annual), 0), "kWh"))
print(paste("Estimated standard deviation:", round(sd(rf_annual), 1), "kWh"))
print(paste("Given the data so far, we can be about", 
            pi_percent_data, 
            "confident that the annual consumption will be between",
            format(lcl, digits = 3),
            "and",
            format(ucl, digits = 3),
            "kWh."))
