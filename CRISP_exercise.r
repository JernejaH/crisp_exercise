library(timetk)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(tidyr)
library(xts)
library(plyr)
library(dplyr)
library(ggplot2)
library(widyr)
library(Hmisc)
library(visdat)
library(imputeTS)
library (scales)
library(pracma)

rm(list = ls())
dev.off()

# Read the data
filename <- file.choose()
temperature_data = read.csv(filename, sep = ",")

# check data for completeness
sum(is.na(temperature_data))
percent_na_columns = (colSums(is.na(temperature_data)) / nrow(temperature_data)) * 100
percent_na_columns
vis_dat(temperature_data, warn_large_data = FALSE)

# replace missing data
temperature_data = na_seadec(temperature_data, algorithm = "interpolation", find_frequency=TRUE)

# subset and prepare data to check if selected time zone makes sense
temperature_data$datetime = as.POSIXct(temperature_data$datetime, tryFormats = c("%Y-%m-%d %H:%M"))
temperature_data_20150809 = filter(temperature_data, 
                                   datetime >= as.Date('2015-08-09') & 
                                   datetime <= as.Date('2015-08-13'))
temperature_data_20150809= gather(temperature_data_20150809, cities, temperature, 
                          (Vancouver:Jerusalem))
temperature_data_20150809 = group_by(temperature_data_20150809, cities)

temperature_data_20150809_subset = temperature_data_20150809[
    temperature_data_20150809$cities == "Miami"
  | temperature_data_20150809$cities == "Jerusalem"
  | temperature_data_20150809$cities == "Dallas"
  | temperature_data_20150809$cities == "Denver"
  | temperature_data_20150809$cities == "Los.Angeles",]

# plotted hourly temperature for cities in various time zones
plot_find_timezone = ggplot(data = temperature_data_20150809_subset, 
                      aes(x = datetime, y = temperature, col = cities)) + 
                      geom_line() + theme_light() +
                      scale_x_datetime(breaks = date_breaks("4 hours"), 
                      labels=date_format("%H")) + labs(y="Temperature [K]", 
                      x = "Time [h]", colour = "Cities")
plot_find_timezone

# assign assumed time zone
temperature_data$datetime = as.POSIXct(temperature_data$datetime, tryFormats = c("%Y-%m-%d %H:%M"),
                                       tz = "EDT")
# convert to requested time zone
temperature_data$datetime <- with_tz(temperature_data$datetime, "UTC")

# For each city calculate the correlation of temperature from hour to hour, day to day, week to week, and month to month
# prepare data and visualize dataset
temperature_data = gather(temperature_data, cities, temperature, 
                          (Vancouver:Jerusalem))
temperature_data_grouped = group_by(temperature_data, cities)

temperature_data_plot = ggplot(data = temperature_data_grouped, 
                        aes(x = datetime, y = temperature, col = cities)) +
                        geom_line() + theme_light() +
                        geom_smooth(method = "lm", se = FALSE, size = 1) +
                        labs(y="Temperature [K]", x = "Time", colour = "Cities") +
                        theme(legend.text=element_text(size=6))
temperature_data_plot

# define time scales
temperature_hourly = temperature_data_grouped

temperature_daily = summarise_by_time(temperature_data_grouped,
    .date_var = datetime,
    .by = "day", 
    value = mean(temperature))

temperature_weekly = summarise_by_time(temperature_data_grouped,
    .date_var = datetime,
    .by = "week", 
    value = mean(temperature))

temperature_monthly = summarise_by_time(temperature_data_grouped,
    .date_var = datetime,
    .by = "month",
     value = mean(temperature))

# calculate autocorrelation for above defined time scales
autocorrelation_hourly = tk_acf_diagnostics(
  temperature_hourly, 
  datetime, temperature, 
  .ccf_vars = NULL, 
  .lags = 120)
autocorrelation_hourly

autocorrelation_daily = tk_acf_diagnostics(
  temperature_daily, 
  datetime, value, 
  .ccf_vars = NULL, 
  .lags = 370)
autocorrelation_daily

autocorrelation_weekly = tk_acf_diagnostics(
  temperature_weekly, 
  datetime, value, 
  .ccf_vars = NULL, 
  .lags = 4*12*2)
autocorrelation_weekly

autocorrelation_monthly = tk_acf_diagnostics(
  temperature_monthly, 
  datetime, value, 
  .ccf_vars = NULL, 
  .lags = 12*5)
autocorrelation_monthly

# Visualize results
autocorrelation_hourly_subset = autocorrelation_hourly[
  autocorrelation_hourly$cities == "Los.Angeles"
  | autocorrelation_hourly$cities == "San.Francisco"
  | autocorrelation_hourly$cities == "Boston"
  | autocorrelation_hourly$cities == "Montreal",]

plot_hourly_correlation = ggplot(data = autocorrelation_hourly_subset, 
                          aes(x = lag, y = ACF, col = cities)) +
                          geom_line() + theme_light() +
                          labs(y="ACF", x = "Lag", colour = "Cities")
plot_hourly_correlation

autocorrelation_daily_subset = autocorrelation_daily[
  autocorrelation_daily$cities == "Los.Angeles"
  | autocorrelation_daily$cities == "San.Francisco"
  | autocorrelation_daily$cities == "Boston"
  | autocorrelation_daily$cities == "Montreal",]

plot_daily_correlation = ggplot(data = autocorrelation_daily_subset, 
                         aes(x = lag, y = ACF, col = cities)) +
                        geom_line() + theme_light() +
                        labs(y="ACF", x = "Lag", colour = "Cities")
plot_daily_correlation

autocorrelation_weekly_subset = autocorrelation_weekly[
  autocorrelation_weekly$cities == "Los.Angeles"
  | autocorrelation_weekly$cities == "San.Francisco"
  | autocorrelation_weekly$cities == "Boston"
  | autocorrelation_weekly$cities == "Montreal",]

plot_weekly_correlation = ggplot(data = autocorrelation_weekly_subset, 
                          aes(x = lag, y = ACF, col = cities)) +
                          geom_line() + theme_light() +
                          labs(y="ACF", x = "Lag", colour = "Cities")
plot_weekly_correlation

autocorrelation_monthly_subset = autocorrelation_monthly[
  autocorrelation_monthly$cities == "Los.Angeles"
  | autocorrelation_monthly$cities == "San.Francisco"
  | autocorrelation_monthly$cities == "Boston"
  | autocorrelation_monthly$cities == "Montreal",]

plot_monthly_correlation = ggplot(data = autocorrelation_monthly_subset, 
                           aes(x = lag, y = ACF, col = cities)) +
                           geom_line() + theme_light() +
                           labs(y="ACF", x = "Lag", colour = "Cities")
plot_monthly_correlation

# For  cities located in North America, calculate the pairwise correlation in their temperatures at various timescales
# data preparation
temperature_north_america = temperature_data_grouped[
  temperature_data_grouped$cities != "Beersheba"
  & temperature_data_grouped$cities != "Jerusalem"
  & temperature_data_grouped$cities != "Tel.Aviv.District"
  & temperature_data_grouped$cities != "Nahariyya"
  & temperature_data_grouped$cities != "Haifa"
  & temperature_data_grouped$cities != "Eilat",]

temperature_north_america_hourly = summarise_by_time(temperature_north_america,
                                    .date_var = datetime,
                                    .by = "hour",
                                    value = mean(temperature))

temperature_north_america_daily = summarise_by_time(temperature_north_america,
                                    .date_var = datetime,
                                    .by = "day",
                                    value = mean(temperature))

temperature_north_america_weekly = summarise_by_time(temperature_north_america,
                                    .date_var = datetime,
                                    .by = "week",
                                    value = mean(temperature))

temperature_north_america_monthly = summarise_by_time(temperature_north_america,
                                    .date_var = datetime,
                                    .by = "month",
                                    value = mean(temperature))

# converto to wide dataframe
temperature_north_america_hourly = ungroup(temperature_north_america_hourly)
temperature_north_america_hourly = pivot_wider(temperature_north_america_hourly, 
                                     names_from = cities, 
                                     values_from = value)
tmp1 = subset(temperature_north_america_hourly, select = -datetime)

temperature_north_america_daily = ungroup(temperature_north_america_daily)
temperature_north_america_daily = pivot_wider(temperature_north_america_daily, 
                                     names_from = cities, 
                                     values_from = value)
tmp2 = subset(temperature_north_america_daily, select = -datetime)

temperature_north_america_weekly = ungroup(temperature_north_america_weekly)
temperature_north_america_weekly = pivot_wider(temperature_north_america_weekly, 
                                     names_from = cities, 
                                     values_from = value)
tmp3 = subset(temperature_north_america_weekly, select = -datetime)

temperature_north_america_monthly = ungroup(temperature_north_america_monthly)
temperature_north_america_monthly = pivot_wider(temperature_north_america_monthly, 
                                     names_from = cities, 
                                     values_from = value)
tmp4 = subset(temperature_north_america_monthly, select = -datetime)

# calculate pairwise correlation
temperature_north_america_hourly.cor = cor(tmp1, use="pairwise.complete.obs")
temperature_north_america_hourly.cor

temperature_north_america_daily.cor = cor(tmp2, use="pairwise.complete.obs")
temperature_north_america_daily.cor

temperature_north_america_weekly.cor = cor(tmp3, use="pairwise.complete.obs")
temperature_north_america_weekly.cor

temperature_north_america_monthly.cor = cor(tmp4, use="pairwise.complete.obs")
temperature_north_america_monthly.cor

# define correlation coef as distance measure
temperature_hourly.dist <- as.dist(1 - temperature_north_america_hourly.cor)

temperature_daily.dist <- as.dist(1 - temperature_north_america_daily.cor)

temperature_weekly.dist <- as.dist(1 - temperature_north_america_weekly.cor)

temperature_monthly.dist <- as.dist(1 - temperature_north_america_monthly.cor)

# Carry out a clustering of the cities
temperature_hourly.tree <- hclust(temperature_hourly.dist, method="complete")
plot(temperature_hourly.tree)

temperature_daily.tree <- hclust(temperature_daily.dist, method="complete")
plot(temperature_daily.tree)

temperature_weekly.tree <- hclust(temperature_weekly.dist, method="complete")
plot(temperature_weekly.tree)

temperature_monthly.tree <- hclust(temperature_monthly.dist, method="complete")
plot(temperature_monthly.tree)
