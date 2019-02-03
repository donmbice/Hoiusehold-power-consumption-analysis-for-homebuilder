# R-Pipeline-Prediction-TimeSeries
#"Power Consumption Data"
#Don Bice
###############
# Housekeeping
###############
# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd()
dir()

################################
## Install and load packages
################################
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
require(lubridate) # work with dates
require(dplyr)     # data manipulation (filter, summarize, mutate)
require(tidyr)
require(plotly)
###############
# Load dataset 
###############
hhpwr <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(hhpwr)
str(hhpwr)

##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")

class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "America/New_York"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%d/%m/%Y")

str(hhpwrDT)

##------- Change data types---------##

# Note: Understand the difference between as.numeric(as.character()) and as.numeric()
hhpwrDT$Global_active_power <- as.numeric(as.character(hhpwr$Global_active_power))
hhpwrDT$Global_reactive_power <- as.numeric(as.character(hhpwr$Global_reactive_power))
hhpwrDT$Voltage <- as.numeric(as.character(hhpwr$Voltage))
hhpwrDT$Global_intensity <- as.numeric(as.character(hhpwr$Global_intensity))
hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))
hhpwrDT$Sub_metering_3 <- as.numeric(as.character(hhpwr$Sub_metering_3))

str(hhpwrDT)

## ------ Evaluate NA values ----------##
# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Count the number of values = NA
sum(is.na(hhpwrDT$Sub_metering_1)) # Review any metadata with dataset

## -------- Save pre-processed dataset --------##

# Save file, or
# Save object


#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): filter for time interval (Yr/Mo/Day) using lubridate w/in dplyr mutate() to create col.
# 2. dplyr::filter(): select cols to filter by; full ds + col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select desired time interval to subset
# 4. dplyr::summarize(): select which vars and any calculations for the vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 


#############
## DS ANNUAL 
#############
# main dataset for units per day
day.sum.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  
  # Don't add "1" after month (or label) - causes subset class=tibble when subsetting for seasons below 
  filter(Date >= "2007-01-01" & Date <= "2010-11-25") %>%
  group_by(Year, Month, Day) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(Day)) # Remove the last row that has NA 

# subset to get weekday averages. 
wday.avg.winter.wide <- day.sum.wide %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==12 | Month==1 | Month==2) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))

#########################################################################
# FORECASTING TASKS
#########################################################################

#####--- TSLM FORECAST ---#####
### - 1. Create a subset that shows the total kWh per year for submeter 3 for the years 2007-09. 
### Forecast for 2010 and 2011. 

yr.sum.SM3 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Date >= "2007-01-01" & Date <= "2009-12-31") %>%
  group_by(Year, Month) %>%  # Group data by Year, Month
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3), # Total kWh per hour
            DateTime = first(DateTime)) %>%   # To verify date of first instance
  filter(!is.na(Month)) # Remove the last row that has NA 

yr.sum.SM3  #36 monthly kWh observations

## Create TS object with SubMeter3
tsSM3_070809yearly <- ts(yr.sum.SM3$SM3, frequency=12, start=c(2007,1))

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
install.packages("forecast")
library(forecast)
fitSM3 <- tslm(tsSM3_070809yearly ~ trend + season) 
summary(fitSM3)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -86.059 -18.649   4.661  17.097  81.967 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  299.6832    23.2302  12.901 5.15e-12 ***
# trend          1.8541     0.6391   2.901 0.008056 ** 
# season2      -51.5211    30.6858  -1.679 0.106690    
# season3      -27.9612    30.7057  -0.911 0.371942    
# season4      -65.0082    30.7390  -2.115 0.045489 *  
# season5      -54.1640    30.7855  -1.759 0.091807 .  
# season6      -86.7491    30.8451  -2.812 0.009884 ** 
# season7     -144.9178    30.9179  -4.687 0.000102 ***
# season8     -171.0412    31.0036  -5.517 1.31e-05 ***
# season9      -69.5510    31.1023  -2.236 0.035330 *  
# season10     -53.9824    31.2137  -1.729 0.097130 .  
# season11     -37.0492    31.3378  -1.182 0.249185    
# season12       7.0448    31.4744   0.224 0.824870    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 37.57 on 23 degrees of freedom
# Multiple R-squared:  0.7572,	Adjusted R-squared:  0.6306 
# F-statistic: 5.978 on 12 and 23 DF,  p-value: 0.000126

plot(fitSM3) #for stats plots showing residual errors

## Create the forecast for sub-meter 3. Forecast ahead 24 time periods 
forecastfitSM3 <- forecast(fitSM3, h=24)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3, main= "Power Consumption 2007-2009 w/ Forecast", ylab = "kWh/month - SM3" , xlab = "Time")


#### - 2. Create a subset that shows the total kWh per month for submeter 3 for the months Jan-07 through Oct-10.
# Forecast for Nov-10 through Dec-11. 
# Note: Be sure to make the adjustment depending on if the ts is seasonal. Also, be sure to record the summary 
# metrics and know how to interpret the output; specifically, R-squared, Adjusted R-squared, F-stat, and p-value. 
# Also, understand how the p-value relates to the null hypothesis regarding the statistics (i.e., slope coefficients). 
## 
####Same as above but use data from 2010 as well. Added "X" to all data objects
Xyr.sum.SM3 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Date >= "2007-01-01" & Date <= "2010-10-31") %>%
  group_by(Year, Month) %>%  # Group data by Year, Month
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3), # Total kWh per hour
            DateTime = first(DateTime)) %>%   # To verify date of first instance
  filter(!is.na(Month)) # Remove the last row that has NA 

Xyr.sum.SM3  #46 monthly kWh observations

## Create TS object with SubMeter3
XtsSM3_070809yearly <- ts(Xyr.sum.SM3$SM3, frequency=12, start=c(2007,1))

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
install.packages("forecast")
library(forecast)
XfitSM3 <- tslm(XtsSM3_070809yearly ~ trend + season) 
summary(XfitSM3)
plot(XfitSM3) #for stats plots showing residual errors

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -73.206 -22.304  -0.428  19.314  95.510 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  305.4980    20.6848  14.769 4.22e-16 ***
#   trend          1.9116     0.4308   4.437 9.59e-05 ***
#   season2      -35.2116    26.8680  -1.311  0.19906    
# season3      -39.9132    26.8783  -1.485  0.14705    
# season4      -65.2747    26.8956  -2.427  0.02085 *  
#   season5      -50.5291    26.9197  -1.877  0.06938 .  
# season6      -89.9117    26.9507  -3.336  0.00211 ** 
#   season7     -162.5647    26.9886  -6.023 9.00e-07 ***
#   season8     -190.8591    27.0333  -7.060 4.42e-08 ***
#   season9      -90.8444    27.0847  -3.354  0.00201 ** 
#   season10     -65.2370    27.1429  -2.403  0.02202 *  
#   season11     -44.1863    29.0681  -1.520  0.13801    
# season12      -0.1499    29.0968  -0.005  0.99592    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 37.99 on 33 degrees of freedom
# Multiple R-squared:  0.7666,	Adjusted R-squared:  0.6817 
# F-statistic:  9.03 on 12 and 33 DF,  p-value: 2.522e-07

## Create the forecast for sub-meter 3. Forecast ahead 14 time periods 
XforecastfitSM3 <- forecast(XfitSM3, h=14)
## Plot the forecast for sub-meter 3. 
plot(XforecastfitSM3, main = "Power Consumption 2007-2010 w/ Forecast", ylab = "kWh/month - SM3" , xlab = "Time")




####-- DECOMPOSE ---#####################################
# - 1. Using the ts for SM3 that shows kWh by month over the Jan-07 thru Oct-10 time period, 
# decompose this ts into seasonal, trend, and random components. Also provide a plot for these components.

## Decompose Sub-meter 3 into trend, seasonal and remainder
componentsSM3monthly <- decompose(XtsSM3_070809yearly)
## Plot decomposed sub-meter 3 
plot(componentsSM3monthly)
## Check summary statistics for decomposed sub-meter 3 
summary(componentsSM3monthly)

#- 2. Create a subset that shows kWh by hour over each day during Feb-10. Create a ts object for SM3 and 
# decompose this ts into seasonal, trend, and random components. Also provide a plot for these components. 
# Things to consider: 1) the number of seasonal periods in this month, and 2) the frequency value needed for 
# each of these seasonal periods (you may need to research how to set the frequency argument for seasonal 
# periods less than a year). 
# Note: for each of the above decomposed ts, be sure to include a table (Excel) that shows the respective 
# summary statistics. 
# 
# 
hourly.feb.2010 <- hhpwrDT %>%
  mutate(Month = month(DateTime), Day = day(DateTime), Hour = hour(DateTime)) %>%
  filter(Date >= "2010-02-01" & Date <= "2010-02-28") %>%
  group_by(Month, Day, Hour) %>%  # Group data by Day, Hour
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>%
  filter(!is.na(Day))
##Now have 672 hourly observations for month of Feb 2010

## Create TS object with SubMeter3
tsSM3_hourlyfeb2010 <- ts(hourly.feb.2010$SM3, frequency= 24)

## Decompose Sub-meter 3 into trend, seasonal and remainder
componentshourlyfeb2010 <- decompose(tsSM3_hourlyfeb2010)
## Plot decomposed sub-meter 3 
plot(componentshourlyfeb2010)
## Check summary statistics for decomposed sub-meter 3 
summary(componentshourlyfeb2010)



#################--- HOLT-WINTERS ---################### 
# Need non-seasonal ts for HW. Therefore, create a ts object for SM3 for each of the following 4 seasons: 
# Win-09/10, Spr-10, Smr-10, Fall-10 (thru 11/25). To do this, create a subset that shows kWh by day over 
# each season, then forecast the next 30 days. Plot the fit and forecast objects for each season. 
# (The plot will include the data leading up to the forecast, and the forecast. In the POA, it mentions to 
# plot 'forecast only', but don't worry about plotting only the forecast.) The plot will show the actual data 
# and the forecasted (in red) in the same chart. Note: to create the HW forecast object, you may need to use 
# forecast() I/O forecast.HoltWinters(). You may want to consider using decompose to remove any seasonality 
# that may be present in these ts objects. Be sure to evaluate the residuals using the Ljung-Box test, etc. 
# Refer to the Little Book of R. 

## main dataset for units per day from Dec 09 to Nov 10
season2010 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  
  # Don't add "1" after month (or label) - causes subset class=tibble when subsetting for seasons below 
  filter(Date >= "2009-12-01" & Date <= "2010-11-25") %>%
  group_by(Year, Month, Day) %>%  # Group data by Month, Day
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3), # Total kWh per day
            DateTime = first(DateTime)) %>% 
  filter(!is.na(Day)) # Remove the last row that has NA 
###Now have 360 daily observations###

######WINTER SEASON

#### subset to get kWh by day over Winter 2009-2010 season. 
winter2010 <- season2010 %>%
  filter(Month==12 | Month==1 | Month==2) #90 daily observations

ts_winter2010 <- ts(winter2010$SM3, frequency = 7)
plot(ts_winter2010)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components_winter2010 <- decompose(ts_winter2010)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts_winter2010Adjusted <- ts_winter2010 - components_winter2010$seasonal
autoplot(ts_winter2010Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(ts_winter2010Adjusted))

## Holt Winters Exponential Smoothing & Plot
ts_HW_winter2010 <- HoltWinters(ts_winter2010Adjusted, beta=FALSE, gamma=FALSE) 

# Holt-Winters exponential smoothing without trend and without seasonal component.
# Smoothing parameters:
# alpha: 0.2000043
# beta : FALSE
# gamma: FALSE
# Coefficients:
#   [,1]
# a 11.84378

plot(ts_HW_winter2010)

## HoltWinters forecast & plot (Having created a ts obj containing exponentially smoothed data w/ no seasonality)
HW_winter2010forecast <- forecast(ts_HW_winter2010, h=30, level = c(50, 75))

plot(HW_winter2010forecast, main= "Winter 2009/2010", ylab= "kWh", xlab="Weeks")


######SPRING SEASON

#### subset to get kWh by day over Winter 2009-2010 season. 
spring2010 <- season2010 %>%
  filter(Month==3 | Month==4 | Month==5) #90 daily observations

ts_spring2010 <- ts(spring2010$SM3, frequency = 7)
plot(ts_spring2010)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components_spring2010 <- decompose(ts_spring2010)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts_spring2010Adjusted <- ts_spring2010 - components_spring2010$seasonal
autoplot(ts_spring2010Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(ts_spring2010Adjusted))

## Holt Winters Exponential Smoothing & Plot
ts_HW_spring2010 <- HoltWinters(ts_spring2010Adjusted, beta=FALSE, gamma=FALSE) 

plot(ts_HW_spring2010)

## HoltWinters forecast & plot (Having created a ts obj containing exponentially smoothed data w/ no seasonality)
HW_spring2010forecast <- forecast(ts_HW_spring2010, h=30,level = c(50, 75))

plot(HW_spring2010forecast, main= "Spring 2010 (SM3)", ylab= "kWh", xlab="Weeks")

######SUMMER SEASON

#### subset to get kWh by day over Winter 2009-2010 season. 
summer2010 <- season2010 %>%
  filter(Month==6 | Month==7 | Month==8) #90 daily observations

ts_summer2010 <- ts(summer2010$SM3, frequency = 7)
plot(ts_summer2010)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components_summer2010 <- decompose(ts_summer2010)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts_summer2010Adjusted <- ts_summer2010 - components_summer2010$seasonal
autoplot(ts_summer2010Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(ts_summer2010Adjusted))

## Holt Winters Exponential Smoothing & Plot
ts_HW_summer2010 <- HoltWinters(ts_summer2010Adjusted, beta=FALSE, gamma=FALSE) 
plot(ts_HW_summer2010)

## HoltWinters forecast & plot (Having created a ts obj containing exponentially smoothed data w/ no seasonality)
HW_summer2010forecast <- forecast(ts_HW_summer2010, h=30,level = c(50, 75))
plot(HW_summer2010forecast, main= "Summer 2010 (SM3)", ylab= "kWh", xlab="Weeks")

######FALL SEASON

#### subset to get kWh by day over Winter 2009-2010 season. 
fall2010 <- season2010 %>%
  filter(Month==9 | Month==10 | Month==11) #90 daily observations

ts_fall2010 <- ts(fall2010$SM3, frequency = 7)
plot(ts_fall2010)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components_fall2010 <- decompose(ts_fall2010)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts_fall2010Adjusted <- ts_fall2010 - components_fall2010$seasonal
autoplot(ts_fall2010Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(ts_fall2010Adjusted))

## Holt Winters Exponential Smoothing & Plot
ts_HW_fall2010 <- HoltWinters(ts_fall2010Adjusted, beta=FALSE, gamma=FALSE) 
plot(ts_HW_fall2010)

## HoltWinters forecast & plot (Having created a ts obj containing exponentially smoothed data w/ no seasonality)
HW_fall2010forecast <- forecast(ts_HW_fall2010, h=30, level = c(50, 75))
plot(HW_fall2010forecast, main= "Fall 2010 (SM3)" , ylab= "kWh", xlab="Weeks")


###########################################################################################
###BELOW IS THE CODE AND STEPS IN THE POA.  #####
####Holt-Winters Forecasting####
#To make forecasts using simple exponential smoothing, you can fit a simple exponential smoothing predictive
#model using the HoltWinters() function from the stats package for R. 

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot (Having created a ts obj containing exponentially smoothed data w/ no seasonality)
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
