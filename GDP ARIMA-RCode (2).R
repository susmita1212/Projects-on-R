
# STEP 1 ###############################


# Step 1 - First Import the dataset:
#   Environment on right top corner > Import Dataset > Import Excel 
#   > Browse > Rename the file to the name you feel right 
#   > Click on Import

setwd("F:/Business Analytics/R Code And Related Content/ARIMA/GDP Dataset")
getwd()

gdp <- read_excel("Time Series GDP Dataset.xlsx")
View(gdp)



# STEP 2 ###################

# Step 2 - We'll see if R-Studio is considering imported dataset as
# Time-Series or not?
class(gdp)

# STEP 3 ########################

# Step 3 - If you find dataset is not a time series. 
#   Then we'll need to convert it into Time-Series.

gdptime <- ts(gdp$GDP, start = min(gdp$DATE), end = max(gdp$DATE), frequency = 4)

# What we have done in the above command.
# We converted gdp dataset from dataframe to a Time-Series Dataset.
# ts() command is used to convert dataset to a Time-Series Dataset.
# ts() command took 4 parameters as follows   
# First i.e., gdp$GDP which is a column that we are interested in
# converting into TimeSeries.

#     Second parameter, we specified the starting point of the
#     time series which is done by giving the first value which 
#       we are interested in 
#       which should be the smallest value for the date.

#     Similarly third parameter is to specify the last value of the
#     dataset which should be the biggest date in the dataset.

#     Fourth Parameter defines the frequency of the data in terms 
#     of time.
#     
#     If data is yearly, frequency will be 1 i.e., 12/12
#     If data is Biannual, frequency will be 2 i.e., 12/6
#     If data is Quarterly, frequency will be 4 i.e., 12/3
#     If data is monthly, frequency will be 12 i.e., 12/1
#     If data is daily, frequency will be 365



# Now Let's see if the new dataset that we have created
# is a TimeSeries or not

class(gdptime)


# Now as we have to work on time series dataset we need commands
# that are used to manipulate timeseries, so we'll import a library
# called teseries.

# We'll also need to forecast data as it's our prime purpose for
# working on Time Series. So we'll import a library called forecast

library(tseries)
library(forecast)

# STEP 4 #################

# Step 4 - Now before doing anything obviously we want to see how our
# data looks like so we'll plot the data (as a graph).

plot(gdptime)

# STEP 5 ##################

# Step 5- Now we have to check if the Time Series is stationary
# or not, as we cannot do ARIMA Modelling on Non-Stationary data.

# So first we'll check the auto-correlation, which is as follows

acf(gdptime)

# Now as we see the graph there are 2 blue lines.
# Now we have to check if there are spikes going beyond the two blue
# line. This shows that data has high autocorrelation.
# This shows that data is not stationary.

# STEP 6 #####################

# Step 6-  Now another check for stationarity that we need to do is
# PACF test. It works in a similar way to ACF. If spikes goes beyond
# blue lines, that implies that data is stationary.


# (Same for both ACF and PAcF)
# If there are more spikes, then data is non stationary.
# If there are less spikes, then data is Stationary.

pacf(gdptime)

# STEP 7 #################

# Step 7 - Now we'll do another check for stationary.
# that is Dickey-Fuller Test. We can do it through adf.test command

adf.test(gdptime)

# Here in the console we'll see if the p-value in the summary of
# dickey-fuller test is less than 0.05 (significance level).
# If it is less than 0.05, then the data is stationary.
# otherwise it is non-stationary.



# Now luckily we have a function called auto.arima in R which will
# make the data stationary for us as well as find out the
# fittest model for us.

# STEP 8 ##################

# Step 8 - We'll make the data stationary and find out the fittest
# model for our time series with auto.arima function.

gdpautomodel <- auto.arima(gdptime, ic = 'aic', trace = TRUE)

# We can see in the summary in the console, that best model for 
#our Time Series is ARIMA(0,2,2)
# It has chosen the best model for our TimeSeries on the basis of
# AIC values which are listed in the summary next to their
# corresponding ARIMA models.

# The best model will be the one with lowest AIC value.

# What is AIC?? What are AIC values?


# The Akaike information criterion (AIC) is an estimator of
# prediction error and thereby relative quality of statistical
# models for a given set of data. Given a collection of models
# for the data, AIC estimates the quality of each 
# model, relative to each of the other models.


# Let's see this fittest model.

gdpautomodel

# STEP 9 ######

# Now we'll check if the best model that auto.arima
# gave us is stationary or not?
# WE'll do ACF & PACF on this Model to check stationarity
# as we did before.


acf(ts(gdpautomodel$residuals))
pacf(ts(gdpautomodel$residuals))

# We can see both acf and pacf graph. Almost all the spikes are
# between the blue lines, Only 2 in ACF and only 1 in PACF.
# This shows that the our model is stationary.

# STEP 10 #####

# Now we can forecast for our model for future.

gdpforecast <- forecast(gdpautomodel, level = c(95), h = 10*4)

# Here we have forecasted the values for next 10 years.
# Let's see the parameters we have given:
# first we'll give the name of the model we want to forecast.
# level: level defines the confidence level that we have taken 95.
# h: h defines the number of datapoints upto which you want to
# forecast. As here we want to forecast for next 10 years which
# will be quarterly forecast as the data is quarterly in nature.
# So it will be total 10*4 data points.

# Now lets see our data:

gdpforecast

# Here we have accessed data from start upto next 10 years.
# There are three values corresponding to each quarter.
# First value that is forecasted value. which is most probable
# value according to the forecast
# The next 2 values are Lower and Upper limits for our forecasted
# values with confidence level of 95%, which shows that there is
# 95 % chance that our value will lie between upeer and lower limit.
# for a given instance (quarter).

#Let's see how our forecast look like (with graph).


plot(gdpforecast)

# Here the dark blue line shows the most probable forecasted values.
# While the light blue area shows the region between the upper and 
# lower limit for our forecasted values with 95% confidence level.


# STEP 11 #####

# Now we have to validate our forecast.
# For that we need to do box test.

Box.test(gdpforecast$residuals, lag = 5, type = "Ljung-Box")

# So here we have three arguments:
# first we have taken residual values from gdpforecast
# lag: represents the degree for freedom.
# type: kind of box test you want to do: Box-Pierce or Ljung-Box

# Now in the console we can see summary of box test.
# df is degree of freedom
# Here we have to again focus on p-value.
# P-value always should be more than 0.05 (significance level)
# which is opposed to above case (adf test).
# We check p-value here to see if there is still
# autocorrelation in the Model.
# If p-value is more than significance level, then there is no
# autocorrelation.
# If p-value is less than significance level, then there is still
# aurtocorrelaton.

# Now we need to check the same for different lag values(degree
# of freedom) to see if it is true for all of them or not.
# Sometimes, There might be the case that p-value is less than
# significance level for one or more lag values.

# Now let's check it for different values of lag.


Box.test(gdpforecast$residuals, lag = 15, type = "Ljung-Box")
Box.test(gdpforecast$residuals, lag = 20, type = "Ljung-Box")
Box.test(gdpforecast$residuals, lag = 30, type = "Ljung-Box")
Box.test(gdpforecast$residuals, lag = 35, type = "Ljung-Box")


# We can see in all the cases the p-value is always more than the
# significance level, which shows that model does not have
# autocorrelation and thus data is stationary.










