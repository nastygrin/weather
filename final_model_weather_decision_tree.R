# Author: Nishkarsh Gautam
# Weather of capital city of Gujarat : Gandhidham for next 14 days
# the dataset will contain following columns :

# temp - the current temperature at Gandhinagar in Kelvin
# temp_min - the minimum temperature in Kelvin
# temp_max - the maximum temperature in Kelvin
# feels_like - the apparent temperature which is felt by the people
# humidity - the humidity at the place in the percentage %
# visibilty - the average visibilty in meters
# Pressure - Atmospheric pressure on the sea level in hPa
# wind_speed - speed of the wind in meter/sec
# wind_deg - the direction of the winf the degrees
# all - contains the cloudiness percentage %
# lat - the latitudinal coordinates to the city
# lon - the longotudinal coordinates to the city
# sunrise - the time of sunrise in UTC unix
# sunset - the time of sunset in UTC unix
# dt - time of data UTC unix
# name - name of the city
# weather.main - the type of weather at that point (example- smoke, cloud)


# Adding the required libraries
library(utils)
library(rpart)
library(rpart.plot)

# Reading the data from the csv file from the github
data <- read.csv(("https://raw.githubusercontent.com/nastygrin/final-dataset/main/19BDS0048.csv"))
head(data)

# DATA PRE-PROCESSING

# Changing temeprature from Kelvin to Celcius

data$main.temp <- data$main.temp-273.15  
data$main.feels_like <- data$main.feels_like-273.15
data$main.temp_max <- data$main.temp_max-273.15
data$main.temp_min <- data$main.temp_min-273.15

# Removing the unnecessary columns to simplify the dataset
df=subset(data, select=-c(weather.description,weather.icon,base,sys.country,timezone,id,cod,sys.type))
head(df)

# Fit model Decision Tree
dtr_model <- rpart(main.temp~., data=df, method="anova", minsplit=5)
dtr_model

# Prediction: Decision Tree
prediction = predict(dtr_model, data.frame(coord.lon=72.6833, coord.lat=23.2167, weather.id=711, weather.main="Smoke", main.feels_like=301.65, main.temp_min=303.16, main.temp_max=303.16, main.pressure=1009, main.humidity=26, visibility=10000, wind.speed=5.13, wind.deg=70, all=78, sys.id=9049, sys.sunrise=1637025825, sys.sunset=1637065472, name="Gandhinagar", dt=1637050245))
prediction

# plot tree
prp(dtr_model, nn=T)


library(e1071)
library(caTools)

test_normal_sample <- df[1:50,]
split <- sample.split(df, SplitRatio = 0.67)
train_data <- subset(df, split == "TRUE")
test_data <- subset(df, split == "FALSE")

y_pred <- predict(dtr_model, newdata=test_data)
y_pred

observed_data <- test_data$main.temp
predicted_data <- y_pred
mean(observed_data == predicted_data)
logic <- (observed_data == predicted_data)
num <- as.numeric(logic)
mean(num)


library(caret)

# MAE (Mean absolute error) represents the difference between the original and predicted values extracted by averaged the absolute difference over the data set.
MAE(predicted_data, observed_data)

# MSE (Mean Squared Error) represents the difference between the original and predicted values extracted by squared the average difference over the data set.
# MSE(predicted, original)

# RMSE (Root Mean Squared Error) is the error rate by the square root of MSE.
RMSE(predicted_data, observed_data)

# R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
R2(predicted_data, observed_data, form = "traditional")

# Bar plot of temeperature variation in Gandhinagar city over 14 days
barplot(df$main.temp,names.arg=df$dt,xlab="time in unix",ylab="temperature",
        main="temperature variation chart over 14 days in Gandhinagar")

# Bar plot of humidity variation in Gandhinagar city over 14 days
barplot(df$main.humidity,names.arg=df$dt,xlab="time in unix",ylab="humidity",
        main="humidity % variation in Gandhinagar over 14 days")

# line plot of humidity variation in Gandhinagar city over 14 days
plot(df$all, type="o", names.arg=df$dt,xlab="time in unix",ylab="cloudiness",
        main="cloudiness % variation in Gandhinagar over 14 days")