---
  title: "Wine Magazine Technical Interview"
author: "Keri Plevniak"
date: "10/12/2018"
output: html_document
---

winemag_data_pandata <- read.csv("~/Documents/Job Applications/TechnicalInterview/Pandata/winemag_data_pandata.csv")

#Building a linear model predicting points from price

#Visualize the data


hist(winemag_data_pandata$points, main="Histogram of Points for wine\nReviewed in WineEnthusiast", xlab = "Points (1-100)")
hist(winemag_data_pandata$price, main="Histogram of Prices of Wine\nReviewed in Wine Enthusiast", xlab = "Price (in Dollars)")


#Check to see if there are any outliers


boxplot(winemag_data_pandata$points, ylab="Wine Points", main="Boxplot of Wine Points")
boxplot(winemag_data_pandata$price, ylab="Wine Price", main="Boxplot of Wine Price")

#From the data visualization it looks like the price of wine is skewed 

# without Transforming the data

#we can look at the data in a scatter plot

plot(winemag_data_pandata$points, winemag_data_pandata$price, xlab="Price", ylab="Points", main = "Points vs Price of Wine")

#it looks like a price increases the point value of the wine also increases. However, its difficult to see the relationship between the variables from this graph.

#we can check to see if there is a correlation between the variables price and points

cor(winemag_data_pandata$price, winemag_data_pandata$points, use ="complete.obs")

#There is somewhat of a correlation between Wine price and the points the wine recieves from Wine Enthusiast

#We can then model this relationship with a regression

#First make a dataframe

WinePricePointsDF<-as.data.frame(winemag_data_pandata[,5:6])


#make a regression model


winepricepointmodel<-lm(points~price, data=WinePricePointsDF)
summary(winepricepointmodel)

#the R^2^ of this model is 0.1762 which is very low. This means that only about 17 % of the varation can be explaed by the model. The p-value is significant (p-value<.001) which suggests that the models regusts are statistically significant. 

#However this might not be the best model since the data we used from price is skewed and will need to be normalized. 

# Log transforming the Price of Wine

#Log transforming price will normalize the distribution of price so that the data meet the assumptions of a linear model


winemag_data_pandata$LogPrice<-log(winemag_data_pandata$price, use="everything")

#Visualize the log transformed price of wine


boxplot(winemag_data_pandata$LogPrice, ylim=c(0,10), main="Log of Price of Wine", ylab="Price of Wine (log transformed)")


#The box plot shows that the data is more normally distributed then without a log transformation

#We can plot the data to see the relationship

plot(winemag_data_pandata$LogPrice, winemag_data_pandata$points, main = "Price (log transformed) vs Points of Wine", ylab="Price (log transformed)", xlab = "Points" )

#this graph looks like there is a better correlation to be able to predict points from price

#we can try to see how well the data correlates with transforming the data


cor.test(winemag_data_pandata$price, winemag_data_pandata$LogPrice, use="complete.obs")

#Using the log transformed data has a better correlation between price and points

#now we can model the data

#First make a dataframe for the model

WineLogPriceDF<-as.data.frame(winemag_data_pandata[,c(5,15)])

WineLogPriceModel<-lm(points~LogPrice, data=WineLogPriceDF)
summary(WineLogPriceModel)

#we can plot the model as well

plot(winemag_data_pandata$LogPrice, winemag_data_pandata$points, main = "Price (log transformed) vs Points of Wine", ylab="Price (log transformed)", xlab = "Points" )
abline(WineLogPriceModel)

# we can see from this model that the log tansformed price and can predict points of wine recieved from Wine Enthusiast Magazine 