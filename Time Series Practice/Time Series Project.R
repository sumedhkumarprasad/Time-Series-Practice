rm(list=ls())
# Set Working Directory 
setwd("D:/Great Lakes PGPDSE/Great Lakes/12 Time Series Forecasting/Mini Project")
#Read Data file in CSV format
# first row is deleted from data in excel and from excel format conveted into the csv format
# plotted the graph in the excel for item a and item b
demand=read.csv("DemandAB.csv")
head(demand)
summary(demand)

# Total 187 observation with 4 Variables.
View(demand)
# 4 variables names is year , Month , item a and item b
# 2 missing values is there one in item a and other one in item b
# in item a missing value is at Dec 2011
# in item b missing value is at Sep 2005
str(demand) 

# for na value treatment import imputeTS package and missing value treatment 
library(imputeTS)
# On the basis of na.interpolation
a=ts(demand[,3],start = c(2002,1),frequency = 12)
demand$itema=na.interpolation(a,option="spline")
b=ts(demand[,4],start=c(2002,1),frequency=12)
demand$itemb=na.interpolation(b,option = "spline")


#For Item A there is a overall increase in the seasonality from January to August after that decrease then again increase.

#For Item B is a overall decrease in the seasonality from January to October after that there is a increase in the seasonality for Nov and Dec. 


#View(demand)
# item a plot and time series
itema <- ts(demand[,3], start=c(2002,1), frequency=12)
par(mfrow=c(1,2))
plot(itema, main= "itema plot")
# Plot for seasonality
monthplot(itema, main="itema seasonality")
#insight from the graph
# from this graph it is clear that there is a trend and seasonality is there
# Graph is looks like multiplicative in nature and increasing trend and seasonality.
# in seasonality there is high variation in the Nov. and Dec.  Month.
# for the initial period there is not much variation is there Feb to July.

# item b plot and time series
itemb <- ts(demand[,4], start=c(2002,1), frequency=12)
par(mfrow=c(1,2))
plot(itemb, main= "itemb plot")
# Plot for seasonality
monthplot(itemb, main="itemb seasonality")
# insight from the graph 
# it is a multiplicative in nature 
# there is a descreasing trend ad Seasonality.
# For each month Jan to June there is a descreasing seasonality 
# In the month of July and August there is a high seasonality is there.
# Season variation are changing across year.

# Seasonality plot for Item A and B plot for seasonality and the not trend
par(mfrow=c(1,2))
monthplot(itema,main="item A Seasonality plot")
monthplot(itemb,main="item B Seasonality plot")



# For Item A monthplot graph interpretatation
# Here item A is showing a seasonality but not sure whether there is a trend or not. 
# It is showing properties of addictive series till 2010, but after 2010 it is showing more variance and showing some positive trend.
# So item A can be considered as multiplicative series.
# Here the average values for months February to August is almost same. But in November and December it increases and reaches the peak. Also the graph is showing some variances in most of the months. It showing highest variation in December. Also it has some overall increasing demand .

# For item B monthplot graph interpretation
#Item B is showing both seasonality and negative trend.  
#Here the Item B follows multiplicative series.
#Here the averages are increasing from January and reaches the peak at june and again reduces. Here in june, july and august the demand for B is high. But it have a high variance in all the months. From the graph it is clear that the demand for B is reducing over years.


# Analysis of a multiplicative series for item a
# considering item a as multiplicative
logitema <- log(itema)
itemadec <- stl(logitema,s.window="p")
plot(itemadec)
# trying with window width 3 in item a
itemadec1 <- stl(logitema,s.window=3)
plot(itemadec1,main="Decompose of item a") 
# there is 2 residual near to the end which is not much concern to me beacuse remainder < trend+seasonality 
# there is a increasing trend 
# Compare to constant window width "p" now with window size = 3 residual has been reduced.
# After 2012 there is increase in seasonality variation in the data.
# From the addictive decomposition plot we can see that seasonality component have more importance than trend and reminder is much less compared to trend and seasonality.
plot(itemadec1$time.series[,2],main='TREND FOR DEMAND A YEARLY WISE',col='red') ## trend

plot(itemadec1$time.series[1:12,2],main='TREND FOR DEMAND A MONTHLY WISE',col='BLUE',type='l')
#Conclusion: There is an overall increase in trend for the demand A from the year 2002 to 2017.It means that the demand for the consumable item A increases over the period of time.

plot(itemadec1$time.series[,1],ylab='DEMAND_A_SEASONALITY',col='pink')
#In which month(s) do you see higher sales and which month(s) you see lower sales for Demand A?
#Ans) Heigher Sales - DECEMBER (Average  Demand  Around 4400)
#Lower Sales  -  JANUARY    (Average Demand  Around 2300)

# item a decompose  season with seasonality and trend of item a
 
itemadseason <- exp(itemadec1$time.series[145:187,1])
plot(itemadseason,type="l", main="item a with two series Seasonality separately")
# item a decompose  season with seasonality of item a
itemadtrend <- exp(itemadec1$time.series[145:187,2])
plot(itemadtrend,type="l", main="item a with two series trend separately")

# for itemadseason and itemadtrend form 2014 Jan till 2017 July
par(mfrow=c(1,2))
plot(itemadseason,type="l", main="item a with two series Seasonality separately")
plot(itemadtrend,type="l", main="item a with two series trend separately")


# from the above plot it is clear that graph of trend is increasing 
# there is a high seasonal variation during the month of Nov. and Dec. 

# item a decompose  season with seasonality and trend of item b

# Analysis of a multiplicative series for item b
# considering item a as multiplicative
logitemb <- log(itemb)
itembdec <- stl(logitemb,s.window="p")
plot(itembdec)
# trying with window width 3 in item b
itembdec1 <- stl(logitemb,s.window=3)
plot(itembdec1,main="Decompose of item b")
#In which month(s) do you see higher sales and which month(s) you see lower sales for Demand B(consumable item B) ?
#Ans) Heigher Sales - JULY (Average Demand Around 4000)
#Lower Sales  -  JANUARY (Average Demand Around 1800)

DemandB_Seasonality<- exp(itembdec1$time.series[,1])
plot(DemandB_Seasonality, type="l",col='red',main = 'Seasonality PLOT FOR DEMAND B Monthly Wise')

# from the graph it is clear that there is a few large residual during the initial period but is it not impacting much.
# beacuse remainder < trend + seasonality there is not much impact of the residual to the model.
# The graph shown with the descresing trend
# Seasonailty also descreases with after 2010.
itemb_trend<- exp(itembdec1$time.series[,2])
plot(itemb_trend, type="l",col='purple',main = 'TREND PLOT FOR DEMAND B Yearly Wise')
itemb_trendd<- exp(itembdec1$time.series[,2])
plot(itemb_trendd, type="l",col='purple',main = 'TREND PLOT FOR DEMAND B Monthly Wise')
#Conclusion: There is an overall decrease in trend for the demand B from the year 2002 to 2017 but from the year 2015(August) onwards there is slightly increase in trend .
#It means that the demand for the consumable item B decreases  over the period of time (2002 to 2015) and then demand started increasing slightly.

itemb_seasonality<- exp(itembdec1$time.series[,1])
plot(itemb_seasonality, type="l",col='red',main = 'Seasonality PLOT FOR DEMAND B Monthly Wise')

# item a decompose  season with seasonality and trend of item b

itemddseason <- exp(itembdec1$time.series[1:36,1])
plot(itemddseason,type="l", main="item b with 3 series Seasonality separately")
# Seasonality with 3 series from Jan 2002 to Dec 2004


# item a decompose  season with seasonality of item b
itembdtrend <- exp(itemadec1$time.series[1:36,2])
plot(itembdtrend,type="l", main="item b with 3 series trend separately")

# for itemadseason and itemadtrend form 2002 Jan till 2004 Dec
par(mfrow=c(1,2))
plot(itemadseason,type="l", main="item a with two series Seasonality separately")
plot(itemadseason,type="l", main="item a with two series trend separately")

# from the above plot it is clear that graph of trend and seasonlity both are same from Jan 2014 to July 2017
library(forecast)
 #checking  for moving average item a 

#
# moving average with window width 17 showing the smooth curve for item A
itema7 = ma(itema, order = 7)
itema9 = ma(itema, order = 9)
itema13 = ma(itema, order = 13)
itema17 = ma(itema, order = 17)
itema21= ma(itema,order=23)
ts.plot(itema,itema7,itema9,itema13,itema17,itema21,lty=c(1:6),
col=c('black','red','green','blue','yellow','brown'), main = "TS plot for item A Moving average ")

# checking for moving average itme b
itemb7 = ma(itemb, order = 7)
itemb9 = ma(itemb, order = 9)
itemb13 = ma(itemb, order = 13)
itemb17 = ma(itemb, order = 17)
itemb21= ma(itemb,order=23)
ts.plot(itemb,itemb7,itemb9,itemb13,itemb17,itemb21,lty=c(1:6),
col=c('black','red','green','blue','yellow','brown'),main = "TS plot for item B Moving average ")
# moving average with window with 9 optimal compares to others.

#Now considering item A as multiplicative Series
mitema<-decompose(itema, type = "m")
plot(mitema)
mitema$figure
plot(mitema$figure, type = "l",main = "Seasonal Demand Item A")
# Overall there is a increase in Seasonal graph year to year 
#/* For item A, average sales is highest in November and December of every year and it is lowest in January. Here the average sales gradually increases from January to December even though there is dip in months April, June and September. Also the demand of item A slightly increasing over years from 2010.*/

#Item B multiplicative decomposition 
#Here demand of item is multiplicative. So taking multiplicative decomposition model.
mitemb<-decompose(itemb, type = "m")
plot(mitemb)
mitemb$figure
plot(mitemb$figure, type = "l",main = "Seasonal Demand Item B")
# interpretation of the item
# From the above graph it is clear that average sales increases from January and reaches its peak at July and then decreases up to October and again raises in December. Here sales is lowest in January. For item A also sales was lowest in January. But for item B, the demand is gradually decreasing over years, from the trend graph it is clear. That is when comparing the demand of A & B we can see that the demand of item is slightly increasing in each year and demand of B gradually decreasing.

#Checking the residuals for both decompositions for Item A and Item B
plot(remainder(mitema), main = "Residuals Item A")
#Interpertation of the graph : Here the residuals doesn't follow any particular pattern. It suddenly increases and decreases without showing any pattern.It looks like residual is Random in nature. 

plot(remainder(mitemb), main = "Residuals Item B")
#Interpertation of the graph : Residuals of B also not showing any pattern and it doesn't have much contribution in the entire time series model.There is asudeen dip in the residual before 2005.Residual is moving under specific band approx 08 to 1.2 values.

# Dividing a time series into train and test

# For item A (Dividing a time series into train and test)
atrain <- window(itema, start=c(2002,1), end=c(2015,10),frequency=12)
atest <- window(itema, start=c(2015,11),frequency=12)
#Made last 21 months data for testing purpose.

# For item B (Dividing a time series into train and test)
btrain <- window(itemb, start=c(2002,1), end=c(2015,10),frequency=12)
btest <- window(itemb, start=c(2015,11),frequency=12)
#Made last 21 months data for testing purpose.

#Considering item A and item B both as multiplicative model

# Among all three method 1) Simple exponential smoothing model(ses) , 2) holt ( Double Exponential Smoothing model) , 3) Holt Winter  model
# Hotl Winter model gives the better results beacuse it have three smoothing parameters 
# aplha = level/ randomness/ residuals / remainders
# beta= trends in the model
# gamma= seasonality in the model
# So alpha beta and gamma helps to decide the smoothness of the model.

#SIGNIFICANCE OF THE FOLLOWING PARAMETER IN MODELLING:

# Alpha=  for the estimate of the level at the current time point .
#Value of smoothing parameter for the level.
#Beta= for the estimate of the slope b of the trend component at the current time point
#Value of smoothing parameter for the trend.
#Gamma= Value of smoothing parameter for the seasonal component.

# So Holt winter model covers the all three smoothing parameters and gives better results copares to the others.
atrain.fc = hw(atrain, seasonal = 'm', h=21)# forecast for 21  periods
atrain.fc
plot(atrain.fc)
names(atrain.fc)
# for cehcking the Holt winter odel parameters 
atrain.fc$model
# from this model alpha, beta and gamma value is given below
#Smoothing parameters:
#alpha = 0.108 
#beta  = 0.004 
#gamma = 1e-04

Vec<- cbind(atest,atrain.fc$mean)
ts.plot(Vec, col=c("blue", "red"), main="Monthly Sales of A: Actual vs Forecast")

# Calculating the MAPE
MAPE <- mean(abs(Vec[,1]-Vec[,2])/Vec[,1])
MAPE  # MAPE for this model is coming up with 13.67%

# Checking for the second model on atest data ( second model)
atrain.fc1 = hw(atrain, seasonal = 'm',h=21,alpha=0.09,beta=0.04,gamma=0.3)
# for this holt winter model
# alpha =0.09
# beta=0.04
# gamma=0.3
plot(atrain.fc1)# dark shawdow show 80 % C.I and lighter shawdow show 95% C.I.

Vec1<- cbind(atest,atrain.fc1$mean)
ts.plot(Vec1, col=c("blue", "red"), main="Monthly Sales of Item A: Actual vs Forecast")
# this plot shows that Actual and forecast are overlapping to each other there is hardly difference between actual and forecasted value.
MAPE <- mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])

MAPE # MAPE 6.68% which descrease from 13.67% which shows there is reduction in the error from the first( inital model)

#Applying holt winters method,
btrain.fc = hw(btrain, seasonal = 'm',h=21)
btrain.fc
plot(btrain.fc)

btrain.fc$model
#From the above model Smoothing parameters are:
#Smoothing parameters:
#alpha = 0.0225 
#beta  = 0.0013 
#gamma = 1e-04

# Plotting the graph for Item B
Vec2<- cbind(btest,btrain.fc$mean)
ts.plot(Vec2, col=c("blue", "red"), main="Monthly Sales of Item B: Actual vs Forecast")
#Here the actual and forecasted data are showing bit difference. So adjustment of smoothing  parameters are required.
MAPE2 <- mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
MAPE2 # 10.78% initial model MAPE is there so fine tunning is requried.

# Smoothing parameter tunning of Item B
btrain.fc1 = hw(btrain, seasonal = 'm',h=21,alpha=0.2,beta=0.13,gamma=0.17)
plot(btrain.fc1)# from the graph it is clear that as the no. of period increases Confidence interval also increases chance of error in the forecast also increases
Vec3<- cbind(btest,btrain.fc1$mean)
ts.plot(Vec3, col=c("blue", "red"), main="Monthly Sales of Item B: Actual vs Forecast")
# As actual and forecsted is overlapped aprroximately to each other compared to the previous plot this model show good fitting of the curve.
MAPE3 <- mean(abs(Vec3[,1]-Vec3[,2])/Vec3[,1])
MAPE3 # 8.18% of MAPE is obtained from previous model.Earlier it was coming around 10.78%.There is a reduction in the error.

# Comparison between the Actual vs Forecast for  Item A and Item B
#Here the model for item A demand gives MAPE of 6.68% and model for item B demand gives MAPE of 8.18%. Comparing the MAPE s of two models, model for item A is giving better performance. But comparing the actual v/s forecasted graphs of A and B we can find that, forecasted values of A are more equally distributed around the actual value.

# Forecasting of item A with 17 periods from July 2017 to Dec 2018
itema.fc<-hw(itema, seasonal= 'm',h=17,alpha=0.09,beta=0.04,gamma=0.3)
itema.fc
plot(itema.fc)
#From the graph it is clear forecasting of data from August 2017 to Dec 2018 is good beacuse there is a maximum overlap of Confidence interval of 80% and 95% .The dark shadow show 80% C.I. and light shadow show 95% C.I. and there is a maximum overlap between two which shows that it is a good forecasting model.
# If we see the forecasting value for year Nov 2017 and Dec 2017 shows higher value for year 2017.
# Similarly if we see the forecsting value for year Nov 2018 and Dec 2018 shows higher value for year 2018.
# As the past data also says that there is more demand of Item A in the month of November and December in any year.
# So as a Store manager I will plan to keep more stock of item A in November and December to meet the excess customer demand during these months.
# In the month of Jan 2018 sales is very low we have three option 1) Give some promotional offer on item A in month of Jan. 2)Increase the sales campagin for the month of Jan. 3)Keep less stock or procure less material for Item A.


# Forecasting of Item B.
itemb.fc = hw(itemb, seasonal = 'm',h=21,alpha=0.2,beta=0.13,gamma=0.17)
itemb.fc
plot(itemb.fc)
#From the graph it is clear forecasting of data from August 2017 to Dec 2018 is good beacuse there is a maximum overlap of Confidence interval of 80% and 95% .The dark shadow show 80% C.I. and light shadow show 95% C.I. and there is a maximum overlap between two which shows that it is a good forecasting model.
#If we see the forecasting value for year Jan 2018 and Feb 2018 shows low values and this trend is repeated for next forecasted year and also same trend is also replicate on the historical data.
#So as a Store Manger There is more demands of item B during July, Aug and Dec 2018 So keep the extra inventory of material during thesr periods.
#From the forecasted data we can find that, the sales of item B is likely to go down in Jan and Feb 2018, so can plan for giving some promotional offer for item B in these month to increase the sales.
#After Feburary sales may go up as per forecasting, so have to stock keep more stock of item B after Feburary to meet the increasing customer Demand.
#In the month of Jan and Feb to incease the sale we have following option 1) Give Some promotion offer during this month( Jan and Feb) 2) Increase the sale campagin for the month of Jan and Feb. Keep less stock during these month ( Jan and Feb).
