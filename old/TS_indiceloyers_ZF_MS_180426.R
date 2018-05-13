###Time Series Project 21.5.18


library(itsmr)
library(tseries)
library(forecast)

#importation of data (quarterly indice des loyers, y1993-2017)

#indiceloyers1993quarterly<-read.csv("C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1993quarterly.csv", header = TRUE, sep = ";")
#save(indiceloyers1993quarterly, file = "C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1993quarterly.RData")

#data 1993-2017 quarterly (100 obs.)
load("C:/Users/Spörri/Desktop/Marc UniNE/SS 18 Time Series Analysis/TS Project/indiceloyers1993quarterly.RData")

indiceloyers <- indiceloyers1993quarterly #load indiceloyers1993quarterly Data (100 Observations)


#formating and data cleansing
indiceloyers.vector <- as.vector(t(indiceloyers)) #transposing first the matrix, then vectorizing in order to transform into a time series object
indiceloyers.vector <- indiceloyers.vector[2:101]  #data should be reduced to [2:101] in order to have a proper dataset without missing values  (=100 observations, beginning in the 3rd quarter)
indiceloyers.train <-  indiceloyers.vector[1:65]  #for further data testing and cross-validation purposes creating a training set consisting of around 2/3 of the observations
indiceloyers.train <- as.numeric(indiceloyers.train)
indiceloyers.train <- ts(indiceloyers.train,start=c(1993,2),end=c(2009,2),freq=4)
indiceloyers.test <-  indiceloyers.vector[66:100] 
indiceloyers.test <- as.numeric(indiceloyers.test)
indiceloyers.test <- ts(indiceloyers.test,start=c(2009,3),end=c(2018,1),freq=4) #for further data testing and cross-validation purposes creating a test data set consisting of around 1/3 of the observations
indiceloyers.vector <- as.numeric(indiceloyers.vector)
indiceloyers <- ts(indiceloyers.vector,start=c(1993,2),end=c(2018,1),freq=4)  #in the case of the 1993-2017 start=c(1993,3)

plot(indiceloyers,xlab="year",ylab="rent index (basescore = 100 (year 1993))")
plot(indiceloyers.train,xlab="year",ylab="rent index (basescore = 100 (year 1993))")
plot(indiceloyers.test,xlab="year",ylab="rent index (basescore = 100 (year 1993))")

#since there is fore sure a positive trend and the values are kinda increasing exponentially we could try to choose a logarithmic model

#implementing a seasonal model to check for seasonal effects
season.model <- model.matrix(~ 1 + factor(cycle(indiceloyers)))[, -1] 
dimnames(season.model)[[2]] <- c("2nd quarter","3rd quarter","4rd quarter")
summary(lm(indiceloyers ~ season.model)) 
t <- 1:length(indiceloyers) 
summary(lm(indiceloyers ~ t + season.model))
t2<-t^2	
summary(lm(indiceloyers ~ t+t2 + season.model))
#there is no seasonal effect

#linear, quadratic, cubic and loglinear regressions
t <- 1:length(indiceloyers) 
linear.trend <- lm(indiceloyers~t)  #lin
summary(linear.trend) 
t2<-t^2		
quadratic.trend <- lm(indiceloyers~t+t2) 
summary(quadratic.trend) 
t3<-t^3		
cubic.trend <- lm(indiceloyers~t+t2+t3)
summary(cubic.trend) 
t4<-t^4		
fourthdegree.trend <- lm(indiceloyers~t+t2+t3+t4)
summary(fourthdegree.trend) 

#to check if the values are increasing kinda exponentially we try as well a logarithmic model 
log.trend <- lm(indiceloyers~log(t)) 
summary(log.trend) 


##2. remove the trend and seasonal component to get stationary residuals
#removing trend when there is no seasonality, p.25/46
#plot(linear.trend$residuals)
#abline(h = 0, lwd = 2)
resid.linear.trend <- ts(residuals(linear.trend), end=c(2018,1), freq=4)   #in the case of the 1993-2017 start=1993
sd(resid.linear.trend)
Box.test(resid.linear.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.linear.trend, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(resid.linear.trend) #small p-values suggest that the series is not stationary and a differencing is required.
plot(resid.linear.trend,type="l", xlab="year", ylab="Residuals")
abline(h = 0, lwd = 1)
acf(resid.linear.trend, lag.max = 40)
pacf(resid.linear.trend, lag.max = 40)
#the residual time series >t clearly shows temporal dependence, and would badly be modelled by an iid noise

#plot(quadratic.trend$residuals)
#abline(h = 0, lwd = 2)
resid.quadratic.trend <- ts(residuals(quadratic.trend), end=c(2018,1), freq=4) #in the case of the 1993-2017 start=1993
sd(resid.quadratic.trend)
Box.test(resid.quadratic.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.quadratic.trend, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(resid.quadratic.trend)  #small p-values suggest that the series is not stationary and a differencing is required.
plot(resid.quadratic.trend,type="l", xlab="year", ylab="Residuals")
abline(h = 0, lwd = 1)
acf(resid.quadratic.trend, lag.max = 40)
pacf(resid.quadratic.trend, lag.max = 40)

resid.cubic.trend <- ts(residuals(cubic.trend), end=c(2018,1), freq=4) #in the case of the 1993-2017 start=1993
sd(resid.cubic.trend)
plot(resid.cubic.trend,type="l", xlab="year", ylab="Residuals")
abline(h = 0, lwd = 1)
Box.test(resid.cubic.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.cubic.trend, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(resid.cubic.trend)  #small p-values suggest that the series is not stationary and a differencing is required.
acf(resid.cubic.trend, lag.max = 40, main="ACF Cubic model")
pacf(resid.cubic.trend, lag.max = 40, main="PACF Cubic model")

#plot(log.trend$residuals)
resid.log.trend <- ts(residuals(log.trend), end=c(2018,1), freq=4) #in the case of the 1993-2017 start=1993
sd(resid.log.trend)
Box.test(resid.log.trend, lag=1, type="Ljung-Box")   #we reject the H0 of independence of lag1 of the residuals
adf.test(resid.log.trend, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(resid.log.trend)  #small p-values suggest that the series is not stationary and a differencing is required.
plot(resid.log.trend,type="l", xlab="year", ylab="Residuals")
abline(h = 0, lwd = 1)
acf(resid.log.trend, lag.max = 40)
pacf(resid.log.trend, lag.max = 40)

#trend elimination by differentiation at lag = 1
d1.indiceloyers <- ts(diff(indiceloyers, lag=1), end=c(2018,1), freq=4) 
d1.indiceloyers  #for comparison
sd(d1.indiceloyers)

##############################################################
d2.indiceloyers <- ts(diff(indiceloyers, lag=2), end=c(2018,1), freq=4) 
d2.indiceloyers  
sd(d2.indiceloyers)
d2.indiceloyers.train <- ts(diff(indiceloyers.train, lag=2), end=c(2009,2), freq=4) 
d2.indiceloyers.train
d2.indiceloyers.test <- ts(diff(indiceloyers.test, lag=2), end=c(2018,1), freq=4) 
d2.indiceloyers.test  

d3.indiceloyers <- ts(diff(indiceloyers, lag=3), end=c(2018,1), freq=4) 
d3.indiceloyers  #for comparison
sd(d3.indiceloyers)

d4.indiceloyers <- ts(diff(indiceloyers, lag=4), end=c(2018,1), freq=4) 
d4.indiceloyers  #for comparison
sd(d4.indiceloyers)

d5.indiceloyers <- ts(diff(indiceloyers, lag=5), end=c(2018,1), freq=4) 
d5.indiceloyers  #for comparison
sd(d5.indiceloyers)

d10.indiceloyers <- ts(diff(indiceloyers, lag=10), end=c(2018,1), freq=4) 
d10.indiceloyers  #for comparison

plot(d1.indiceloyers, xlab="year", ylab="once-differenced rent index" ) 
abline(h = 0, lwd = 1)
Box.test(d1.indiceloyers, lag=1, type="Ljung-Box")   #we cannot reject the H0 of independence of lag1 of the residuals
adf.test(d1.indiceloyers, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(d1.indiceloyers)  #small p-values suggest that the series is not stationary and a differencing is required.
#therefore there exist most probably no more auto-correlation between the residuals in the differentiation-1-model
acf(d1.indiceloyers, lag.max = 40)   #acf shows the same pattern, no more auto-correlation in the differentiation-1-model
pacf(d1.indiceloyers, lag.max = 40)
#the model which reduced the trend by lag1-differentiation seems to have too independent residuals (too iid-noisy)
#and therefore no correlated values which impedes a good prediction model

plot(d2.indiceloyers, xlab="year", ylab="twice-differenced rent index" ) 
abline(h = 0, lwd = 1) 
mean(d2.indiceloyers)
d2.indiceloyers.centered <- d2.indiceloyers-mean(d2.indiceloyers)
plot(d2.indiceloyers.centered, xlab="year", ylab="twice-differenced rent index, centered" ) 
abline(h = 0, lwd = 1) 
Box.test(d2.indiceloyers, lag=2, type="Ljung-Box")  #we reject the H0 of independence of the lag2-residuals
adf.test(d2.indiceloyers, alternative = "stationary", k=2)  #the small p-value suggest the data is stationary (rejecting the H0 of non-stationarity).
                       #being aware that the Dickie-Fuller-Test assumes AR-Processes and provides therefore only low power to test for stationarity
kpss.test(d2.indiceloyers)  #the p-value >0.05 suggests that we cannot reject the H0 of stationarity and we can assume that the data is stationary
acf(d2.indiceloyers.centered, lag.max = 40)
pacf(d2.indiceloyers.centered, lag.max = 40)

#doing some cross-validation, separating in 2 segments and have a look if the serie's characteristics remain the same
plot(d2.indiceloyers.test, main="Rent index lag=2", ylab="diff rent index" )  
Box.test(d2.indiceloyers.test, lag=2, type="Ljung-Box") #p-value of 0.024 suggests the data are (time-)independent
adf.test(d2.indiceloyers.test, alternative = "stationary", k=2)  #H0 of non-stationarity cannot be rejected which is not a problem, that could be due to lack of observations
kpss.test(d2.indiceloyers.test)  #with a p-value>0.05 the test data seems to be stationary aswell
acf(d2.indiceloyers.test, lag.max = 40)
pacf(d2.indiceloyers.test, lag.max = 40)
plot(d2.indiceloyers.train, main="Rent index lag=2", ylab="diff rent index" )  
Box.test(d2.indiceloyers.train, lag=2, type="Ljung-Box") #the data are independent, but we can see a slight increase indicating that in the first 2 tiers (during the train set) was higher than in the years in the test data set which begins after 2009. that can be explained by the big baisse in the early 90ies, therefore the increase was stronger and after 2009 the growth in rental prices slowed down, which can be very well explained aswel lby the subprime crises.
adf.test(d2.indiceloyers.train, alternative = "stationary", k=2)  
kpss.test(d2.indiceloyers.train)  
acf(d2.indiceloyers,train, lag.max = 40)
pacf(d2.indiceloyers.train, lag.max = 40)



#conclusion: we should work on the model which reduced the trend by twice-differencing since there are quite 
#some correlation between the residuals and the hypothesis of independence of the lag2-residuals has been rejected
plot(d3.indiceloyers, xlab="year", ylab="triple-differenced rent index" ) 
abline(h = 0, lwd = 1)  
adf.test(d3.indiceloyers, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(d3.indiceloyers)  #small p-values suggest that the series is not stationary and a differencing is required.
acf(d3.indiceloyers, lag.max = 40)
pacf(d3.indiceloyers, lag.max = 40)
plot(d4.indiceloyers,xlab="year", ylab="quadruple-differenced rent index" ) 
abline(h = 0, lwd = 1) 
Box.test(d4.indiceloyers, lag=4, type="Ljung-Box") 
adf.test(d4.indiceloyers, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(d4.indiceloyers)  #small p-values suggest that the series is not stationary and a differencing is required.
acf(d4.indiceloyers, lag.max = 40)
pacf(d4.indiceloyers, lag.max = 40)
plot(d5.indiceloyers, main="Rent index lag=5", ylab="diff rent index" )  #for comparison
Box.test(d5.indiceloyers, lag=5, type="Ljung-Box")
adf.test(d5.indiceloyers, alternative = "stationary")  #small p-values suggest the data is stationary and doesn't need to be differenced stationarity.
kpss.test(d5.indiceloyers)  #small p-values suggest that the series is not stationary and a differencing is required.
acf(d5.indiceloyers, lag.max = 40)
pacf(d5.indiceloyers, lag.max = 40)
################################################################# 

#diagnostics
tsdiag(d2.indiceloyers.centered, gof.lag=40)


#Yule-Walker-Test for preliminary prediction of an AR(p)-process (rough estimation)
source("Innovation_ARMA.R")
source("ARMA_estimation.R")

YuleWalker(indiceloyers,p=1)  #Yule-Walker-Test confirms the visualization (ACF/PACF), indicating an AR(1)-Process for our data
YuleWalker(indiceloyers,p=2)  #since the first coefficient is the only one strongly differentiating from zero, then
YuleWalker(indiceloyers,p=3)  #the other coefficients are too weak/too close to zero
YuleWalker(indiceloyers,p=4)

#AIC hier: searching through the program to find the number of p with the smallest AICC-value

abs(polyroot(z=c(1, 0.9768351)))  #1.023714 slightly outside unit circle
#itsmr: hannan
HannanRissanen(indiceloyers,p=1,q=0) #H-RRissanen provides evidence for the first coefficient, and slightly less for the second aswell
HannanRissanen(indiceloyers,p=2,q=0)
HannanRissanen(indiceloyers,p=3,q=0)
HannanRissanen(indiceloyers,p=4,q=0)

HannanRissanen(indiceloyers,p=1,q=1)
HannanRissanen(indiceloyers,p=2,q=1)
HannanRissanen(indiceloyers,p=1,q=2)
HannanRissanen(indiceloyers,p=2,q=2)
HannanRissanen(indiceloyers,p=1,q=3)
HannanRissanen(indiceloyers,p=2,q=3)
HannanRissanen(indiceloyers,p=1,q=4)
HannanRissanen(indiceloyers,p=2,q=4)
#HannanRissanen-Algoritm shows strongly varying coefficients theta (q) for different fixed numbers of coefficients
#which further underlines our assumption that an AR(p)-model suits our data well instead of an MA(q) or ARMA(p,q) process.

coefficients(YuleWalker(indiceloyers,p=1))

hannan(d2.indiceloyers.centered, p=1, q=1)
hannan(d2.indiceloyers.centered, p=2, q=1)
hannan(d2.indiceloyers.centered, p=3, q=1)
hannan(d2.indiceloyers.centered, p=4, q=1)
hannan(d2.indiceloyers.centered, p=1, q=2)
hannan(d2.indiceloyers.centered, p=1, q=3)
hannan(d2.indiceloyers.centered, p=1, q=4)

d2.indiceloyers.centered.vectorized <- as.vector(d2.indiceloyers.centered)
ar(d2.indiceloyers.centered, aic = TRUE, order.max = 15, method = "yule-walker")

#we can see bringing a q into the game doesn't bring much, this is aswell confirmed by the AICC which is 41 and heigher than 34


##use itsmr: autofit and then arima() to get AICC
autofit(d2.indiceloyers.centered, p= 0:5, q = 0:5)
autofit(d1.indiceloyers, p= 0:3, q = 0:3)
autofit(d2.indiceloyers.centered, p= 0:3, q = 0:3)
autofit(indiceloyers, p= 0:3, q = 0:3)

auto.arima(d2.indiceloyers.centered, max.p = 5, max.q = 5)
arima(d2.indiceloyers.centered, order=c(3,0,1), optim.method = "BFGS" )

#rough estimates
ar(d2.indiceloyers.centered, aic = TRUE, order.max = NULL,method = c("yule-walker"))
ar(d2.indiceloyers.centered, aic = TRUE, order.max = NULL,method = "burg")

#real estimates

arima(d2.indiceloyers.centered, order=c(3,0,0), optim.method = "BFGS" )
arima(d2.indiceloyers.centered, order=c(4,0,0), optim.method = "BFGS" ) #we should go with the ARIMA(4,0,0) since there is not much further gain in using an ARIMA(5,0,0), the first coefficients remain more or less the same and the 5th one is close to zero and therefore presumably not significant. on the other hand the coefficients in comparison to the ARIMA(3,0,0) model are quite different, indicating that there is a gain by adding an additional AR-term
arima(d2.indiceloyers.centered, order=c(5,0,0), optim.method = "BFGS" )

#
fit <- arima(d2.indiceloyers.centered, order=c(4,0,0), optim.method = "BFGS" )
summary(fit)
acf(residuals(fit))
Box.test(residuals(fit), lag=2, type="Ljung")
#The ACF plot of the residuals from the ARIMA(3,1,1) model shows all correlations within the threshold limits 
#indicating that the residuals are behaving like white noise. A ljung-box test returns a large p-value, also suggesting 
#the residuals are white noise. 