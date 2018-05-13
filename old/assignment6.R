

# number of simulated terms
n <- 100

#ex1
model <- list(order = c(0,0,0))
plot(arima.sim(model, n))

#ex2
model <- list(order = c(1,0,0), ar=-0.9)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#there is a negative covariance between yt and yt-1 and a positive cov. between yt and yt-2
#the acf shows a negative auto-corr. and the pacf confirms the AR(1) (1 significant bar outside the 1.96-bounds)
model <- list(order = c(1,0,0), ar=-0.3)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#neg. corr as well , less strong than before, pacf indicates aswell an AR(1) process

model <- list(order = c(1,0,0), ar=0.5)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
model <- list(order = c(1,0,0), ar=0.9)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
model <- list(order = c(1,0,0), ar=1)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#in both cases positive autocovariances and the pacf plots with 1 significant bar suggesting an AR(1) process, ar=1 is not stationary

#ex3
model <- list(order = c(0,0,1), ma=1.9)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#there is a positive covariance between yt and yt-1 (confirmed as well by acf plot with sign. bar at lag1), and at lag2 it is no corr. see the dispersed plot yt vs yt2
model <- list(order = c(0,0,1), ma=0.5)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#two positive autocorrelations (AR2) visible at both plots yt<->yt-1 and yt<->yt-2
#shouldn't the pacf exponentially decrease if it's an MA process??
model <- list(order = c(0,0,1), ma=-0.5)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
##???  shouldn't the pacf exponentially decrease if it's an MA process??
model <- list(order = c(0,0,1), ma=-1)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#ma=-1 means not invertible?

#ex4 
model <- list(order = c(1,0,1), ar=0.7, ma=-0.3)
plot(simul <- arima.sim(model, n))
plot(simul[2:length(simul)],simul[1:(length(simul)-1)])
plot(simul[3:length(simul)],simul[1:(length(simul)-2)])
acf(simul)
pacf(simul)
#is the pacf really decreasing? the negative moving average is clearly visible!

#ex5
# number of simulated terms
n <- 100
# 1.
# model of ARIMA(p,d,q)
model <- list(order = c(2,0,0),ar = c(-0.2, 0.48))
plot(simul <- arima.sim(model, n))
acf(simul)
pacf(simul)
# 3.
model <- list(order = c(1,0,1),ar = c(-0.6), ma = c(-1.2))
plot(simul <- arima.sim(model, n))
acf(simul)
pacf(simul)
#that is clear ARMA with a clearly decreasing AR and a clearliy decreasing moving average process with a clear exponentially decreasing MA!
#but why is it ARMA(1,1) and not ARMA(2,2) since they are at least 2 significant PACF bars and 2 significant ACF bars
# 4.
model <- list(order = c(2,0,0), ar = c(-1.8, 0.81))
plot(simul <- arima.sim(model, n))
acf(simul)
pacf(simul)
#ar part not stationary. why? because of the -1.8?