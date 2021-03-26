######################################
#
#   MSARIMA
#
######################################
library(smooth)

tt = link_data$travel_time

training_length = 4
pred_length = 1

tt_sel = tt[1:(training_length*10080)]

plot(tt, type = 'l')
plot(tt_sel, type = 'l')

model.auto = auto.msarima(y = tt,
                orders = list(ar = c(2,1,1), i=c(1,0,0), ma=c(3,1,1)),
                lags = c(1,1440,10080),
                fast = T,
#                loss = c("MSE"),
                h = 10080,
                interval = "likelihood",
                silent = F)


model = msarima(tt_sel, 
                orders =list(ar = c(1,1,1), i=c(1,1,0), ma=c(1,1,1)),
                lags = c(1,1440,10080),
                h=10080,
                holdout = T,
                interval = "np")


model
tt_est = as.numeric(model$forecast)
tt_true = tt[10081:(10080+10080*pred_length)]

plot(tt_est, type = 'l')
ines(tt_true,col="red")

mape = abs(tt_true-tt_est)/tt_true
plot(mape,type = 'l')



model2 = msarima(tt_sel, 
                orders =list(ar = c(1,1,1), i=c(1,0,0), ma=c(1,1,1)),
                lags = c(1,1440,10080),
                h=10080,
                holdout = T,
                interval = "np")
model2
model_p=model2
tt_est = as.numeric(model_p$forecast)

plot(tt_est, type = 'l')
lines(tt_true,col="red")

mape = abs(tt_true-tt_est)/tt_true
plot(mape[1:1440],type = 'l')



model3 = msarima(tt_sel, 
                 orders =list(ar = c(1,2,1), i=c(2,0,0), ma=c(1,1,1)),
                 lags = c(1,1440,10080),
                 h=10080,
                 holdout = T,
                 interval = "np")
model3
model_p=model3
tt_est = as.numeric(model_p$forecast)

plot(tt_est, type = 'l')
lines(tt_true,col="red")

mape = abs(tt_true-tt_est)/tt_true
plot(mape,type = 'l')

model4= msarima(tt_sel, 
                 orders =list(ar = c(2,2,1), i=c(2,0,0), ma=c(1,1,1)),
                 lags = c(1,1440,10080),
                 h=10080,
                 holdout = T,
                 interval = "np")
model4
model_p=model4
tt_est = as.numeric(model_p$forecast)

plot(tt_est, type = 'l')
ines(tt_true,col="red")

mape = abs(tt_true-tt_est)/tt_true
plot(mape,type = 'l')