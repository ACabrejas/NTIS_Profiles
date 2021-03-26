library(sarima)
library(forecast)


rm(list = ls()) # Clear variables
graphics.off()  # Clear plots
cat("\014")     # Clear console

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./profiles_functions/get_link_data.R")

## Choose motorway
mX <- "m6"
file_name = paste(mX,"_data_spikes_background_threshold_only.rda", sep = "")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- links_list_df$link_id
link = links_list[1]
method = "threshold"
if (mX == "m6") {
  spike_parameter = "1.1"
} else if (mX == "m11") {
  spike_parameter = "1.2"
} else if (mX == "m25") {
  spike_parameter = "1.4"
}

link_data = get_link_data(link = link, method = method, spike_parameter = spike_parameter)

tt = link_data$travel_time
#############################################################################


ggAcf(tt)

a = sarima((tt) ~
             0 | ma(1, c(-0.3)) + sma(12,1, c(-0.1)) + i(2) + s(12), ss.method = "base")


seasonal = list(order = c(0,0,0), period = 1440, order =c(0,0,0), period = 10080)
b = arima(tt, order = c(0,0,0), seasonal)
tsdiag(b)
plot(b)


#############################################

c = auto.arima(tt, seasonal=FALSE, lambda=0,
               xreg=fourier(tt, K=c(10,10)))

##################################################

tt.ts = ts(tt, frequency = 10080)

fit <- auto.arima(tt, seasonal=FALSE, lambda=0,
                  xreg=fourier(tt.ts, K=10080/2))
fit %>%  forecast(xreg=fourier(t.tst, K=c(10080), h=10080)) %>%  autoplot(include=4*10080) +
  ylab("TT") + xlab("Weeks")

