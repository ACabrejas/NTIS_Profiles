

precision = rep(-99,length(unique(data$m_data_selected$link_id)))

for (i in (1:(length(precision)))) {
  a = hybrid_results$threshold[[1]]$relative_errors$relative_error[i,]
  prec = sum(abs(a))/length(a)
  precision[i] = prec
}
print(precision)

MAPE = round(precision*100,2)
MAPE
mean(MAPE)



for (i in (1:(length(precision)))) {
  a = hybrid_results$threshold[[1]]$quantile_errors$rms_error[i,]
  prec = sum(abs(a))/length(a)
  precision[i] = prec
}
print(precision)

RMSE = round(precision,2)
RMSE
mean(RMSE)




TravelTime = data$m_data_selected$travel_time[data$m_data_selected$link_id == links_list[3]]
par(mar=c(5,4.2,2,2)+0.1)
bacf =  acf(TravelTime, lag.max = (100 + 10080 * 4), plot = T, xlab = "Lag [Days]", xaxt="n",yaxt="n",  main="", cex.lab = 1.8)
axis(1, at=seq(0,40320, by = 2880), labels = seq(0,28, by=2), cex.axis = 1.8)
axis(2, at=seq(0,1, by = 0.2), seq(0,1, by = 0.2), cex.axis = 1.8)


bacfdf <- with(bacf, data.frame(lag, acf))
q <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")
q


## DISTRIBUTION OF ERRORS
bob = thales_results$threshold$`1.1`$relative_errors$relative_error
breaks = c(-10,-.25,-.15,-.05,.05,.15,.25,10)
alice = hist(bob, breaks = breaks, freq = F)
freq = alice$counts/length(bob)
freq = 100*freq
freq
