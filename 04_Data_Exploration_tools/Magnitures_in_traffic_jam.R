
## Clear-all
rm(list = ls()) # Clear variables
graphics.off()  # Clear plots
cat("\014")     # Clear console

## Choose motorway
mX <- "m25"

## Load motorway data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#file_name <- paste(mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_0.8.RData',sep="")
file_name = paste(mX,"_data_spikes_background_threshold_only_high.rda", sep = "")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- links_list_df$link_id
link=links_list[1]

speed = data$m_data_selected$traffic_speed[data$m_data_selected$link_id==link]
dens = data$m_data_selected$traffic_flow[data$m_data_selected$link_id==link] / speed
conc = data$m_data_selected$traffic_concentration[data$m_data_selected$link_id==link]
head = data$m_data_selected$traffic_headway[data$m_data_selected$link_id==link]

plot(link_data$travel_time[26300:26600], type = 'l', lwd=2, ylim = c(0,200), 
     xlab = "Time [minutes]", ylab= "Travel Time [s] / Speed [Km/h] / Density [cars/Km]")
lines(speed[26300:26600], col="blue", lwd=2)
lines(dens[26300:26600], col = "red", lwd = 2)
lines(conc[26300:26600], col="green", lwd = 2)
lines(head[26300:26600], col="orange", lwd = 2)
legend(1,205, legend = c("Travel Time [s]", "Speed [Km/h]", "Density [cars/Km]", "Traffic Concentration [%]",
                         "Car Headway [1/10ths of s]"), col=c("Black", "Blue", "Red", "Green", "Orange"), lty=1, cex = 0.65)
