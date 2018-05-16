############################################################################
#
#    GOAL: Generation of histograms for the average maximum day density.
#    IN  : Data interp of the 
#
############################################################################



# Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

# Load libraries
library(stats)
library(graphics)

# --- Load and assign data (edit for M6 or M11!!!) ----------------------------------------

motorway = 'm6'

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(paste('../00_Data/',motorway,'_data_interp.RData', sep = ''))
links <- unique(m_data_interp$link_id)

#Analysis parameters
link_selected <- links[14]
period_weeks <- 12
starting_week <- 0

# Data extraction (missing data substituted with median)
start_period <- (1+(6*1440)+(1440*7*starting_week))
end_period <- (1+(6*1440)+(1440*7*starting_week)+(1440*7*period_weeks))

travel_time <- m_data_interp$travel_time[m_data_interp$link_id == link_selected]
travel_time[travel_time == -1] <- median(travel_time[travel_time != -1])
travel_time <- travel_time[start_period:end_period]

flow <- m_data_interp$traffic_flow[m_data_interp$link_id == link_selected]
flow[flow == -1] <- median(flow[flow != -1])
flow <- flow[start_period:end_period]

speed <- m_data_interp$traffic_speed[m_data_interp$link_id == link_selected]
speed[speed == -1] <- median(speed[speed != -1])
speed <- speed[start_period:end_period]

free_flow_time <- m_data_interp$free_flow[m_data_interp$link_id == link_selected]
free_flow_time[free_flow_time == -1] <- median(free_flow_time[free_flow_time != -1])
free_flow_time <- free_flow_time[start_period:end_period]

headway <- m_data_interp$free_flow[m_data_interp$link_id == link_selected]
headway[headway == -1] <- median(headway[headway != -1])
headway <- headway[start_period:end_period]

density <- flow/speed

#DENSITIES Histogram MAX DAILY
data_hist = numeric()
for (week in 0:(period_weeks-1)) {
  for (day in 0:6) {
    index_start = 1+week*7*1440+day*1440
    index_end = index_start + 1440
    day_index = week*7+day
    data_hist[day_index] = max(density[index_start:index_end])
  }
}
#Max daily density records from the Link, non scaled
hist(data_hist, breaks = 50, freq = TRUE, xlab= 'Maximum Daily Density', 
     main = 'Max Daily Density M6-Link9 (01.03-30.05)')
#All density records from the Link, non scaled
hist(density, breaks = 50, freq = TRUE, xlab= 'Maximum Daily Density', 
     main = 'Density M6-Link9 (01.03-30.05)')
#Max daily density records from the Link, scaled by average max daily
average_max_density = mean(data_hist, na.rm = TRUE)
scaled_day_max = data_hist/average_max_density
hist(scaled_day_max, breaks = 50, freq = TRUE, xlab= 'Maximum Daily Density', 
     main = paste('Max Scaled Daily Density M6-Link9 (01.03-30.05).', average_max_density))
#All density records from the Link, scaled by average max daily
scaled_global <- density / average_max_density
hist(scaled_global, breaks = 50, freq = TRUE, xlab= 'Maximum Daily Density', 
     main = paste('Scaled Density M6-Link9 (01.03-30.05).', average_max_density))
#All density records from the Link, scaled by average max daily. LOG SCALE
hist.data = hist(scaled_global, plot=F)
hist.data$counts = log(hist.data$counts, 2)     
plot(hist.data)

