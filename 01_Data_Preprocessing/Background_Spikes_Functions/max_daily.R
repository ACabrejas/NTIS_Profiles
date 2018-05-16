max_daily = function (algorithm, threshold) {
  ## Prameter "threshold" acts as the % of the average of the daily maximums from which we
  ## consider any point as belonging into a spike.
  ## Example: With threshold = 0.8. it will be considered as a spike any point having a
  ## travel time over 80% of the average of daily maximums (densities or travel times).
  
  
  # Create containers for internal calculations
  spikes <- rep(0,length(travel_time))
  spike_flag <- rep(0,length(travel_time))
  background <- rep(median(travel_time, na.rm = TRUE),length(travel_time))
  
  ## USING DAILY TRAVEL TIMES
  if (algorithm == "max_day_traveltime") {
    
    ## Calculate MAX DAILY DENSITY
    travel_time_daily_max = numeric()
    for (week in 0:(length_weeks$total_weeks-1)) {
      for (day in 0:6) {
        index_start = 1+week*7*1440+day*1440
        index_end = index_start + 1439
        day_index = week*7+day
        travel_time_daily_max[day_index] = max(travel_time[index_start:index_end])
      }
    }
    
    ## Take the mean
    average_max_traveltime = mean(travel_time_daily_max, na.rm = TRUE)
    
    ## Scale the density by the average daily max density in the link
    travel_time_scaled = travel_time/average_max_traveltime
    
    ## Fill background container with the items below the threshold
    background[travel_time_scaled < threshold] <- travel_time[travel_time_scaled < threshold]
    
    # Fill the gaps in the time series (in the positions of the spikes, the remainder equals the value at the point minus the spike)
    background[travel_time_scaled > threshold] <- average_max_traveltime*threshold
    
    ## Fill Spike and SpikeFlag containers with the items above the threshold
    spike_flag[travel_time_scaled > threshold] <- 1
    
    # Fill the gaps in the spikes series (in spike locations, actual height of spike is value of the point minus threshold)
    spikes[travel_time_scaled > threshold] <- travel_time[travel_time_scaled > threshold] - average_max_traveltime*threshold
    
    
    ## USING DAILY DENSITIES
  } else if (algorithm == "max_day_density") {
    ## Retrieve speed data, necessary for inferring density
    speed <- m_data_selected$traffic_speed[m_data_selected$link_id == link]
    speed[speed == 0] <- median(speed, na.rm = TRUE)

    density <- flow/speed  # Here units are cars/Km   [ car/hour / Km/hour]
    
    ## Calculate MAX DAILY DENSITY
    density_daily_max = numeric()
    travel_time_daily_max = numeric()
    for (week in 0:(length_weeks$total_weeks-1)) {
      for (day in 0:6) {
        index_start = 1+week*7*1440+day*1440
        index_end = index_start + 1439
        day_index = week*7+day
        density_daily_max[day_index] = max(density[index_start:index_end])
        travel_time_daily_max[day_index] = max(travel_time[index_start:index_end])
      }
    }
    
    ## Take the mean
    average_max_density = mean(density_daily_max, na.rm = TRUE)
    average_max_traveltime = mean(travel_time_daily_max, na.rm = TRUE)
    
    ## Scale the density by the average daily max density in the link
    density_scaled = density/average_max_density

    ## Fill background container with the items below the threshold
    background[density_scaled < threshold] <- travel_time[density_scaled < threshold]

    # Fill the gaps in the time series (in the positions of the spikes, the remainder equals the value at the point minus the spike)
    background[density_scaled > threshold] <- average_max_traveltime*threshold
    
    
    ## Fill Spike and SpikeFlag containers with the items above the threshold
    spike_flag[density_scaled > threshold] <- 1
    
    # Fill the gaps in the spikes series (in spike locations, actual height of spike is value of the point minus threshold)
    spikes[density_scaled > threshold] <- travel_time[density_scaled > threshold] - average_max_traveltime*threshold

  } else {
    stop("Error while attempting to magnitude to calculate daily maxima. \n
         Please select either \"density\" or \"travel_time\".")
  }
  
  output = list(background = background, spikes = spikes, spike_flag = spike_flag)
  
  return(output)
}