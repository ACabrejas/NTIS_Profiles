scaled_threshold = function(th_high) {
  ## METHOD 1: Scale travel times as a fraction of free flow time, based on link length and max speed
  
  # Define scaling speed
  if (mX == "m25") {
    v = 31.2928 ## 70mph only for M25
  } else if (mX == "m6" || mX == "m11") {
    v <- 28.5 ## 63.76 mph = 102.6 km/h = 28.5 m/s
  } else {
    stop("Error while attempting to select speed for scaling travel times. \n
         Please make sure the motorway selection is correct.")
  }
  
  # Method low parameters (ignore times below this ratio of free flow travel time and substitute by median)
  th_low = 0.8

  # Scale travel times based on calculated free flow travel time
  travel_time_scaled <- travel_time / (links_list_df$link_length[which(links_list==link)]/v)
  
  background <- rep(median(travel_time),length(travel_time))
  background_scaled <- rep(1,length(travel_time))
  background[travel_time_scaled >= th_low & travel_time_scaled <= th_high] <- travel_time[travel_time_scaled >= th_low & travel_time_scaled <= th_high]
  background_scaled[travel_time_scaled >= th_low & travel_time_scaled <= th_high] <- travel_time_scaled[travel_time_scaled >= th_low & travel_time_scaled <= th_high]
  
  spikes <- rep(0,length(travel_time))
  spikes_scaled <- rep(0,length(travel_time_scaled))
  spike_flag <- rep(0,length(travel_time))
  spikes[travel_time_scaled > th_high] <- travel_time[travel_time_scaled > th_high] - (links_list_df$link_length[which(links_list==link)]/v)
  spikes_scaled[travel_time_scaled > th_high] <- travel_time_scaled[travel_time_scaled > th_high] - 1 ## "1" is the scaled expected travel time
  spike_flag[travel_time_scaled > th_high] <- 1
  
  output = list(background = background, spikes = spikes, spike_flag = spike_flag)
  
  return(output)
  
}