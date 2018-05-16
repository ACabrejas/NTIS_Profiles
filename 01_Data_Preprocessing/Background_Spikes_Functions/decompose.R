decompose = function(algorithm, parameters, storage) {
  
  # Loop through different parametric configurations for the same link data
  for (i in 1:length(parameters)) {
    # Choose method and call selected function, the functions interact directly with the storage
    if (algorithm == "threshold") {
     output = scaled_threshold(th_high = parameters[i])
     
     storage[[i]]$background[which(links_list == link),] = output$background
     storage[[i]]$spikes[which(links_list == link),] = output$spikes
     storage[[i]]$spike_flag[which(links_list == link),] = output$spike_flag
     
    } else if (algorithm == "max_day_traveltime") {
      output = max_daily(algorithm = algorithm, threshold = parameters[i])
      
      storage[[i]]$background[which(links_list == link),] = output$background
      storage[[i]]$spikes[which(links_list == link),] = output$spikes
      storage[[i]]$spike_flag[which(links_list == link),] = output$spike_flag
      
    } else if (algorithm == "max_day_density") {
      output = max_daily(algorithm = algorithm, threshold = parameters[i])
      
      storage[[i]]$background[which(links_list == link),] = output$background
      storage[[i]]$spikes[which(links_list == link),] = output$spikes
      storage[[i]]$spike_flag[which(links_list == link),] = output$spike_flag
      
    } else if (algorithm == "wavelet") {
      
      
      
      storage[[i]]$background[which(links_list == link),] = output$background
      storage[[i]]$spikes[which(links_list == link),] = output$spikes
      storage[[i]]$spike_flag[which(links_list == link),] = output$spike_flag
      
    } else {
      stop("Incorrect or missing input calculation algorithm. \n Please use a string chosen from: \n - threshold \n - max_day_traveltime \n - max_day_density \n - wavelet")
    }
  }
  
  
  return(storage)
}