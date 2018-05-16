background_spikes_storage = function(parameter_list) {
  
  # Create an empty list, it will contain all results for the chosen method
  method_storage = list()
  # Convert the different parameters of the method to character, to use as name of each container
  parameter_names = as.character(parameter_list)
  
  # Loop through the different parameters for the given method
  for (i in 1:length(parameter_list)) {
    # In each of them, create a matrix of (#links * #travel time points) for the 3 magnitudes
    background <- matrix(-1,nrow = length(links_list), ncol =  length(travel_time))
    spikes <- matrix(-1,nrow = length(links_list), ncol =  length(travel_time))
    spike_flag <- matrix(-1,nrow = length(links_list), ncol =  length(travel_time))
    
    # Group and label them in a list
    parameter_storage = list(background = background, spikes = spikes, spike_flag = spike_flag)
    
    # Save the storage for the parameter using the parameter's value as name 
    method_storage[[parameter_names[i]]] = parameter_storage
  }
  
  # Return the finished container
  return(method_storage)
}