get_link_data = function(link, method, spike_parameter) {
  ##################################################################################################
  ##                                                                                              ##
  ## ------------------------------  DATA RETRIEVAL FUNCTION  ----------------------------------  ##
  ##                                                                                              ##
  ## Input: Link ID, fetch information                                                            ##
  ## Operation: Indicate the type of data that needs to be fetched and the corresponding list.    ##
  ## Output: Named list with the requested information, labelled.                                 ##
  ##                                                                                              ##
  ##################################################################################################
  
  # Fetch all data
  
  travel_time <- data$m_data_selected$travel_time[data$m_data_selected$link_id == link]
  flow <- data$m_data_selected$traffic_flow[data$m_data_selected$link_id == link]
  flow[is.na(flow)] = median(flow, na.rm = T)
  
  background <-data$background_spikes[[method]][[as.character(spike_parameter)]]$background[which(links_list==link),]
  spikes <- data$background_spikes[[method]][[as.character(spike_parameter)]]$spikes[which(links_list==link),]
  spike_flag <- data$background_spikes[[method]][[as.character(spike_parameter)]]$spike_flag[which(links_list==link),]
  
  #background <-data$background_spikes[[method]][[spike_parameter]]$background[data$m_data_selected$link_id == link]
  #spikes <- data$background_spikes[[method]][[spike_parameter]]$spikes[data$m_data_selected$link_id == link]
  #spike_flag <- data$background_spikes[[method]][[spike_parameter]]$spike_flag[data$m_data_selected$link_id == link]
  
  #background_tt2 <- m_data_selected$background_tt2[m_data_selected$link_id == link]
  #spikes_tt2 <- m_data_selected$spikes_tt2[m_data_selected$link_id == link]
  #spike_flag_tt2 <- m_data_selected$spike_flag_tt2[m_data_selected$link_id == link]
  #background_dens <- m_data_selected$background_dens[m_data_selected$link_id == link]
  #spikes_dens <- m_data_selected$spikes_dens[m_data_selected$link_id == link]
  #spike_flag_dens <- m_data_selected$spike_flag_dens[m_data_selected$link_id == link]
  
  output = list(travel_time = travel_time,background = background, flow = flow, spikes = spikes, spike_flag = spike_flag)
  
  return(output)
}