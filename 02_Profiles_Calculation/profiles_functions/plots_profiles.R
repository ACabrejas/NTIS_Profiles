relative_error_daytime_singlelink = function(error_type, link_number, method, spike_parameter) {
  # Create date and time index
  date_create = seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min")
  # Remove the last entry
  date = date_create[1:(length(date_create)-1)]
  # Remove the date and leave time of the day
  time = substr(date,12,19)
  #Define the labels
  seqq = seq(1,1440, 60)
  time_label = time[seqq]
  
  # Create dataframe to plot depending on the needed data
  if (error_type == "Relative") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           Null = null_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           STL = stl_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           Fourier = fourier_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           Segmentation = segmentation_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_matrix[link_number,],
                           time)
    } else if (error_type == "Negative") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,],
                             
                             time)
    } else if (error_type == "Density") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$day_time_error_matrices$day_time_density_error_matrix[link_number,],
                             time)
    } else {
      stop("Incorrect or missing input for error type. \n Please use a string chosen from: \n -Relative \n -Negative \n -Density")
      
    }
  # Melt it
  plot_data_long = melt(plot_data, id="time")
  
  # Plot
  ggplot(data = plot_data_long, aes(x=time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + xlab("Time of the day") +
    geom_line(size=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_x_discrete(breaks=time_label, labels=as.character(time_label)) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste(error_type, "error for link", links_list[link_number], "across times of the day", sep = " "))
}

#############################################################################################################

relative_error_daytime = function(error_type, method, spike_parameter) {
  # Create date and time index
  date_create = seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min")
  # Remove the last entry
  date = date_create[1:(length(date_create)-1)]
  # Remove the date and leave time of the day
  time = substr(date,12,16)
  #Define the labels
  seqq = seq(1,1440, 120)
  time_label = time[seqq]
  
  # Create dataframe to plot depending on the needed data
  if (error_type == "Relative") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           Null = null_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           STL = stl_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           Fourier = fourier_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           Segmentation =  segmentation_results[[method]][[spike_parameter]]$daytime_errors$day_time_error,
                           time)
  } else if (error_type == "Negative") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           Null = null_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           STL = stl_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           Fourier = fourier_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           Segmentation =  segmentation_results[[method]][[spike_parameter]]$daytime_errors$day_time_error_neg,
                           time)
  } else if (error_type == "Density") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           Null = null_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           STL = stl_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           Fourier = fourier_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           Segmentation =  segmentation_results[[method]][[spike_parameter]]$daytime_errors$day_time_density_error,
                           time)
  } else {
    stop("Incorrect or missing error type. \n Please choose from: \n -Relative \n -Negative \n -Density")
  }
  # Melt it
  plot_data_long = melt(plot_data, id="time")
  
  # Plot
  ggplot(data = plot_data_long, aes(x=time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + 
    xlab("Time of the day") +
    #theme(axis.title.x = element_blank()) + 
    geom_line(size=1.5) + theme(text = element_text(size=28),axis.text.x = element_text(angle = 0)) + 
    scale_x_discrete(breaks=time_label, labels=as.character(time_label)) +
    scale_y_continuous(breaks=seq(0.025,0.15,by=0.025)) + 
    scale_colour_discrete(name  =label_mx)+
                          #breaks=c("Published", "Null", "STL", "Fourier", "Hybrid"),
    #                      labels=c("Published Model", "Null Model", "STL Model", "Fourier Model", "Hybrid Model"))  +
    theme(legend.position = c(0.14, 0.9)) + theme(legend.text=element_text(size=24)) 
    #ggtitle(paste("Average", error_type, "error for all links across times of the day"))
}

###########################################################################################################

relative_error_timeseries = function(error_type, method, spike_parameter) {
  # Create x axis (only for proper indexing in the dataframe)
  Time = 1:(1440*7*length_weeks$pred_weeks)
  # Select type of error and create corresponding dataframe with aggregated results
  if (error_type == "Relative") {
    plot_data = data.frame(Published = colMeans(thales_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           Null = colMeans(null_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           Fourier = colMeans(fourier_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           STL = colMeans(stl_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           Hybrid = colMeans(hybrid_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           Segmentation = colMeans(segmentation_results[[method]][[spike_parameter]]$relative_errors$relative_error),
                           Time = Time)
  } else if (error_type == "Negative") {
    plot_data = data.frame(Published = colMeans(thales_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           Null = colMeans(null_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           Fourier = colMeans(fourier_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           STL = colMeans(stl_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           Hybrid = colMeans(hybrid_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           Segmentation = colMeans(segmentation_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg),
                           Time = Time)
  } else if (error_type == "Density") {
    plot_data = data.frame(Published = colMeans(thales_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           Null = colMeans(null_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           Fourier = colMeans(fourier_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           STL = colMeans(stl_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           Hybrid = colMeans(hybrid_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           Segmentation = colMeans(segmentation_results[[method]][[spike_parameter]]$relative_errors$density_error),
                           Time = Time)
  } else {
    # Message and break if error input has an unexpected value
    stop("Incorrect or missing error type. \n Please choose from: \n -Relative \n -Negative \n -Density")
  }
  
  # Melt dataframe
  plot_data_long = melt(plot_data, id="Time")
  
  # One axis tick per prediction day
  axis_breaks = c(1,seq(1440, (length_weeks$pred_weeks*7)*1440, length.out = ((length_weeks$pred_weeks*7)-1)))
  custom_ticks = seq(from=1, to=length_weeks$pred_weeks*7, by=1)
  
  # Plot
  ggplot(data = plot_data_long, aes(x=Time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + xlab("Time in Days from date of prediction") +
    geom_line(size=.8, alpha=1) + 
    scale_x_continuous(breaks=axis_breaks, labels=custom_ticks) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("Average", error_type,"error from prediction date", sep = " "))
}

###########################################################################################################

relative_error_timeseries_singlelink = function(error_type, link_number, method, spike_parameter) {
  # Create x axis (only for proper indexing in the dataframe)
  Time = 1:(1440*7*length_weeks$pred_weeks)
  # Select type of error and create corresponding dataframe with aggregated results
  if (error_type == "Relative") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           Null = null_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           Fourier = fourier_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           STL = stl_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           Segmentation = segmentation_results[[method]][[spike_parameter]]$relative_errors$relative_error[link_number,],
                           Time = Time)
  } else if (error_type == "Negative") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           Null = null_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           Fourier = fourier_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           STL = stl_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           Segmentation = segmentation_results[[method]][[spike_parameter]]$relative_errors$relative_error_neg[link_number,],
                           Time = Time)
  } else if (error_type == "Density") {
    plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           Null = null_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           Fourier = fourier_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           STL = stl_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           Hybrid = hybrid_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           Segmentation = segmentation_results[[method]][[spike_parameter]]$relative_errors$density_error[link_number,],
                           Time = Time)
  } else {
    # Message and break if error input has an unexpected value
    stop("Incorrect or missing error type. \n Please choose from: \n -Relative \n -Negative \n -Density")
  }
  
  # Melt dataframe
  plot_data_long = melt(plot_data, id="Time")
  
  # One axis tick per prediction day
  axis_breaks = c(1,seq(1440, (length_weeks$pred_weeks*7)*1440, length.out = ((length_weeks$pred_weeks*7)-1)))
  custom_ticks = seq(from=1, to=length_weeks$pred_weeks*7, by=1)
  
  # Plot
  ggplot(data = plot_data_long, aes(x=Time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + xlab("Time in Days from date of prediction") +
    geom_line(size=.8, alpha=0.5) + 
    scale_x_continuous(breaks=axis_breaks, labels=custom_ticks) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste(error_type,"error in link", links_list[link_number], "from prediction date", sep = " "))
}


###########################################################################################################

quantile_error_singlelink = function(quantile, error_type, link_number, method, spike_parameter) {
  
  false_x_axis = 1:100
  # Create dataframe to plot depending on the needed data
  if (quantile == 1) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$ab_error[link_number,],
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$rms_error[link_number,],
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
        - \"rms\" (root mean squared error)")
    }
    
  } else if (quantile == 95) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$ab_error95[link_number,],
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$rms_error95[link_number,],
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
        - \"rms\" (root mean squared error)")
    }
  } else if (quantile == 99) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$ab_error99[link_number,],
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$rms_error99[link_number,],
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
  } else if (quantile == custom_quantile_start) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$ab_error_custom[link_number,],
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors$rms_error_custom[link_number,],
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
  } else {
    # Error message missing quantile
    stop("Incorrect or missing quantile input. \n Please choose from: \n - quantile = 1 (results for range 1-100) \n 
        - quantile = 95 (results for range 95-100) \n - quantile = 99 (results for range 99-100 \n 
         - quantile = custom_quantile_start (results for custom range)")
  }
  
  # Melt it
  plot_data_long = melt(plot_data, id="false_x_axis")
  # Define custom ticks for x axis
  if (quantile == 1) {
    custom_ticks = c(1,seq(from = 10, to= 100, length.out = 10))
  } else {
    custom_ticks = c(quantile, seq(from = quantile + ((100-quantile)/10), to = 100, length.out = 10))
  }  

  # Plot
  ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
    ylab("Error") + xlab("Percentile of travel times") +
    geom_line(size=1.5) + 
    scale_x_continuous(breaks=c(1,seq(from = 10, to= 100, length.out = 10)), labels=custom_ticks) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("AB/RMS error for link", links_list[link_number], "across percentiles of travel time", sep = " "))
  
}

#######################################################################################################################################

quantile_error = function(quantile, error_type, method, spike_parameter) {
  
  false_x_axis = 1:100
  # Create dataframe to plot depending on the needed data
  if (quantile == 1) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_avg,
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_avg,
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
    
  } else if (quantile == 95) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error95_avg,
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error95_avg,
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
  } else if (quantile == 99) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error99_avg,
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error99_avg,
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
  } else if (quantile == custom_quantile_start) {
    if (error_type == "ab") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$ab_error_custom_avg,
                             false_x_axis)
    } else if (error_type == "rms") {
      plot_data = data.frame(Published = thales_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             Null = null_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             STL = stl_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             Fourier = fourier_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             Hybrid = hybrid_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             Segmentation = segmentation_results[[method]][[spike_parameter]]$quantile_errors_avg$rms_error_custom_avg,
                             false_x_axis)
    } else {
      # Error message missing method
      stop("Incorrect or missing method input. \n Please choose from: \n - \"ab\" (absolute error) \n 
           - \"rms\" (root mean squared error)")
    }
  } else {
    # Error message missing quantile
    stop("Incorrect or missing quantile input. \n Please choose from: \n - quantile = 1 (results for range 1-100) \n 
        - quantile = 95 (results for range 95-100) \n - quantile = 99 (results for range 99-100 \n 
         - quantile = custom_quantile_start (results for custom range)")
  }
  
  # Melt it
  plot_data_long = melt(plot_data, id="false_x_axis")
  # Define custom ticks for x axis
  if (quantile == 1) {
    custom_ticks = c(1,seq(from = 10, to= 100, length.out = 10))
  } else {
    custom_ticks = c(quantile, seq(from = quantile + ((100-quantile)/10), to = 100, length.out = 10))
  }

  # Plot
  ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
    ylab("Relative Error") + 
    #xlab("Percentile of travel times") +
    geom_line(size=1.5) + theme(text = element_text(size=24),axis.text.x = element_text(hjust = 1)) + 
     labs(x="Percentile of Travel Time")+
    scale_x_continuous(breaks=c(1,seq(from = 10, to= 100, length.out = 10)), labels=custom_ticks) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0,0.5)) +
    scale_colour_discrete(name  =label_mx, breaks=c("Published", "Null", "STL", "Fourier", "Hybrid"),
                          labels=c("Published Model", "Null Model", "STL Model", "Fourier Model", "Hybrid Model")) +
    theme(text = element_text(size=18),legend.position = c(0.14, 0.90)) 
    #ggtitle(paste("Average AB/RMS error for all links across percentiles of travel time", sep = " "))
  
  ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
    ylab("Relative Error") + 
    labs(x="Percentile of Travel Time")+
    #theme(axis.title.x = element_blank()) + 
    geom_line(size=1.5) + theme(text = element_text(size=24),axis.text.x = element_text(angle = 0)) + 
    scale_x_continuous(breaks=c(1,seq(from = 10, to= 100, length.out = 10)), labels=custom_ticks) +
    scale_colour_discrete(name  =label_mx)+
    #breaks=c("Published", "Null", "STL", "Fourier", "Hybrid"),
    #                      labels=c("Published Model", "Null Model", "STL Model", "Fourier Model", "Hybrid Model"))  +
    theme(legend.position = c(0.14, 0.90)) + theme(legend.text=element_text(size=18)) 

  
  
  #ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
  #  ylab(paste("Abs of Relative Error", sep = " ")) + 
  #  xlab("Percentile of travel time") +
  #  theme(axis.title.x = element_blank()) + 
  #  geom_line(size=1.3) + theme(text = element_text(size=30),axis.text.x = element_text(angle = 0, hjust = 1)) + 
  #  scale_x_continuous(breaks=custom_ticks, labels=as.character(custom_ticks)) +
  #  scale_colour_discrete(name  ="M11", breaks=c("Published", "Null", "STL", "Fourier", "Hybrid"),
  #                        labels=c("Published Model", "Null Model", "STL Model", "Fourier Model", "Hybrid Model"))  +
  #  theme(legend.position = c(0.1, 0.85)) + theme(legend.text=element_text(size=22))
  
}


#############################################################################################################

relative_error_daytime_profile = function(profile, error_type) {
  # Create date and time index
  date_create = seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min")
  # Remove the last entry
  date = date_create[1:(length(date_create)-1)]
  # Remove the date and leave time of the day
  time = substr(date,12,19)
  #Define the labels
  seqq = seq(1,1440, 60)
  time_label = time[seqq]
  
  names = matrix("a", nrow = length(names(methods_parameters)), ncol = length(methods_parameters[[method]]))
  plot_data = data.frame(time)
  
  
  if (error_type == "Relative") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,fourier_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error)
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,stl_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error)
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,hybrid_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error)
        }
      }
    } else if (profile == "hybrid_new") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,hybrid_new_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error)
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else if (error_type == "Negative") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,fourier_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error_neg)
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,stl_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error_neg)
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,hybrid_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_error_neg)
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else if (error_type == "Density") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,fourier_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_density_error)
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,stl_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_density_error)
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = with(plot_data,hybrid_results[[method]][[as.character(spike_parameter)]]$daytime_errors$day_time_density_error)
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else {
    stop("Incorrect or missing error type. \n Please choose from: \n -Relative \n -Negative \n -Density")
  }
  
  # Melt it
  plot_data_long = melt(plot_data, id="time")
  
  # Plot
  ggplot(data = plot_data_long, aes(x=time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + xlab("Time of the day") +
    geom_line(size=.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_x_discrete(breaks=time_label, labels=as.character(time_label)) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("Average", error_type, "error, using",profile,"method for all links across times of the day"))
}


#############################################################################################################

relative_error_daytime_profile_singlelink = function(link_number, profile, error_type) {
  # Create date and time index
  date_create = seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min")
  # Remove the last entry
  date = date_create[1:(length(date_create)-1)]
  # Remove the date and leave time of the day
  time = substr(date,12,19)
  #Define the labels
  seqq = seq(1,1440, 60)
  time_label = time[seqq]
  
  names = matrix("a", nrow = length(names(methods_parameters)), ncol = length(methods_parameters[[method]]))
  plot_data = data.frame(time)
  
  if (error_type == "Relative") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_matrix[link_number,]
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_matrix[link_number,]
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_matrix[link_number,]
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else if (error_type == "Negative") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,]
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,]
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_error_neg_matrix[link_number,]
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else if (error_type == "Density") {
    if (profile == "fourier") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_density_error_matrix[link_number,]
        }
      }
    } else if (profile == "stl") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_density_error_matrix[link_number,]
        }
      }
    } else if (profile == "hybrid") {
      for (method in names(methods_parameters)) {
        for (spike_parameter in methods_parameters[[method]]) {
          plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$day_time_error_matrices$day_time_density_error_matrix[link_number,]
        }
      }
    } else {
      stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
    }
  } else {
    stop("Incorrect or missing error type. \n Please choose from: \n -Relative \n -Negative \n -Density")
  }
  
  # Melt it
  plot_data_long = melt(plot_data, id="time")
  
  # Plot
  ggplot(data = plot_data_long, aes(x=time, y=value, colour=variable, group = variable)) + 
    ylab(paste(error_type,"Error", sep = " ")) + xlab("Time of the day") +
    geom_line(size=.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_x_discrete(breaks=time_label, labels=as.character(time_label)) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("Average", error_type, "error, using",profile,"method for all links across times of the day"))
}


#############################################################################################################

quantile_error_profile_singlelink = function(link_number, quantile, profile, error_type) {
  
  false_x_axis = 1:100
  # Create date and time index
  date_create = seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min")
  # Remove the last entry
  date = date_create[1:(length(date_create)-1)]
  
  names = matrix("a", nrow = length(names(methods_parameters)), ncol = length(methods_parameters[[method]]))
  plot_data = data.frame(false_x_axis)
  
  if (quantile == 1) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == 95) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error95[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error95[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error95[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error95[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error95[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error95[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == 99) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error99[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error99[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error99[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error99[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error99[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error99[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == custom_quantile_start) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error_custom[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error_custom[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$ab_error_custom[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error_custom[link_number,]
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error_custom[link_number,]
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors$rms_error_custom[link_number,]
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else {
    # Error message missing quantile
    stop("Incorrect or missing quantile input. \n Please choose from: \n - quantile = 1 (results for range 1-100) \n 
         - quantile = 95 (results for range 95-100) \n - quantile = 99 (results for range 99-100 \n 
         - quantile = custom_quantile_start (results for custom range)")
  }
  
  
  # Melt it
  plot_data_long = melt(plot_data, id="false_x_axis")
  # Define custom ticks for x axis
  if (quantile == 1) {
    custom_ticks = c(1,seq(from = 10, to= 100, length.out = 10))
  } else {
    custom_ticks = c(quantile, seq(from = quantile + ((100-quantile)/10), to = 100, length.out = 10))
  }
  
  # Plot
  ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
    ylab("Error") + xlab("Percentile of travel times") +
    geom_line(size=.5) + 
    scale_x_continuous(breaks=c(1,seq(from = 10, to= 100, length.out = 10)), labels=custom_ticks) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("Average ", error_type," error for link ",links_list[link_number]," across percentiles of travel time", sep = " "))
  
}

#############################################################################################################

quantile_error_profile = function(quantile, profile, error_type) {
  
  false_x_axis = 1:100

  names = matrix("a", nrow = length(names(methods_parameters)), ncol = length(methods_parameters[[method]]))
  plot_data = data.frame(false_x_axis)
  
  if (quantile == 1) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == 95) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error95_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error95_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error95_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error95_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error95_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error95_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == 99) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error99_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error99_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error99_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error99_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error99_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error99_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else if (quantile == custom_quantile_start) {
    if (error_type == "ab") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_custom_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_custom_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$ab_error_custom_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else if (error_type == "rms") {
      if (profile == "fourier") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = fourier_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_custom_avg
          }
        }
      } else if (profile == "stl") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = stl_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_custom_avg
          }
        }
      } else if (profile == "hybrid") {
        for (method in names(methods_parameters)) {
          for (spike_parameter in methods_parameters[[method]]) {
            plot_data[[paste(method, "_", spike_parameter, sep = "")]] = hybrid_results[[method]][[as.character(spike_parameter)]]$quantile_errors_avg$rms_error_custom_avg
          }
        }
      } else {
        stop("Incorrect or missing profile choice. \n Please choose from: \n -fourier \n -stl \n -hybrid")
      }
    } else {
      stop("Incorrect or missing error type. \n Please choose from: \n -rms \n -ab")
    }
  } else {
    # Error message missing quantile
    stop("Incorrect or missing quantile input. \n Please choose from: \n - quantile = 1 (results for range 1-100) \n 
         - quantile = 95 (results for range 95-100) \n - quantile = 99 (results for range 99-100 \n 
         - quantile = custom_quantile_start (results for custom range)")
  }
  
  
  # Melt it
  plot_data_long = melt(plot_data, id="false_x_axis")
  # Define custom ticks for x axis
  if (quantile == 1) {
    custom_ticks = c(1,seq(from = 10, to= 100, length.out = 10))
  } else {
    custom_ticks = c(quantile, seq(from = quantile + ((100-quantile)/10), to = 100, length.out = 10))
  }
  
  # Plot
  ggplot(data = plot_data_long, aes(x=false_x_axis, y=value, colour=variable, group = variable)) + 
    ylab("Error") + xlab("Percentile of travel times") +
    geom_line(size=.5) + 
    scale_x_continuous(breaks=c(1,seq(from = 10, to= 100, length.out = 10)), labels=custom_ticks) +
    theme(legend.title=element_blank()) + 
    ggtitle(paste("Average ", error_type," error using", profile ," method for all links across percentiles of travel time", sep = " "))
  
  }



