wavelet = function(k, thresholding_method) {
  
  # Manual switch for the thresholding method
  thresholding_method = 1
  # Manual selector for k (method 1)
  k = 1.75
  # Manual selector for threshold (method 2)
  thresh=0.02
  
  # Substract mean from the time series? (turns it into oscillations around the mean)
  minusmean = T
  if (minusmean == T) {
    oscillations = ts(travel_time - mean(travel_time), frequency = 1440)
  } else {
    oscillations = ts(travel_time, frequency = 1440)
  }
  
  # Calculate length of the overlap
  data_length = length(travel_time)
  overlap = 2*(2**16) - data_length
  
  # Compute DWT
  w <- dwt(oscillations, filter="haar", boundary="reflection")
  
  levels = c(1:w@level)
  range=c(1,9)
  
  
  ## FIRST METHOD
  if (thresholding_method == 1) {
    # In all levels (timescales)
    for (i in levels) {
      
      # Select only timescales in the range
      if (i >= range[1] & i <= range[2]) {
        
        # Take all coefficients and eliminate oscillations above/below certian threshold
        # In this case the threshold is k*IQR of the coefficients
        
        ## WHY ARE ALL COEFFICIENTS PUT TOGETHER???
        C = c(w@V[[i]][,1], w@W[[i]][,1]) 
        q = quantile(C)
        iqr = q[[4]]-q[[2]]
        Tminus = q[[2]] - k*iqr
        Tplus = q[[4]] + k*iqr
        message("Level ", i, ": Tplus = ", round(Tplus,2), " Tminus = ", round(Tminus,2))
        C[C>Tplus | C < Tminus] = 0.0
        w@V[[i]][,1] = C[1:(length(C)/2)]
        w@W[[i]][,1] = C[(length(C)/2+1):(length(C))]
      }
    }
    ## SECOND METHOD
  } else if (thresholding_method == 2) {
    C = c(0.0,0.0)
    # In all levels (timescales)
    for (i in c(1:w@level)) {
      # Select only timescales in the range
      if (i >= range[1] & i <= range[2]) {
        
        # Put all coefficients in the same series
        C = c(C, w@V[[i]][,1], w@W[[i]][,1]) 
      }
    }
    # Take all coefficients and eliminate oscillations above/below certian threshold
    # In this case the threshold is a fraction of the quantile range (it discards top and bottom X% of coefficients)
    q = quantile(C, c(thresh,1.0-thresh))
    Tminus=q[[1]]
    Tplus=q[[2]]
    message("Global : Tplus = ", round(Tplus,2), " Tminus = ", round(Tminus,2))
    
    for (i in c(1:w@level)) 
    {
      if (i >= range[1] & i <= range[2])
      {
        n=length(w@V[[i]])
        C = c(w@V[[i]][,1], w@W[[i]][,1]) 
        C[C>Tplus | C < Tminus] = 0.0
        w@V[[i]][,1] = C[1:n]
        w@W[[i]][,1] = C[(n+1):(2*n)]
        message("Timescale: ",length(w@W[[1]])/n, " minutes")
      }
    }
  } else {
    stop("Error while choosing method for the wavelet transform. \n
         Please select either \"1\" or \"2\".")
  }
  
  # Reconstruct timeseries
  X1 = idwt(w)
  
  #####
  background = as.numeric(X1) + mean(travel_time)
  spikes = travel_time - background
  
  # DEBUG
  plot(travel_time, type = 'l')
  lines(background, col='red')
  
  plot(spikes, type = 'l')
  
  remainder = travel_time-X1
  plot(remainder)
  remainder.ts = ts(remainder, frequency = 10080)
  bla.stl = stl(remainder.ts, s.window = "periodic")
  plot(bla.stl)
  myhist = hist(bla.stl$time.series[,"remainder"], breaks = 200)
  plot(myhist$counts, log="y", type = 'h')

  
  
  output = list(background = background, spikes = spikes, spike_flag = spike_flag)
  
  return (output)
}

