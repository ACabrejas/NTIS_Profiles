  ## --- Program description -------------------------------------------------------------
  ##
  ##     Goal: Calculate profiles for weeks 9-12 and assess errors in any Quantile Range
  ##
  ##
  ##     IN  : mX_data_selected_and_links_list_AdvBackgroundSpikes_th_0.X.RData
  ##     OUT : STL & Hybrid Profiles. PLOTS: Error across quantiles. Daytime errors (ALL/ONLY NEGATIVE) 
  ##
  ## --- Initialize program ------------------------------------------------------------
  
  ## Clear-all
  #rm(list = ls()) # Clear variables
  #graphics.off()  # Clear plots
  #cat("\014")     # Clear console
  
  mX_list = c("m6", "m11")
  
  for (mX in mX_list) {
    ## Choose motorway
    mX <- "m11"
    
    ## Custom quantile range start (All, top 95% and top 99% are always included)
    custom_quantile_start = 90
    
    ## Load motorway data
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    #file_name <- paste(mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_0.8.RData',sep="")
    file_name = paste(mX,"_data_spikes_backgrund.rda", sep = "")
    load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
    links_list <- links_list_df$link_id
    
    ## SOURCE FILES (user functions)
    source("./profiles_functions/plots_profiles.R")
    source("./profiles_functions/rms_error.R")
    source("./profiles_functions/data_length.R")
    source("./profiles_functions/get_link_data.R")
    source("./profiles_functions/get_method_parameters.R")
    source("./profiles_functions/create_profile_storage.R")
    source("./profiles_functions/calculate_profile.R")
    source("./profiles_functions/relative_errors.R")
    source("./profiles_functions/relative_errors_neg.R")
    source("./profiles_functions/density_errors.R")
    source("./profiles_functions/basic_errors.R")
    source("./profiles_functions/daytime_errors.R")
    source("./profiles_functions/errors_across_links.R")
    source("./profiles_functions/Fourier_Profile.R")
    source("./profiles_functions/Thales_Profile.R")
    source("./profiles_functions/Null_Profile.R")
    source("./profiles_functions/STL_Profile.R")
    source("./profiles_functions/Hybrid_Profile.R")
    
    ## LIBRARIES
    library(ggplot2)
    library(reshape2)
    
    # Define length of timeseries measured in weeks (3 variables under $, span_weeks, total_weeks, train_weeks)
    length_weeks = data_length(mX)
    
    # Create a list with the names and parameters used in each method used for background and spikes
    methods_parameters = get_methods_parameters()
    
    #methods_parameters[[2]] = NULL
    #methods_parameters[[2]] = NULL
    #methods_parameters[[1]] = c(1,1.5)
    
    
    num_param = 0
    for (i in 1:(length(names(methods_parameters)))) {
      num_param = num_param + length(methods_parameters[[i]])
    }
    total_iterations = length(links_list) * num_param
    # Length of Links List and selector for Isolated Links
    #link = links_list[1]
    #links_list = links_list[1:13]
    # Isolated parameters and methods
    #method = names(methods_parameters[1])
    #spike_parameter = methods_parameters[[method]][[1]]
    # Isolated methods
    #profile_algorithm = "stl"
    
    ## LOOP THROUGH DIFFERENT METHODS FOR BACKGROUND AND SPIKES
    for (method in names(methods_parameters)) {
      print(paste("Calculating profiles using splitting method: ", method))
      ## LOOP THROUGH DIFFERENT PARAMETERS FOR THE CHOSEN METHOD
      for (spike_parameter in methods_parameters[[method]]) {
        print(paste("Current threshold parameter in method = ",spike_parameter))
        ## LOOP THROUGH LINKS AND CALCULATE PROFILES
        for (link in links_list) {
          print(paste("Calculating link number ",which(links_list==link),"/",length(links_list)," within this configuration. ID: ",link))
          
          link_data = get_link_data(link = link)
          
          # Forecasts from currently used model
          thales_results = calculate_profile(profile_algorithm = "thales", profile_storage = thales_results)
          # Forecasts always the median travel time for the link
          null_results = calculate_profile(profile_algorithm = "null", profile_storage = null_results)
          # Forecasts using FFT
          fourier_results = calculate_profile(profile_algorithm = "fourier", profile_storage = fourier_results)
          # Forecasts using STL
          stl_results = calculate_profile(profile_algorithm = "stl", profile_storage = stl_results)
          # Hybrid Fourier-STL forecasts
          hybrid_results = calculate_profile(profile_algorithm = "hybrid", profile_storage = hybrid_results)
          
          #rm(link_data)
        }
      }
    }
    
    for (method in names(methods_parameters)) {
      ## LOOP THROUGH DIFFERENT PARAMETERS FOR THE CHOSEN METHOD
      for (spike_parameter in methods_parameters[[method]]) {
        ## CALCULATE ERRORS ACROSS LINKS
        fourier_results = errors_across_links(profile_storage = fourier_results)
        thales_results = errors_across_links(profile_storage = thales_results)
        null_results = errors_across_links(profile_storage = null_results)
        stl_results = errors_across_links(profile_storage = stl_results)
        hybrid_results = errors_across_links(profile_storage = hybrid_results)
      }
    }
    
    
    
    file_out_name = paste(mX,"_profiles_results_v3.rda", sep = "")
    file_out_path = paste("../00_Data/05_New_Profiles/", file_out_name, sep = "")
    save(thales_results, null_results, fourier_results, stl_results, hybrid_results, file = file_out_path)
    
    rm(list=ls()[! ls() %in% c("mX","mX_list")])
    graphics.off()  # Clear plots
    cat("\014")     # Clear console
  }
  
  