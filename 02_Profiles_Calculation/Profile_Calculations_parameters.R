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
rm(list = ls()) # Clear variables
graphics.off()  # Clear plots
cat("\014")     # Clear console

## Choose motorway
mX <- "m6"

## Recalculate?
recalculate = T
testrun = T
save_results = F
## Custom quantile range start (All, top 95% and top 99% are always included)
custom_quantile_start = 50

## Load motorway data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#file_name <- paste(mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_0.8.RData',sep="")
file_name = paste(mX,"_data_spikes_background_threshold_only.rda", sep = "")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- links_list_df$link_id
#links_list = links_list[14:25]

##
if (recalculate == FALSE) {
  file_out_name = paste(mX,"_profiles_results_threshold.rda", sep = "")
  file_out_path = paste("../00_Data/05_New_Profiles/", file_out_name, sep = "")
  load(file = file_out_path)
}


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
source("./profiles_functions/ewmaweights.R")
source("./profiles_functions/Segmentation_Profile.R")
source("./profiles_functions/hybrid_profile_new.R")

source("./profiles_functions/Fourierlog_Profile.R")
source("./profiles_functions/STLlog_Profile.R")
source("./profiles_functions/Hybridlog_Profile.R")



## LIBRARIES
library(ggplot2)
library(reshape2)

# Define length of timeseries measured in weeks (3 variables under $, span_weeks, total_weeks, train_weeks)
length_weeks = data_length(mX)

# Create a list with the names and parameters used in each method used for background and spikes
methods_parameters = get_methods_parameters()
## DELETE PARAMETERS FOR TEST RUN
if (testrun) {
  methods_parameters = methods_parameters[1]
  methods_parameters[[1]] = methods_parameters[[1]][2]
  link = links_list[1]
  method = "threshold"
  spike_parameter = methods_parameters[[1]][[1]]
  #links_list = links_list[1:3]
}

# Calculate number of used parameters
num_param = 0
for (i in 1:(length(names(methods_parameters)))) {
  num_param = num_param + length(methods_parameters[[i]])
}
total_iterations = length(links_list) * num_param

labels_mx = c("M6", "M11", "M25")
if(mX=="m6"){label_mx = labels_mx[1]}else if(mX=="m11"){label_mx = labels_mx[2]}else if(mX=="m25"){label_mx=labels_mx[3]}


## DEBUGGING ####################################################

# Length of Links List and selector for Isolated Links
#link = links_list[1]
#links_list = links_list[1:13]
# Isolated parameters and methods
#method = names(methods_parameters[1])
#spike_parameter = methods_parameters[[method]][[1]]
# Isolated methods
#profile_algorithm = "stl"

################################################################

if (recalculate == TRUE) {
  ## LOOP THROUGH DIFFERENT METHODS FOR BACKGROUND AND SPIKES
  for (method in names(methods_parameters)) {
    print(paste("Calculating profiles using splitting method: ", method))
    ## LOOP THROUGH DIFFERENT PARAMETERS FOR THE CHOSEN METHOD
    for (spike_parameter in methods_parameters[[method]]) {
      print(paste("Spike_parameter = ", spike_parameter))
      #print(paste("Current threshold parameter in method = ",spike_parameter, ". Progress = ", round(100*(which(methods_parameters[[method]]==spike_parameter)+3*(which(names(methods_parameters)==method)-1))/num_param,2)), "%")
      ## LOOP THROUGH LINKS AND CALCULATE PROFILES
      for (link in links_list) {
        print(paste("Calculating link number ",which(links_list==link),"/",length(links_list)," within this configuration. ID: ",link))
        
        link_data = get_link_data(link = link, method = method, spike_parameter = spike_parameter)
        
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
        # Naive Segmentation profile
        segmentation_results = calculate_profile(profile_algorithm = "segmentation", profile_storage = segmentation_results)
        # Fourierlog 
        fourierlog_results = calculate_profile(profile_algorithm = "fourier", profile_storage = fourierlog_results)
        # Stllog
        stllog_results = calculate_profile(profile_algorithm = "stllog", profile_storage = stllog_results)
        # Hybrid log
        hybridlog_results = calculate_profile(profile_algorithm = "hybridlog", profile_storage = hybridlog_results)
        # SARIMA
        sarima_results = calculate_profile(profile_algorithm = "sarima", profile_storage = sarima_results)

        #rm(link_data)
      }
    }
  }
  
  for (method in names(methods_parameters)) {
    ## LOOP THROUGH DIFFERENT PARAMETERS FOR THE CHOSEN METHOD
    for (spike_parameter in methods_parameters[[method]]) {
      ## CALCULATE ERRORS ACROSS LINKS
      fourier_results = errors_across_links(profile_storage = fourier_results, method = method, spike_parameter = spike_parameter)
      thales_results = errors_across_links(profile_storage = thales_results, method = method, spike_parameter = spike_parameter)
      null_results = errors_across_links(profile_storage = null_results, method = method, spike_parameter = spike_parameter)
      stl_results = errors_across_links(profile_storage = stl_results, method = method, spike_parameter = spike_parameter)
      hybrid_results = errors_across_links(profile_storage = hybrid_results, method = method, spike_parameter = spike_parameter)
      segmentation_results = errors_across_links(profile_storage = segmentation_results, method = method, spike_parameter = spike_parameter)
      hybridlog_results = errors_across_links(profile_storage = hybridlog_results, method = method, spike_parameter = spike_parameter)
    }
  }
  
  if (save_results == T) {
    file_out_name = paste(mX,"_profiles_results_threshold.rda", sep = "")
    file_out_path = paste("../00_Data/05_New_Profiles/", file_out_name, sep = "")
    save(thales_results, null_results, fourier_results, stl_results, hybrid_results, segmentation_results,  file = file_out_path)
    #save(thales_results, fourier_results, stl_results, hybrid_results, file = file_out_path)
  }

  
}




################################################
######                                    ######
######      SAVE THE DATA NOW!!!!!!!!     ######
######                                    ######
################################################

# RESULTS CHECK ON A LINK_BY_LINK BASIS
i=1
method = "threshold"
spike_parameter = 1.1
error_type = "Relative"
spike_parameter = as.character(spike_parameter)
# Results daytime check for a SINGLE LINK
# Choose "Relative", "Negative" or "Density" as output
relative_error_daytime_singlelink(error_type = "Relative", link_number = i, method = method, spike_parameter = spike_parameter)
relative_error_daytime_singlelink(error_type = "Negative", link_number = i, method = method, spike_parameter = spike_parameter)
relative_error_daytime_singlelink(error_type = "Density", link_number = i, method = method, spike_parameter = spike_parameter)

# Results relative errors over time for a SINGLE LINK
relative_error_timeseries_singlelink(error_type = "Relative", link_number = i, method = method, spike_parameter = spike_parameter)
relative_error_timeseries_singlelink(error_type = "Negative", link_number = i, method = method, spike_parameter = spike_parameter)
relative_error_timeseries_singlelink(error_type = "Density", link_number = i, method = method, spike_parameter = spike_parameter)

# Results quantile errors for a SINGLE LINK
quantile_error_singlelink(quantile = 1, error_type = "ab", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = 95, error_type = "ab", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = 99, error_type = "ab", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = custom_quantile_start, error_type = "ab", link_number = i, method = method, spike_parameter = spike_parameter)

quantile_error_singlelink(quantile = 1, error_type = "rms", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = 95, error_type = "rms", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = 99, error_type = "rms", link_number = i, method = method, spike_parameter = spike_parameter)
quantile_error_singlelink(quantile = custom_quantile_start, error_type = "rms", link_number = i, method = method, spike_parameter = spike_parameter)
i=i+1


# RESULTS CHECK ON A GLOBAL BASIS
# Daytime
relative_error_daytime("Relative", method = method, spike_parameter = spike_parameter)
relative_error_daytime("Negative", method = method, spike_parameter = spike_parameter)
relative_error_daytime("Density", method = method, spike_parameter = spike_parameter)

# Results percentile errors
quantile_error(quantile = 1, error_type ="ab", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = 95, error_type ="ab", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = 99, error_type ="ab", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = custom_quantile_start, error_type ="ab", method = method, spike_parameter = spike_parameter)

quantile_error(quantile = 1, error_type ="rms", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = 95, error_type ="rms", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = 99, error_type ="rms", method = method, spike_parameter = spike_parameter)
quantile_error(quantile = custom_quantile_start, error_type ="rms", method = method, spike_parameter = spike_parameter)

# Results relative errors over time
relative_error_timeseries("Relative", method = method, spike_parameter = spike_parameter)
relative_error_timeseries("Negative", method = method, spike_parameter = spike_parameter)
relative_error_timeseries("Density", method = method, spike_parameter = spike_parameter)

# Accuracy across links
plot(fourier_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_background, xaxt='n', ylim = c(0.00,0.12), col = "blue", pch=20, xlab = "", ylab = "Relative Error Background")
points(thales_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_background, col = "black", pch=20)
points(null_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_background, col = "orange", pch=20)
points(stl_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_background, col = "green", pch=20)
points(hybrid_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_background, col = "purple", pch=20)
axis(1, at=seq(from= 1, to = length(links_list), by=1), labels = links_list, las = 2)
legend(1,0.12, legend=c("Null Profile","Published Profile","Fourier Profile","STL Profile", "Hybrid Profile"),
       col=c("orange",'black','blue','green', 'purple'),lty=c(F,F,F,F,F),cex=0.8, pch=c(20,20,20,20,20), pt.cex = 1)

plot(fourier_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_spikes, xaxt='n', ylim = c(0,.7), col = "blue", pch=20, xlab = "", ylab = "Relative Error")
points(thales_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_spikes, col = "black", pch=20)
points(null_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_spikes, col = "orange", pch=20)
points(stl_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_spikes, col = "green", pch=20)
points(hybrid_results[[method]][[spike_parameter]]$mean_ab_errors$mean_ab_error_spikes, col = "purple", pch=20)
axis(1, at=seq(from= 1, to = length(links_list), by=1), labels = links_list, las = 2)
legend(0.8,0.70, legend=c("Null Profile","Published Profile","Fourier Profile","STL Profile", "Hybrid Profile"),
       col=c("orange",'black','blue','green', 'purple'),lty=c(F,F,F,F,F),cex=0.6, pch=c(20,20,20,20,20), pt.cex = 1)

## COMPARISONS ACROSS METHODS (choose profile type, choose methods and parameters)
#Daytime Single Link
i=1
relative_error_daytime_profile_singlelink(link_number = i, profile = "stl", error_type = "Relative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "stl", error_type = "Negative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "stl", error_type = "Density")

relative_error_daytime_profile_singlelink(link_number = i, profile = "fourier", error_type = "Relative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "fourier", error_type = "Negative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "fourier", error_type = "Density")

relative_error_daytime_profile_singlelink(link_number = i, profile = "hybrid", error_type = "Relative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "hybrid", error_type = "Negative")
relative_error_daytime_profile_singlelink(link_number = i, profile = "hybrid", error_type = "Density")

# Quantile Single Link
quantile_error_profile_singlelink(link_number = i, quantile = 1, profile = "stl", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 95, profile = "stl", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 99, profile = "stl", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = custom_quantile_start, profile = "stl", error_type = "ab")

quantile_error_profile_singlelink(link_number = i, quantile = 1, profile = "fourier", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 95, profile = "fourier", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 99, profile = "fourier", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = custom_quantile_start, profile = "fourier", error_type = "ab")

quantile_error_profile_singlelink(link_number = i, quantile = 1, profile = "hybrid", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 95, profile = "hybrid", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = 99, profile = "hybrid", error_type = "ab")
quantile_error_profile_singlelink(link_number = i, quantile = custom_quantile_start, profile = "hybrid", error_type = "ab")


# Daytime all links
relative_error_daytime_profile(profile = "stl", error_type = "Relative")
relative_error_daytime_profile(profile = "stl", error_type = "Negative")
relative_error_daytime_profile(profile = "stl", error_type = "Density")

relative_error_daytime_profile(profile = "hybrid", error_type = "Relative")
relative_error_daytime_profile(profile = "hybrid", error_type = "Negative")
relative_error_daytime_profile(profile = "hybrid", error_type = "Density")

relative_error_daytime_profile(profile = "fourier", error_type = "Relative")
relative_error_daytime_profile(profile = "fourier", error_type = "Negative")
relative_error_daytime_profile(profile = "fourier", error_type = "Density")

# Quantile all links
quantile_error_profile(quantile = 1, profile = "stl", error_type = "ab")
quantile_error_profile(quantile = 95, profile = "stl", error_type = "ab")
quantile_error_profile(quantile = 99, profile = "stl", error_type = "ab")
quantile_error_profile(quantile = custom_quantile_start, profile = "stl", error_type = "ab")

quantile_error_profile(quantile = 1, profile = "fourier", error_type = "ab")
quantile_error_profile(quantile = 95, profile = "fourier", error_type = "ab")
quantile_error_profile(quantile = 99, profile = "fourier", error_type = "ab")
quantile_error_profile(quantile = custom_quantile_start, profile = "fourier", error_type = "ab")

quantile_error_profile(quantile = 1, profile = "hybrid", error_type = "ab")
quantile_error_profile(quantile = 95, profile = "hybrid", error_type = "ab")
quantile_error_profile(quantile = 99, profile = "hybrid", error_type = "ab")
quantile_error_profile(quantile = custom_quantile_start, profile = "hybrid", error_type = "ab")

# EXPLORATORY PLOTS (MAYBE BETTER IN THE PROCESSING ALGORITHMS)
# Percentiles of travel time (global)

# Percentiles of travel time per link
