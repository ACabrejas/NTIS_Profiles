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
mX <- "m11"

## Custom quantile range start (All, top 95% and top 99% are always included)
custom_quantile_start = 90

## Load motorway data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file_name <- paste(mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_0.8.RData',sep="")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- links_list_df$link_id
links_list = links_list[1:13]
# Length of Links List and selector for Isolated Links
link = links_list[1]
#links_list = links_list[1:13]

## SOURCE FILES (user functions)
source("./functions/plots_profiles.R")
source("./functions/rms_error.R")
source("./functions/data_length.R")
source("./functions/get_link_data.R")
source("./functions/create_profile_storage.R")
source("./functions/calculate_profile.R")
source("./functions/relative_errors.R")
source("./functions/relative_errors_neg.R")
source("./functions/density_errors.R")
source("./functions/basic_errors.R")
source("./functions/daytime_errors.R")
source("./functions/errors_across_links.R")
source("./functions/Fourier_Profile.R")
source("./functions/Thales_Profile.R")
source("./functions/Null_Profile.R")
source("./functions/STL_Profile.R")
source("./functions/Hybrid_Profile.R")

## LIBRARIES
library(ggplot2)
library(reshape2)

# Define length of timeseries measured in weeks (3 variables under $, span_weeks, total_weeks, train_weeks)
length_weeks = data_length(mX)

## LOOP THROUGH LINKS AND CALCULATE PROFILES
for (link in links_list) {
  print(paste("Calculating link number ",which(links_list==link),"/",length(links_list),". ID: ",link, ". Progress for current run = ", round(100*which(links_list==link)/length(links_list),2),"%.", sep = ""))
  
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

## CALCULATE ERRORS ACROSS LINKS
fourier_results = errors_across_links(profile_storage = fourier_results)
thales_results = errors_across_links(profile_storage = thales_results)
null_results = errors_across_links(profile_storage = null_results)
stl_results = errors_across_links(profile_storage = stl_results)
hybrid_results = errors_across_links(profile_storage = hybrid_results)


# RESULTS CHECK ON A LINK_BY_LINK BASIS
i=13
# Results daytime check for a SINGLE LINK
# Choose "Relative", "Negative" or "Density" as output
relative_error_daytime_singlelink(error_type = "Relative", link_number = i)
relative_error_daytime_singlelink(error_type = "Negative", link_number = i)
relative_error_daytime_singlelink(error_type = "Density", link_number = i)

# Results relative errors over time for a SINGLE LINK
relative_error_timeseries_singlelink(error_type = "Relative", link_number = i)
relative_error_timeseries_singlelink(error_type = "Negative", link_number = i)
relative_error_timeseries_singlelink(error_type = "Density", link_number = i)

# Results quantile errors for a SINGLE LINK
quantile_error_singlelink(quantile = 1, method = "ab", link_number = i)
quantile_error_singlelink(quantile = 95, method = "ab", link_number = i)
quantile_error_singlelink(quantile = 99, method = "ab", link_number = i)
quantile_error_singlelink(quantile = custom_quantile_start, method = "ab", link_number = i)

quantile_error_singlelink(quantile = 1, method = "rms", link_number = i)
quantile_error_singlelink(quantile = 95, method = "rms", link_number = i)
quantile_error_singlelink(quantile = 99, method = "rms", link_number = i)
quantile_error_singlelink(quantile = custom_quantile_start, method = "rms", link_number = i)
i=i+1


# RESULTS CHECK ON A GLOBAL BASIS
# Daytime
relative_error_daytime("Relative")
relative_error_daytime("Negative")
relative_error_daytime("Density")

# Results percentile errors
quantile_error(quantile = 1, method ="ab")
quantile_error(quantile = 95, method ="ab")
quantile_error(quantile = 99, method ="ab")
quantile_error(quantile = custom_quantile_start, method ="ab")

quantile_error(quantile = 1, method ="rms")
quantile_error(quantile = 95, method ="rms")
quantile_error(quantile = 99, method ="rms")
quantile_error(quantile = custom_quantile_start, method ="rms")

# Results relative errors over time
relative_error_timeseries("Relative")
relative_error_timeseries("Negative")
relative_error_timeseries("Density")

# Accuracy across links
plot(fourier_results$mean_ab_errors$mean_ab_error_background, xaxt='n', ylim = c(0.00,0.12), col = "blue", pch=20, xlab = "", ylab = "Relative Error Background")
points(thales_results$mean_ab_errors$mean_ab_error_background, col = "black", pch=20)
points(null_results$mean_ab_errors$mean_ab_error_background, col = "orange", pch=20)
points(stl_results$mean_ab_errors$mean_ab_error_background, col = "green", pch=20)
points(hybrid_results$mean_ab_errors$mean_ab_error_background, col = "purple", pch=20)
axis(1, at=seq(from= 1, to = length(links_list), by=1), labels = links_list, las = 2)
legend(1,0.12, legend=c("Null Profile","Published Profile","Fourier Profile","STL Profile", "Hybrid Profile"),
       col=c("orange",'black','blue','green', 'purple'),lty=c(F,F,F,F,F),cex=0.8, pch=c(20,20,20,20,20), pt.cex = 1)

plot(fourier_results$mean_ab_errors$mean_ab_error_spikes, xaxt='n', ylim = c(0,.7), col = "blue", pch=20, xlab = "", ylab = "Relative Error")
points(thales_results$mean_ab_errors$mean_ab_error_spikes, col = "black", pch=20)
points(null_results$mean_ab_errors$mean_ab_error_spikes, col = "orange", pch=20)
points(stl_results$mean_ab_errors$mean_ab_error_spikes, col = "green", pch=20)
points(hybrid_results$mean_ab_errors$mean_ab_error_spikes, col = "purple", pch=20)
axis(1, at=seq(from= 1, to = length(links_list), by=1), labels = links_list, las = 2)
legend(0.8,0.70, legend=c("Null Profile","Published Profile","Fourier Profile","STL Profile", "Hybrid Profile"),
       col=c("orange",'black','blue','green', 'purple'),lty=c(F,F,F,F,F),cex=0.6, pch=c(20,20,20,20,20), pt.cex = 1)


# EXPLORATORY PLOTS (MAYBE BETTER IN THE PROCESSING ALGORITHMS)
# Percentiles of travel time (global)

# Percentiles of travel time per link
