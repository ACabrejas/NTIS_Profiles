## --- Program description -------------------------------------------------------------
##
##     Goal: Split data between Background and Spikes using different methods
##
##
##     IN  : mX_data_selected_and_links_list_A.RData
##     OUT : STL & Hybrid Profiles. PLOTS: Error across quantiles. Daytime errors (ALL/ONLY NEGATIVE) 
##
## --- Initialize program ------------------------------------------------------------

## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

## Choose motorway
mX <- "m25" 

## Load Data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file_name <- paste(mX,'_data_selected_and_links_list_A.RData',sep="")
load(paste('../00_Data/03_Clean_data/',file_name, sep = ''))
links_list <- unique(m_data_selected$link_id)
link = links_list[1]

## Decomposition methods
## METHOD 1: Scale travel times as a fraction of free flow time, based on link length and max speed
scaled_threshold_param = seq(from = 1.2, to = 2, by = 0.2)

# METHOD 2: Scale travel time as a fraction of average of daily travel time maxima
max_day_param_traveltime = seq(from = 0.7, to = 1.2, by = 0.1)

# METHOD 2: Scale travel time as a fraction of average of daily density maxima
max_day_param_density = seq(from = 0.7, to = 1.7, by = 0.2)

# METHOD 3: Wavelet using a fraction of interquantile range
#wavelet_iqr = seq(from = 0.75, to = 2, by = 0.25)

# METHOD 4: Wavelet using a fraction of the range of coefficients

## SOURCE FILES (user functions)
source('./Background_Spikes_Functions/data_length.R')
source('./Background_Spikes_Functions/background_spikes_storage.R')
source('./Background_Spikes_Functions/decompose.R')
source('./Background_Spikes_Functions/scaled_threshold.R')
source('./Background_Spikes_Functions/max_daily.R')

# Define length of timeseries measured in weeks (3 variables under $, span_weeks, total_weeks, train_weeks)
length_weeks = data_length(mX)

## Loop through links and calculate background and spikes
for (link in links_list) {
  
  # Print progress
  print(paste("Calculating link number ",which(links_list==link),"/",length(links_list),". ID: ",link, ". Progress for current run = ", round(100*which(links_list==link)/length(links_list),2),"%.", sep = ""))
  
  # Get Link data
  travel_time <- m_data_selected$travel_time[m_data_selected$link_id == link]
  flow <- m_data_selected$traffic_flow[m_data_selected$link_id == link]
  
  ## If this is the first iteration, create the storage for the chosen method.
  # The output will be a nested named list of the form:
  # Level 1: Parameter names
  # Level 2: Background, Spike and Spike flag matrices
  # Level 3: In each matrix, rows are links and columns are data points (time)
  if(link == links_list[1]) {
    #threshold = background_spikes_storage(parameter_list = scaled_threshold_param)
    #max_day_traveltime = background_spikes_storage(parameter_list = max_day_param_traveltime)
    max_day_density = background_spikes_storage(parameter_list = max_day_param_density)
    
  }
  
  #threshold = decompose(algorithm = "threshold", parameters = scaled_threshold_param, storage = threshold)
  
  #max_day_traveltime = decompose(algorithm = "max_day_traveltime", parameters = max_day_param_traveltime, storage = max_day_traveltime)
  
  max_day_density = decompose(algorithm = "max_day_density", parameters = max_day_param_density, storage = max_day_density)
  
  #wavelet = decompose(algorithm = "wavelet", parameters = wavelet_param, storage = wavelet)
  
}



#background_spikes = list(threshold = threshold, max_day_traveltime = max_day_traveltime, max_day_density = max_day_density)
background_spikes = list(max_day_density = max_day_density)

data = list(m_data_selected = m_data_selected, background_spikes = background_spikes)


save(data, links_list_df, file = paste("../00_Data/04_Processed_data/",mX,"_data_spikes_background_density_only_high.rda", sep = ""))
