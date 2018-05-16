## --- Program description -------------------------------------------------------------

##     Goal Section 1: Separate travel time into basic background + spikes
##     Goal Section 2: Separate travel time into advanced background + spikes (avg. max. daily)

## --- Initialize program ------------------------------------------------------------

## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

## Load libraries
library(stats)
library(graphics)


## Choose motorway
mX <- "m25" 

## Load Data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file_name <- paste(mX,'_data_selected_and_links_list_A.RData',sep="")
load(paste('../00_Data/03_Clean_data/',file_name, sep = ''))
links_list <- unique(m_data_selected$link_id)

## Velocity for re-scaling
v <- 28.5 ## 63.76 mph = 102.6 km/h = 28.5 m/s
v = 31.2928 ## 70mph only for M25
## Define threshold high and threshold low parameters for normal background and spikes
th_high <- 1.2
th_low <- 0.8

###### ONLY FOR ADVANCED (STEP 2) ########################
## Define threshold high threshold (0.8 = spikes begin at 80% of avg max day travel time for the link)
th_high2 <- 1.5
fill_gaps = TRUE #Prevents holes in the measurements where the spikes are
##########################################################

## Define background and spikes vectors
m_data_selected$background <- rep(0,nrow(m_data_selected))
m_data_selected$spikes <- rep(0,nrow(m_data_selected))
m_data_selected$spike_flag <- rep(0,nrow(m_data_selected))

## --- Go through all links ----------------------------------------------
k <- 1
for (link in links_list) {
  
  print(paste("Separating Background and Spikes. Completion = ", round(100*k/length(links_list),2), "%."))
  # Define travel time, background and spikes (scaled and non-scaled)
  travel_time <- m_data_selected$travel_time[m_data_selected$link_id == link]
  travel_time_scaled <- travel_time / (links_list_df$link_length[k]/v)

  background <- rep(median(travel_time),length(travel_time))
  background_scaled <- rep(1,length(travel_time))
  background[travel_time_scaled >= th_low & travel_time_scaled <= th_high] <- travel_time[travel_time_scaled >= th_low & travel_time_scaled <= th_high]
  background_scaled[travel_time_scaled >= th_low & travel_time_scaled <= th_high] <- travel_time_scaled[travel_time_scaled >= th_low & travel_time_scaled <= th_high]
  
  spikes <- rep(0,length(travel_time))
  spikes_scaled <- rep(0,length(travel_time_scaled))
  spike_flag <- rep(0,length(travel_time))
  #spikes[travel_time_scaled > th_high] <- travel_time[travel_time_scaled > th_high] - median(travel_time) ## The last element is the expected travel time in background mode
  spikes[travel_time_scaled > th_high] <- travel_time[travel_time_scaled > th_high] - (links_list_df$link_length[k]/v)
  spikes_scaled[travel_time_scaled > th_high] <- travel_time_scaled[travel_time_scaled > th_high] - 1 ## "1" is the scaled expected travel time
  spike_flag[travel_time_scaled > th_high] <- 1

  ## Write background and spikes to dataframe
  m_data_selected$background[m_data_selected$link_id == link] <- background
  m_data_selected$spikes[m_data_selected$link_id == link] <- spikes
  m_data_selected$spike_flag[m_data_selected$link_id == link] <- spike_flag
  
  ## Increment counter
  k <- k+1
}

## --- Save m_data_selected and links_list_df in RData file --------------------
file_name_2 <- paste('../00_Data/04_Processed_data/',mX,'_data_selected_and_links_list_BasicBackgroundSpikes.RData',sep="")
save(m_data_selected,links_list_df, file = file_name_2)

#####################################################################################################

## Clear-all
#rm(list = ls()) # Clear variables

rm(list= ls()[!(ls() %in% c('th_high2', 'mX', 'fill_gaps'))])

## Load motorway data
file_name <- paste(mX,'_data_selected_and_links_list_BasicBackgroundSpikes.RData',sep="")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- unique(m_data_selected$link_id)

## Define windows for average of max calculation
period_weeks <- 10
starting_week <- 0

## Define background and spikes vectors
m_data_selected$background_dens <- rep(0,nrow(m_data_selected))
m_data_selected$spikes_dens <- rep(0,nrow(m_data_selected))
m_data_selected$spike_flag_dens <- rep(0,nrow(m_data_selected))
m_data_selected$background_tt2 <- rep(0,nrow(m_data_selected))
m_data_selected$spikes_tt2 <- rep(0,nrow(m_data_selected))
m_data_selected$spike_flag_tt2 <- rep(0,nrow(m_data_selected))
## --- Go through all links ----------------------------------------------
counter_link <- 1
link<- links_list[1]
for (link in links_list) {
  print(paste('Current Link: ',link, '. Progress = ', round(100*counter_link/length(links_list),2), '%.'))
  ## Define travel time, background and spikes (scaled and non-scaled)
  travel_time <- m_data_selected$travel_time[m_data_selected$link_id == link]
  flow <- m_data_selected$traffic_flow[m_data_selected$link_id == link]
  flow[is.na(flow)] = median(flow, na.rm = TRUE)
  speed <- m_data_selected$traffic_speed[m_data_selected$link_id == link]
  speed[speed == 0] <- median(speed, na.rm = TRUE)
  speed[is.na(speed)] = median(speed, na.rm = TRUE)
  
  density <- flow/speed
  
  ## Calculate MAX DAILY DENSITY
  density_daily_max = numeric()
  travel_time_daily_max = numeric()
  for (week in 0:(period_weeks-1)) {
    for (day in 0:6) {
      index_start = 1+week*7*1440+day*1440
      index_end = index_start + 1439
      day_index = week*7+day
      density_daily_max[day_index] = max(density[index_start:index_end])
      travel_time_daily_max[day_index] = max(travel_time[index_start:index_end])
    }
  }
  ## Take the mean
  average_max_density = mean(density_daily_max, na.rm = TRUE)
  average_max_traveltime = mean(travel_time_daily_max, na.rm = TRUE)
  
  ## Scale the density by the average daily max density in the link
  density_scaled = density/average_max_density
  travel_time_scaled = travel_time/average_max_traveltime
  
  ## Create background container for link and fill it with the items below the threshold
  background <- rep(median(travel_time, na.rm = TRUE),length(travel_time))
  background[density_scaled < th_high2] <- travel_time[density_scaled < th_high2]
  
  background_tt2<- rep(median(travel_time, na.rm = TRUE),length(travel_time))
  background_tt2[travel_time_scaled < th_high2] <- travel_time[travel_time_scaled < th_high2]
  if (fill_gaps == TRUE) {
    background_tt2[travel_time_scaled > th_high2] <- average_max_traveltime*th_high2
  }
  ## Create spikes and spike flag container for link and fill it with the items above the threshold
  spikes <- rep(0,length(travel_time))
  spike_flag <- rep(0,length(travel_time))
  spikes_tt2 <- rep(0,length(travel_time))
  spike_flag_tt2 <- rep(0,length(travel_time))
  
  spike_flag[density_scaled > th_high2] <- 1
  spike_flag_tt2[travel_time_scaled > th_high2] <- 1
  if (fill_gaps == TRUE){
    spikes_tt2[travel_time_scaled > th_high2] <- travel_time[travel_time_scaled > th_high2] - average_max_traveltime*th_high2
    spikes[density_scaled > th_high2] <- travel_time[density_scaled > th_high2] - average_max_traveltime*th_high2
  } else {
    spikes_tt2[travel_time_scaled > th_high2] <- travel_time[travel_time_scaled > th_high2] - median(travel_time)
    spikes[density_scaled > th_high2] <- travel_time[density_scaled > th_high2] - median(travel_time)
  }
  
  ## Write background and spikes to dataframe
  m_data_selected$background_dens[m_data_selected$link_id == link] <- background
  m_data_selected$spikes_dens[m_data_selected$link_id == link] <- spikes
  m_data_selected$spike_flag_dens[m_data_selected$link_id == link] <- spike_flag
  m_data_selected$background_tt2[m_data_selected$link_id == link] <- background_tt2
  m_data_selected$spikes_tt2[m_data_selected$link_id == link] <- spikes_tt2
  m_data_selected$spike_flag_tt2[m_data_selected$link_id == link] <- spike_flag_tt2
  ## Increment counter
  counter_link <- counter_link + 1
}

## --- Save m_data_selected and links_list_df in RData file --------------------
file_name_3 <- paste('../00_Data/04_Processed_data/',mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_',th_high2 ,'.RData',sep="")
save(m_data_selected,links_list_df, file = file_name_3)
print("Finished.")