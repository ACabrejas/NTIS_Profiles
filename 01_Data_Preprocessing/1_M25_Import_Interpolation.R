## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console


## Choose motorway
mX <- "m25" 

##################################################################################
#   Discard Links with 10% or more missing data in speed, travel_time or flow    #
##################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
complete_data = 0:76
file_name = paste('link_data_', complete_data, '.csv', sep = '')
df = data.frame()

for (i in 1:77) {
  print(paste('Reading M25 data. Progress: ',round(100*i/77,2) ,'%'))
  df = rbind(df, read.csv(paste('../00_Data/01_Raw_data/M25_link_data/', file_name[i], sep = '')))
}
test = list.files(path =  '../00_Data/01_Raw_data/M25_link_data/',pattern="*.csv")
df = rbind(read.csv(test))


link_list = unique(df$link_id)
count = 1
remove_links = matrix(0, nrow = length(link_list))
for (link in link_list) {
  
  travel_time = df$travel_time[df$link_id == link]
  flow = df$flow[df$link_id == link]
  speed = df$speed[df$link_id == link]
  
  temp_tt = sum(is.na(travel_time))
  #temp_tt
  temp_flow = sum(is.na(flow))
  #temp_flow
  temp_speed = sum(is.na(speed))
  #temp_speed
  
  if (temp_tt > 0.1*length(travel_time) | temp_flow > 0.1*length(flow) | temp_speed > 0.1*length(speed)){
    remove_links[count] = 1
  }
  count = count + 1
}
complete_data = complete_data[remove_links == 0]

## INITIAL DATA HANDLING ################################################################
# Load motorway data into dataframe


rm(list= ls()[!(ls() %in% c('complete_data','mX'))])

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file_name = paste('link_data_', complete_data, '.csv', sep = '')
df = data.frame()

for (i in 1:length(complete_data)) {
  print(paste('Reading M25 data. Progress: ',round(100*i/length(complete_data),2) ,'%'))
  df = rbind(df, read.csv(paste('../00_Data/01_Raw_data/M25_link_data/', file_name[i], sep = '')))
}



links_list_df = data.frame(unique(df$link_id))
m_data_interp = df

file_name2 <- paste('../00_Data/01_Raw_data/',mX,'_data.RData',sep="")
save(m_data_interp,links_list_df, file = file_name2)
file_name5 <- paste('../00_Data/01_Raw_data/',mX,'_data.csv',sep="")
write.csv(m_data_interp, file = file_name5, col.names=TRUE)

m_data_interp$time_zone_info = NULL
m_data_interp$interpolated_flow = NULL
m_data_interp$interpolated_concentration = NULL
m_data_interp$interpolated_speed = NULL
m_data_interp$interpolated_headway = NULL
m_data_interp$interpolated_travel_time = NULL
m_data_interp$interpolated_profile_time = NULL
m_data_interp$smoothed_interpolated_concentration = NULL
m_data_interp$smoothed_interpolated_flow = NULL
m_data_interp$smoothed_interpolated_headway = NULL
m_data_interp$smoothed_interpolated_profile_time = NULL
m_data_interp$smoothed_interpolated_speed = NULL
m_data_interp$smoothed_interpolated_travel_time = NULL
m_data_interp$interpolated_headway = NULL
m_data_interp$interpolated_concentration = NULL

m_data_interp$bla <- rep(seq(0,1439),length(m_data_interp$speed)/(1440*length(unique(m_data_interp$link_id))))

colnames(m_data_interp) = c("link_id", "adjusted_time", "m_date", "day_week", "adjusted_time2", "traffic_flow", "traffic_concentration",
                            "traffic_speed", "traffic_headway", "travel_time", "thales_profile","absolute_time")

m_data = m_data_interp

file_name3 <- paste('../00_Data/01_Raw_data/',mX,'_data.RData',sep="")
save(m_data,links_list_df, file = file_name3)

file_name4 <- paste('../00_Data/01_Raw_data/',mX,'_data.csv',sep="")
write.csv(m_data_interp, file = file_name4, col.names=TRUE)

