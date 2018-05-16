#######################################################################################
##
##     Goals: Generate RData file with selected data for analysis: 
##                 * 12 consecutive weeks from March 7th to May 29th 2016
##                 * Just "Main Carriage way" links with less than 5% of -1's
##                 * -1's of travel time and thales profile replaced
##                 * Outlier (-1) flag
##
##     IN  : mX_interp.RData, mX_links.csv
##     OUT : mX_data_selected_and_links_list_A.RData
##
######################################################################################

## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

## Load libraries
library(stats)
library(graphics)

## Choose motorway
mX <- "m25" ## Choose motorway

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file_name <- paste(mX,'_data_interp.RData',sep="")
load(paste('../00_Data/02_Interpolated_data/',file_name, sep = ''))
file_name_2 <- paste(mX,'_links.csv',sep="")
m_links <- read.csv(paste('../00_Data/01_Raw_data/',file_name_2,sep = ''))

## --- Get motorway relevant links lists ----------------------------------------------------------



if (mX != "m25") {
  ## Get ordered list of all links in the motorway from "m_data_interp" dataframe that was loaded
  links_list_all <- unique(m_data_interp$link_id)
  
  ## Get disordered list of "Main Carriage Way" links from CSV file
  links_list_MC_disordered <- m_links$link_id[m_links$link_type == " mainCarriageway"] ## Consider just main carriageways
  
  ## Get list of non-"Main Carriage Way" links (which will be erased)
  links_erase_1 <- m_links$link_id[m_links$link_type != " mainCarriageway"]
} else {
  links_list_all <- unique(m_data_interp$link_id)
  links_list_MC_disordered = unique(m_data_interp$link_id)
}


## --- Take out links with more than 10% of outliers (-1's) ------------------

## Make histogram of outliers
# outliers_abs_time <- m_data_interp$absolute_time[m_data_interp$travel_time == -1]
# breaks_hist <- seq(0,1500,50)
# png(filename = paste("output/outliers_abs_time_histogram.png",sep=""))
# hist(outliers_abs_time, breaks = breaks_hist, freq = TRUE)
# dev.off()

## Make temporal dataframe with time filtered from start to end (hours)
start <- 0 ## No filter if start = 0 and end = 24
end <- 24 ## No filter if start = 0 and end = 24
m_data_interp_filtered <- m_data_interp[ m_data_interp$absolute_time >= (start*60) & m_data_interp$absolute_time < (end*60), ]

## Make list of links with more than 5% of outliers (-1's) which will be erased
links_erase_2 <- numeric()
percent_outliers <- rep(0,length(links_list_MC_disordered))
counter_link <- 1
for (i in links_list_MC_disordered) {
  travel_time <- m_data_interp_filtered$travel_time[m_data_interp_filtered$link_id == i]
  percent_outliers[counter_link] <- length(travel_time[travel_time == -1]) / length(travel_time)
  if (percent_outliers[counter_link] >= 0.1) {
    links_erase_2 <- c(links_erase_2,i)
  }
  counter_link <- counter_link + 1
}

## Make list of links to erase: concatenate non-Main-Carriage-Way links with links having too much outliers
if (mX != "m25") {
  links_erase <- c(links_erase_1,links_erase_2)
} else {
  links_erase <- c(links_erase_2)
}


## --- Get final list of links -------------------------------------------------

## Make final list of links (disordered)
links_list_disordered <- setdiff(links_list_all,links_erase)

## Sort the list to match order in dataframe
links_list <- numeric() # Will store ordered list of "Main Carriage Way" links
for (i in links_list_all) {
  if (i %in% links_list_disordered) {
    links_list <- c(links_list,i)
  }
}

## --- Make new dataframe for selected data ------------------------------------

## Define new dataframe with selected columns, for selected dates (12 weeks: March 7th to May 29th) and for selected links (main carriage ways)
if (mX != "m25") {
  m_data_selected <- m_data_interp[,c(1,2,14,3,4,6,5,8,9,11,13)] ## Choose columns of interest in new order
  column_names <- colnames(m_data_selected)
  column_names[6] <- "thales_profile"
  colnames(m_data_selected) <- column_names
} else {
  m_data_selected <- m_data_interp
}

for (i in links_erase) {
  m_data_selected <- m_data_selected[m_data_selected$link_id != i,]
}

if (mX != "m25") {
  m_data_selected <- m_data_selected[ m_data_selected$m_date >= 20160307 & m_data_selected$m_date <= 20160529, ]
} else {
  m_data_selected$travel_time[is.na(m_data_selected$travel_time)] <- -1
}
m_data_selected$outlier_flag <- rep(0,nrow(m_data_selected))
m_data_selected$outlier_flag[m_data_selected$travel_time == -1] <- 1

## Replace the -1's of travel time with STL trend + seasonal components via a refinement process, and -1's in thales profile by median of the link
count = 1
for (link in links_list) {
  print(paste("Interpolating Travel Times. Link ", count,"/",length(links_list),". Completion:", round(100*count/length(links_list),2),"%"))
  ## Load data
  travel_time <- m_data_selected$travel_time[m_data_selected$link_id == link]
  
  ## Create travel time temp vector with -1's substituted by median of the link
  travel_time_temp <- travel_time
  travel_time_temp[travel_time == -1] <- median(travel_time[travel_time != -1]) ## Replace -1's by median
  
  ## STL of travel time temp vector for link i
  step <- 1
  signal <- travel_time_temp
  signal.ts <- ts(signal, frequency = 7*(1440/step))
  signal.stl = stl(signal.ts, s.window="periodic")
  trend <- as.vector(signal.stl$time.series[,"trend"])
  seasonal <- as.vector(signal.stl$time.series[,"seasonal"])
  remainder <- as.vector(signal.stl$time.series[,"remainder"])
  STL_profile <- as.vector(trend + seasonal)
  
  ## Replace -1's of travel time by STL trend + seasonal components (refinement process)
  travel_time_ref <- travel_time
  travel_time_ref[travel_time == -1] <- STL_profile[travel_time == -1] ## Replace -1's
  
  ## Update travel time in dataframe
  m_data_selected$travel_time[m_data_selected$link_id == link] <- travel_time_ref 

  ## Replace -1's of thales profile by median of thales profile of the link
  thales_profile <- m_data_selected$thales_profile[m_data_selected$link_id == link]
  thales_profile[thales_profile == -1] <- median(thales_profile[thales_profile != -1])
  m_data_selected$thales_profile[m_data_selected$link_id == link] <- thales_profile ## Update Thales profile
  count = count + 1
}

## Make plot to visualize substitution using m6 link 3
# time <- 1:2000
# plot(time,travel_time[61801:63800],col='black',type='l',xlab='time (s)',ylab='travel time (s)')
# lines(time,travel_time_temp[61801:63800],col='red')
# lines(time, travel_time_ref[61801:63800],col='green')


## Create dataframe for "Main Carriage Way" links with less than 5% outliers
if (mX != "m25") {
  for (i in links_erase) {
    m_links <- m_links[m_links$link_id != i,]
  }
  links_list_df <- m_links
  j <- 1
  for (i in links_list) {
    links_list_df[j,] <- m_links[m_links$link_id == i,]
    j <- j + 1
  }
} else {
  link_id = links_list
  links_list_df <- data.frame(link_id)
  
  link_length = matrix(0,length(links_list_df$link_id),1)
  count = 1
  for (link  in links_list_df$link_id) {
    link_length[count] = m_links$link_length[m_links$link_id == link]
    count = count + 1
  }
  links_list_df$link_length = link_length
}

rownames(links_list_df) <- 1:(length(links_list))


## --- Save m_data_selected and links_list_df in RData file --------------------
file_name_3 <- paste('../00_Data/03_Clean_data/',mX,'_data_selected_and_links_list_A.RData',sep="")
save(m_data_selected,links_list_df, file = file_name_3)
