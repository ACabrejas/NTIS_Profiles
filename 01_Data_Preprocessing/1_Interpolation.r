#############################################################################################
#                                                                                           #
#  GOAL :  This script interpolates the data columns (4 to 10) of 'm6_data.csv' OR          #
#         'm11_data.csv' where there are 10 or less consecutive missing data (the 9999s)    #
#   IN  : mX_data.csv                                                                       #
#   OUT : mX_data_interp.RData, mX_data_interp.csv                                          #
#                                                                                           #
#   Observations: VERY SLOW (hours). It should not be run unless absolutely necessary       #
#                                                                                           #
#############################################################################################

## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

## Load libraries
library(zoo)

## Choose motorway
mX <- "m25" 
## Define maximum number of points to interpolate
p = 10

## INITIAL DATA HANDLING ################################################################
# Load motorway data into dataframe
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (mX == "m25") {
  file_name <- paste(mX,'_data.RData',sep="")
  load(paste("../00_Data/01_Raw_data/",file_name, sep = ''))
  m_data$adjusted_time2=NULL
} else {
  file_name <- paste(mX,'_data.csv',sep="")
  m_data <- read.csv(paste("../00_Data/01_Raw_data/",file_name, sep = ''))
}

# If data is M25, substitute missing values by 9999
if (mX == "m25") {
  m_data[is.na(m_data)] = -1
}
# Substitute 9999's by -1's
m_data[m_data == 9999] <- -1

# Filter some strange huge numbers (errors) that come in the data in row 4-10 (especially 4-6)
if(mX == "m25") {
  for (i in 5:10) {
    temp <- m_data[,i]
    temp[temp > 10000] <- -1
    m_data[,i] <- temp
  }
} else {
  for (i in 4:10) {
    temp <- m_data[,i]
    temp[temp > 10000] <- -1
    m_data[,i] <- temp
  }
}

# Define a new dataframe that will contain interpolated data
if (mX == "m25") {
  m_data_interp = data.frame( m_data[,1],m_data[,3], m_data[11], matrix(-1, nrow(m_data), 6) )
  column_names = c("link_id", "m_date", "absolute_time", "traffic_flow","traffic_concentration",
                   "traffic_speed","traffic_headway","travel_time", "profile_time")
  colnames(m_data_interp) = column_names
} else {
  m_data_interp = data.frame( m_data[,1:3], matrix(-1, nrow(m_data), 7), m_data[,11:13] )
  colnames(m_data_interp) <- colnames(m_data)
  
}

# Get the lists of links and dates
listLinks = unique(m_data_interp$link_id)
dates = unique(m_data_interp$m_date)

## INTERPOLATION #########################################################################

## Go through every link and date
count = 0
for (link_i in listLinks) {
  count2 = 0
  print(paste("Computing interpolation. Completion: ", 100*round((count+1) / (length(listLinks)),4),'%'))
  for (data_i in dates) {
    ## Get row indices of m_data dataframe for selected link and date
    indexLinkDate <- which(m_data$link_id == link_i & m_data$m_date == data_i)
    ## Get values of absolute time for indices
    if (mX == "m25") {
      absoluteTimeSection = m_data[indexLinkDate,11]
      cols_to_interpolate = 5:10
    } else {
      absoluteTimeSection = m_data[indexLinkDate,3]
      cols_to_interpolate = 4:10
    }
    
    
    
    ## Interpolate columns 4 to 10
    for (i in cols_to_interpolate) {
      ## Define data vectors
      sectionData = m_data[indexLinkDate,i] # Section of column i
      sampleTime = absoluteTimeSection[which(sectionData != -1)] # Absolute time vector excluding entries where data is not -1
      sampleVariable = sectionData[which(sectionData != -1)] # Section of column i where data is not -1
      
      if (length(sampleTime) > 1) {
        ## Using zoo library and rollapply function. Find sequences of p consecutive -1's (just initial points of sequence)
        indexOfSequenceInit <- which(rollapply(sectionData, p, identical, rep(-1, p)))
        ## Given the initial points: find the rest of the sequence points (edit this line if p!=10)
        indexOfSequence <- sort(unique(c(indexOfSequenceInit,indexOfSequenceInit+1,indexOfSequenceInit+2,indexOfSequenceInit+3,indexOfSequenceInit+4,indexOfSequenceInit+5,indexOfSequenceInit+6,indexOfSequenceInit+7,indexOfSequenceInit+8,indexOfSequenceInit+9)))
        ## Indices where interpolation can take place
        interpolTimeIndex <- setdiff(1:length(absoluteTimeSection), indexOfSequence)
        ## Interpolation
        interpol <- approx(sampleTime, sampleVariable, absoluteTimeSection[interpolTimeIndex], method="linear")
        if (mX == "m25") {
          m_data_interp[indexLinkDate[interpolTimeIndex],i-1] = interpol$y
        } else {
          m_data_interp[indexLinkDate[interpolTimeIndex],i] = interpol$y
        }
        
      }
    }
    count2 = count2 + 1
  }
  count = count + 1
}

## Eliminate the NA's obtained when interpolator was forced to extrapolate
m_data_interp[is.na(m_data_interp)] <- -1

## Insert weekday column (Week begins with Sunday=1, and ends with Saturday=7)
date_ymd <- m_data$m_date %% 20000000
m_data_interp$day_week <- as.POSIXlt(strptime(date_ymd, format = "%y%m%d"))$wday + 1

####### Save results ################################################################
# Write CSV file
file_name_2 <- paste('../00_Data/02_Interpolated_data/',mX,'_data_interp.csv',sep="")
write.csv(m_data_interp, file = file_name_2, row.names = FALSE, col.names=TRUE)

# Save RData file
file_name_3 <- paste('../00_Data/02_Interpolated_data/',mX,'_data_interp.RData',sep="")
save(m_data_interp, file = file_name_3)