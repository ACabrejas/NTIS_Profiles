## --- Program description -------------------------------------------------------------
##
##     Goal: Repetition of RSG: Thales Project in R
##
## --- Initialize program ------------------------------------------------------------

## Clear-all
rm(list = ls()) # Clear variables
graphics.off() # Clear plots
cat("\014") # Clear console

## Load libraries
library(stats)
library(graphics)
library(chron)
library(gdata)

# Operation options
import_old_csv = FALSE
use_old_data = TRUE
hybrid_mode = FALSE
fit_old_to_new = FALSE

# Traffic Jam Filtering
min_duration <- 20                  # Min duration of deviation to be considered jam by filter
max_duration <- 600                 # Max duration of deviation to be considered jam by filter
jam_threshold <- 300                # Min deviation from profile to be considered jam by filter
min_prediction <- 20                # Minimum predicted duration

# Set Motorway
motorway <- 'm6'

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Import old data and exclude some dates:
if (import_old_csv == TRUE) {
  data = read.csv(paste('../00_Data/01_Interpolated_data/', motorway,"_data.csv", sep = ''), header = FALSE)
  data2 = read.csv('./m6m11TrainingDates.csv', header = FALSE)
  
  data <- data[data$V2 < 20160500,] # March and April
  data <- data[data$V2 != 20160325,] # Exclude Bank Holyday
  data <- data[data$V2 != 20160328,] # Exclude Bank Holyday
  data <- data[data$V2 != 20160502,] # Exclude Bank Holyday
  data <- data[data$V2 != 20160530,] # Exclude Bank Holyday
  data <- data[data$V14 == 2 | data$V14 == 3 | data$V14 == 4 | data$V14 == 5 | data$V14 == 6,] # Weekdays
  
  link_id <- data$V1
  m_date <- data$V2
  day_week <- data$V14
  absolute_time <- data$V3
  travel_time <- data$V4
  thales_profile <- data$V6
  free_flow = data$V5
  traffic_speed <- data$V8
  traffic_flow <- data$V9
  congestion_event <- data$V11
  other_event <- data$V11
  
  dates = data2$V1
  permutatedindex = data2$V2
  
  # Create dataframes with new format
  m_data_selected_old = data.frame(link_id, m_date, day_week, absolute_time, travel_time,
                                   thales_profile, free_flow, traffic_speed, traffic_flow,
                                   congestion_event, other_event)
  m6m11TrainingDates = data.frame(dates,permutatedindex)
  # Save it for future use
  save(m_data_selected_old, file = paste("../00_Data/01_Interpolated_data/",motorway,"_FinalData_Interp_RSG.Rdata", sep = ''))
  save(m6m11TrainingDates, file = paste(getwd(),"/m6m11TrainingDates.Rdata", sep = ''))
  
  # Delete old container
  rm(data, data2)
  
} else if (use_old_data == TRUE) {
  # Load Previously generated data
  load(paste(getwd(),"/",motorway,"_FinalData_Interp_RSG.Rdata", sep = ''))
  
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$m_date < 20160500,] # March and April
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$m_date != 20160325,] # Exclude Bank Holyday
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$m_date != 20160328,] # Exclude Bank Holyday
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$m_date != 20160502,] # Exclude Bank Holyday
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$m_date != 20160530,] # Exclude Bank Holyday
  
  m_data_selected_old <- m_data_selected_old[m_data_selected_old$day_week == 2 | m_data_selected_old$day_week == 3 | 
                                               m_data_selected_old$day_week == 4 | m_data_selected_old$day_week == 5 | 
                                               m_data_selected_old$day_week == 6,]
  
  link_id <- m_data_selected_old$link_id
  m_date <- m_data_selected_old$m_date
  day_week <- m_data_selected_old$day_week
  absolute_time <- m_data_selected_old$absolute_time
  travel_time <- m_data_selected_old$travel_time
  thales_profile <- m_data_selected_old$thales_profile
  free_flow <- m_data_selected_old$free_flow
  traffic_speed <- m_data_selected_old$traffic_speed
  traffic_flow <- m_data_selected_old$traffic_flow
  congestion_event <- m_data_selected_old$congestion_event
  other_event <- m_data_selected_old$other_event
  
  load(paste(getwd(),"/m6m11TrainingDates.Rdata", sep = ''))
  dates = m6m11TrainingDates$dates
  permutatedindex = m6m11TrainingDates$permutatedindex
  
} else if (hybrid_mode == TRUE) {
  m_data_selected <- m_data_selected[m_data_selected$m_date < 20160500,] # March and April
  m_data_selected <- m_data_selected[m_data_selected$m_date != 20160325,] # Exclude Bank Holyday
  m_data_selected <- m_data_selected[m_data_selected$m_date != 20160328,] # Exclude Bank Holyday
  m_data_selected <- m_data_selected[m_data_selected$m_date != 20160502,] # Exclude Bank Holyday
  m_data_selected <- m_data_selected[m_data_selected$m_date != 20160530,] # Exclude Bank Holyday
  
  m_data_selected <- m_data_selected[m_data_selected$day_week == 2 | m_data_selected$day_week == 3 | 
                                       m_data_selected$day_week == 4 | m_data_selected$day_week == 5 | 
                                       m_data_selected$day_week == 6,]
  
  link_id <- m_data_selected$link_id
  m_date <- m_data_selected$m_date
  day_week <- m_data_selected$day_week
  absolute_time <- m_data_selected$absolute_time
  travel_time <- m_data_selected$travel_time
  thales_profile <- m_data_selected$hybrid_profile
  free_flow <- m_data_selected$free_flow
  traffic_speed <- m_data_selected$traffic_speed
  traffic_flow <- m_data_selected$traffic_flow
  congestion_event <- m_data_selected$congestion_event
  other_event <- m_data_selected$other_event
}

# Calculate traffic intensity & density
traffic_intensity <- travel_time - thales_profile - jam_threshold
traffic_density <- rep(0,10000)
traffic_density[traffic_flow>0 & traffic_speed>0] <- traffic_flow[traffic_flow>0 & traffic_speed>0] / traffic_speed[traffic_flow>0 & traffic_speed>0]
traffic_density[traffic_flow<0 | traffic_speed<0] = -1

# Build traffic jam alert vector and filling it
traffic_jam_alert <- rep(0,length(traffic_intensity))

# Jam alerts with NO other event and between 5am-11pm
traffic_jam_alert[traffic_intensity > 0 & other_event == 0 & absolute_time >= 300 & absolute_time < 1380] = 1
traffic_jam_alert[traffic_intensity > 0] = 1


# Finding the real traffic jams using 5 minute criteria
regime <- rep(0,length(traffic_intensity))    # regime = 0 is normal regime, regime = 1 is congestion regime
counter_0 <- 0                                # Count number of consecutive zeros in traffic jam alert vector
counter_1 <- 0                                # Count number of consecutive ones in traffic jam alert vector
jam_flag <- 0

for (i in 1:length(traffic_intensity)) {
  # Find first 5 consecutive points where traffic_jam_alert is 1
  if (traffic_jam_alert[i] == 0) {
    counter_0 <- counter_0 + 1
    counter_1 <- 0
  }
  if (traffic_jam_alert[i] == 1) {
    counter_0 <- 0
    counter_1 <- counter_1 + 1
  }
  if (counter_1 == 6) {
    jam_flag <- 1
    regime[i-5] <- 1
    regime[i-4] <- 1
    regime[i-3] <- 1
    regime[i-2] <- 1
    regime[i-1] <- 1
    regime[i-0] <- 1
  }
  if (jam_flag == 1) {
    regime[i] <- 1
  }
  # Then, find first 5 consecutive points where traffic_jam_alert is 0
  if (counter_0 == 6 & jam_flag == 1) {
    jam_flag <- 0
    regime[i-5] <- 0
    regime[i-4] <- 0
    regime[i-3] <- 0
    regime[i-2] <- 0
    regime[i-1] <- 0
    regime[i-0] <- 0
  }
}  

# Separate traffic jams and calculate jam number
jam_counter <- 0 # count number of jams
jam_number <- rep(0,length(traffic_intensity)) # similar to regime vector. It has the same zeros as regime vector. When regime vector is equal to one, this vector instead has the jam number
for (i in 1:length(traffic_intensity)) {
  if (i==1 & regime[i]==1) {
    jam_counter = jam_counter + 1
  }
  if (i>=2) {
    if (regime[i]==1 & regime[i-1]==0) {
      jam_counter = jam_counter + 1
    }
  }
  if (regime[i] == 1) {
    jam_number[i] = jam_counter
  }
}
# ----Create a matrix of traffic jams. Each row is a different traffic jam
start <- -1
end <- -1
len <- -1
traffic_intensity_matrix <- matrix(0, jam_counter, 1440)
jam_number_matrix <- matrix(0, jam_counter, 1440)
traffic_density_matrix <- matrix(0, jam_counter, 1440)
if(jam_counter >= 1) {
  for (j in 1:jam_counter) {
    print(paste("Running through jams...",round(100*j/jam_counter,2), "% completed", sep = ""))
    # finding start and end of each traffic jam using the jam_number vector
    for (i in 1:length(traffic_intensity)) {
      if (jam_number[i] == j) {
        end <- i
      }
    }
    for (i in length(traffic_intensity):1) {
      if ( jam_number[i] == j ) {
        start <- i
      }
    }
    len <- end - start + 1
    # Writing the traffic jam in the matrix
    if(end >= start) {
      traffic_intensity_matrix[j,1:len] <- traffic_intensity[start:end]
      jam_number_matrix[j,1:len] <- jam_number[start:end]
      traffic_density_matrix[j,1:len] <- traffic_density[start:end]
    }
    # Low pass filter
    if(len>=6) {
      for(k in 6:len) {
        c1 <- 0.5*traffic_intensity_matrix[j,k]
        c2 <- 0.25*traffic_intensity_matrix[j,k-1]
        c3 <- 0.125*traffic_intensity_matrix[j,k-2]
        c4 <- 0.0625*traffic_intensity_matrix[j,k-3]
        c5 <- 0.0625*traffic_intensity_matrix[j,k-4]
        traffic_intensity_matrix[j,k] <- c1 + c2 + c3 + c4 +c5
      }
    }
  }
}

# Filtering traffic jams by min-max duration and intensity (jam_threshold)
delete_rows <- vector('numeric')
for (j in 1:jam_counter) {
  row <- traffic_intensity_matrix[j,]
  row <- row[ row != 0 ]
  if( length(row) < min_duration | length(row)>max_duration | max(row)<0 ) {
    delete_rows <- c(delete_rows,j)
  }
}
traffic_intensity_matrix <- traffic_intensity_matrix[-delete_rows,]
jam_number_matrix <- jam_number_matrix[-delete_rows,]
traffic_density_matrix <- traffic_density_matrix[-delete_rows,]
jam_counter <- length(traffic_intensity_matrix[,1])


##############################################
# ----Summary statistics per traffic jam---- #
##############################################

# Un-normalized statistics vector initialization
duration <- rep(0,jam_counter) # Vector of duration of traffic jams
maximum_intensity <- rep(0,jam_counter) # Vector of maximum intensity of traffic jams
slope_to_max <- rep(0,jam_counter) # slope to global maximum
jam_size <- rep(0,jam_counter) # Vector of sizes of traffic jams
auto_corr <- rep(0,60) # Correlate values as far as 60 minutes
# Normalized statistics vector initialization
location_of_max <- rep(0,jam_counter) # Vector of normalized locations of maximums of traffic jams
symmetry_factor <- rep(0,jam_counter) # Vector of (1-loc_max)/loc_max
skewness <- rep(0,jam_counter) # Vector of Skewness of traffic jams
trapezoid_a <- rep(0,jam_counter) # Vector of "a" values of trapezoid of traffic jams
trapezoid_b <- rep(0,jam_counter) # Vector of "b" values of trapezoid of traffic jams
trapezoid_c <- rep(0,jam_counter) # Vector of "c" values of trapezoid of traffic jams
# Mixed statistics
ratio_sym_slo <- rep(0,jam_counter) # Ration of symmetry factor / slope to global maximum
# Temporal variables
max <- 0
slo <- 0
loc <- 0
size <- 0
h <- 0 # Used in mean calculation
skew <- 0
plateau_threshold <- 0.8 # The plateau region are the minutes for which the traffic jam intensity lies from this threshold to 100% (maximum)
a <- 0 # Used for finding "a"
b <- 0 # Used for finding "b"
c <- 0 # Used for finding "c"
counter_t <- 0 # Count traffic jams with intensity variance greater than zero
max_auto <- 0
# Run through every jam
for (i in 1:jam_counter) {
  jam_i = traffic_intensity_matrix[i,]
  jam_i = jam_i[ jam_i != 0] # Remove zeros of vector

  # Find maximum intensity, slope, maximum location and jam size
  max <- 0
  size <- 0
  for (k in 1:length(jam_i)) {
    if (jam_i[k] >= max) {
      max <- jam_i[k]
      slo <- max/k
      loc <- k
    }
    size <- size + jam_i[k]
  }
  # Find skewness
  h <- 0
  for (k in 1:length(jam_i)) { # mean
    h <- h + (jam_i[k]/size)*k
  }
  mean <- h
  h <- 0
  for (k in 1:length(jam_i)) { # standard deviation
    h <- h + (jam_i[k]/size)*((k-mean)^2)
  }
  sd <- sqrt(h)
  h <- 0
  for (k in 1:length(jam_i)) { # skewness
    h <- h + (jam_i[k]/size)*((k-mean)/sd)^3
  }
  skew <- h
  # Find "a" and "b" values of the trapezoid
  for (k in length(jam_i):1) {
    if (jam_i[k] >= plateau_threshold*max) { a <- k }
  }
  for (k in 1:length(jam_i)) {
    if (jam_i[k] >= plateau_threshold*max) { b <- k }
  }
  b <- b - a
  c <- length(jam_i) - a - b
  # Auto-correlation
  zeros <- rep(0,59)
  xt <- c(jam_i,zeros)
  if (var(jam_i) > 0) {
    counter_t <- counter_t + 1
    for (tao in 0:59) {
      h <- 0
      max_auto <- length(jam_i) - tao
      if (max_auto >= 1) {
        for (k in 1:max_auto) {
          h <- h + (xt[k]-mean(jam_i))*(xt[k+tao]-mean(jam_i))
        }
        h <- h/(length(jam_i)*var(jam_i))
        auto_corr[tao+1] <- auto_corr[tao+1] + h
      }
    }
  }
  # Un-normalized statistics (except auto-correlation)
  duration[i] <- length(jam_i)
  maximum_intensity[i] <- max
  slope_to_max[i] <- slo
  jam_size[i] <- size
  # Normalized statistics
  location_of_max[i] <- loc/length(jam_i)
  if(location_of_max[i] > 0.98) {
    location_of_max[i] <- runif(1, min = 0, max = 1)
  }
  symmetry_factor[i] <- (1-location_of_max[i])/location_of_max[i]
  skewness[i] <- skew
  trapezoid_a[i] <- a/length(jam_i)
  trapezoid_b[i] <- b/length(jam_i)
  trapezoid_c[i] <- c/length(jam_i)
  # Mixed
  ratio_sym_slo[i] <- symmetry_factor[i]/slope_to_max[i]
}
# Auto-correlation
for (j in 1:60) {
  auto_corr[j] <- auto_corr[j]/counter_t # Averaging over autocorrelations of each traffic jam
}


##########################---------------------------
#
#
##########################-------------------------
# Needed variables

forecast_matrix_null <- matrix(0, jam_counter, 1440)
forecast_matrix_thales <- matrix(0, jam_counter, 1440)
forecast_matrix_midpoint <- matrix(0, jam_counter, 1440)
forecast_matrix_linear <- matrix(0, jam_counter, 1440)
forecast_matrix_dyntrap <- matrix(0, jam_counter, 1440)

jam_time <- rep(0, jam_counter) # real duration of each traffic jam
jam_forecasted_time <- rep(0, jam_counter) #forecasted duration of each traffic jam by Thales algorithm
crit_slope <- 0.577 # 0.577 is 30 degrees slope

for (j in 1: jam_counter) {
  print(paste("Running predictions...",round(100*j/jam_counter,2), "% completed", sep = ""))
  
  Maximum_t <- 0
  prediction <- 0 # Absolute time of congestion final clearing
  k <- 1
  a <- 0
  
  while( jam_number_matrix[j,k] > 0 ) {
    
    # Null Model
    if(k > 5) {
      #prediction = median(duration)
      prediction = mean(duration)
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_null[j,k] = prediction
    }
    else {
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_null[j,k] = prediction
    }
    
    # Existing Algorithm (THALES)
    if ( traffic_intensity_matrix[j,k] > Maximum_t & k>5) {
      Maximum_t = traffic_intensity_matrix[j,k]
      prediction = 2 * k # Thales symmetric assumption
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_thales[j,k] = prediction
    }
    else {
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_thales[j,k] = prediction
    }
    
    # Midpoint prediction (x2)
    if(k > 5) {
      prediction = 2 * k # Thales symmetric assumption
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_midpoint[j,k] = prediction
    }
    else {
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_midpoint[j,k] = prediction
    }
    
    # Linear Regression
    if(k >= 11) {
      mu <- mean(traffic_intensity_matrix[j,(k-10):k])
      slop <- (traffic_intensity_matrix[j,k] -  mu) / 5
      prediction = k - (traffic_intensity_matrix[j,k] / slop)
      forecast_matrix_linear[j,k] = prediction
    }
    else {
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_linear[j,k] = prediction
    }
    
    # Dynamic Trapezoid
    if (k > 5) {
      if ( traffic_intensity_matrix[j,k] > Maximum_t) {
        Maximum_t = traffic_intensity_matrix[j,k]
      }
      for (m in k:1) {
        if (traffic_intensity_matrix[j,m] >= 0.8 * Maximum_t) {
          a <- m
        }
      }
      b = k - a
      prediction = (2*a) + b
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_dyntrap[j,k] = prediction
    }
    else {
      if(prediction < min_prediction) { prediction <- min_prediction }
      forecast_matrix_dyntrap[j,k] = prediction
    }
    
    # Weighted Multimodel
    
    # Increment k (minute of the jam) by 1
    k <- k + 1
  }
  jam_time[j] <- k - 1
  # Government criterion
  middle <- (k-1)/2
  jam_forecasted_time[j] <- forecast_matrix_thales[j,middle]
}
  
  
maxmax<-max(forecast_matrix_dyntrap,forecast_matrix_midpoint,forecast_matrix_null,forecast_matrix_thales)
plot(forecast_matrix_midpoint[8,1:maxmax], type = 'l')
max(forecast_matrix_linear)

num_bins = 50

final_errors_null = matrix(0,num_bins,1)
final_errors_thales = matrix(0,num_bins,1)
final_errors_midpoint = matrix(0,num_bins,1)
final_errors_linear = matrix(0,num_bins,1)
final_errors_dyntrap = matrix(0,num_bins,1)

count_bins = matrix(0,num_bins,1)
test = TRUE

for (i in 1:jam_counter){
  if (test == TRUE) {
    selected_bin = ceiling(num_bins * (6:duration[i])/duration[i])
    
    vec_prediction_null = forecast_matrix_null[i, 6:duration[i]]
    vec_prediction_thales = forecast_matrix_thales[i, 6:duration[i]]
    vec_prediction_midpoint = forecast_matrix_midpoint[i, 6:duration[i]]
    vec_prediction_linear = forecast_matrix_linear[i, 6:duration[i]]
    vec_prediction_dyntrap = forecast_matrix_dyntrap[i, 6:duration[i]]
    
  } else {
    selected_bin = ceiling(num_bins * (1:duration[i])/duration[i])
    
    vec_prediction_null = forecast_matrix_null[i, 1:duration[i]]
    vec_prediction_thales = forecast_matrix_thales[i, 1:duration[i]]
    vec_prediction_midpoint = forecast_matrix_midpoint[i, 1:duration[i]]
    vec_prediction_linear = forecast_matrix_linear[i, 1:duration[i]]
    vec_prediction_dyntrap = forecast_matrix_dyntrap[i, 1:duration[i]]
    
  }
  
  prediction_error_raw_null = abs(vec_prediction_null - duration[i]) / duration[i]
  prediction_error_raw_thales = abs(vec_prediction_thales - duration[i]) / duration[i]
  prediction_error_raw_midpoint = abs(vec_prediction_midpoint - duration[i]) / duration[i]
  prediction_error_raw_linear = abs(vec_prediction_linear - duration[i]) / duration[i]
  prediction_error_raw_dyntrap = abs(vec_prediction_dyntrap - duration[i]) / duration[i]
  
  #prediction_error_raw_null[i,1:(duration[i])] = abs(forecast_matrix_null[i,1:(duration[i])] - duration[i])/duration[i]
  #prediction_error_raw_thales[i,1:(duration[i])] = abs(forecast_matrix_thales[i,1:(duration[i])] - duration[i])/duration[i]
  #prediction_error_raw_midpoint[i,1:(duration[i])] = abs(forecast_matrix_midpoint[i,1:(duration[i])] - duration[i])/duration[i]
  #prediction_error_raw_linear[i,1:(duration[i])] = abs(forecast_matrix_linear[i,1:(duration[i])] - duration[i])/duration[i]
  #prediction_error_raw_dyntrap[i,1:(duration[i])] = abs(forecast_matrix_dyntrap[i,1:(duration[i])] - duration[i])/duration[i]
  
  
  for (k in 1:length(selected_bin)) {
    count_bins[selected_bin[k]] = count_bins[selected_bin[k]] + 1
    final_errors_null[selected_bin[k]] = final_errors_null[selected_bin[k]] + prediction_error_raw_null[k]
    final_errors_thales[selected_bin[k]] = final_errors_thales[selected_bin[k]] + prediction_error_raw_thales[k]
    final_errors_midpoint[selected_bin[k]] = final_errors_midpoint[selected_bin[k]] + prediction_error_raw_midpoint[k]
    final_errors_linear[selected_bin[k]] = final_errors_linear[selected_bin[k]] + prediction_error_raw_linear[k]
    final_errors_dyntrap[selected_bin[k]] = final_errors_dyntrap[selected_bin[k]] + prediction_error_raw_dyntrap[k]
  }
}

final_errors_null = final_errors_null / count_bins
final_errors_thales = final_errors_thales / count_bins
final_errors_midpoint = final_errors_midpoint / count_bins
final_errors_linear = final_errors_linear / count_bins
final_errors_dyntrap = final_errors_dyntrap / count_bins

plot(final_errors_null, type = 'l', ylim = c(0,1),xlab = "% of traffic jam", ylab = "Prediction Relative Error",main="Threshold = 5 minutes, long jams",xaxt='n' )
axis_ticks = seq(0,50,5)
custom_label = seq(0,100,10)
axis(1, at=axis_ticks, labels = custom_label)
lines(final_errors_thales, col='blue')
lines(final_errors_midpoint, col='red')
#lines(final_errors_linear, col='yellow')
lines(final_errors_dyntrap, col='green')
legend(1,.4, legend=c("Null Hybrid","Published Hybrid","Midpoint", "DynTrap Hybrid", "Null RSG",
                      "Thales RSG", "DynTrap RSG"),col=c('black','blue', 'red', 'green', 'orange',
                                                         'purple','brown'),lty=c(1,1,1,1),cex=0.7, pch=1, pt.cex = 1)
lines(RSG_Plot$final_errors_null_RSG, col="orange")
lines(RSG_Plot$final_errors_thales_RSG, col= 'purple')
lines(RSG_Plot$final_errors_dyntrap_RSG, col="brown")

final_errors_null_HYB = final_errors_null
final_errors_thales_HYB = final_errors_thales
final_errors_midpoint_HYB = final_errors_midpoint
final_errors_dyntrap_HYB = final_errors_dyntrap

final_errors_null_RSG = final_errors_null
final_errors_thales_RSG = final_errors_thales
final_errors_midpoint_RSG = final_errors_midpoint
final_errors_dyntrap_RSG = final_errors_dyntrap

RSG_Plot = data.frame(final_errors_null_RSG,final_errors_thales_RSG,final_errors_midpoint_RSG,final_errors_dyntrap_RSG)
save(RSG_Plot, file = "PLOTS_COLM.RData")
######################################################


# ----Un-normalized statistics
# Duration
hist(duration, breaks = 30,xlab = "Traffic jam duration (minutes)", ylab = "Frequency")
hist.data = hist(duration, plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "Traffic jam duration (minutes)", ylab = "log10(frequency)")
# Maximum intensity
hist(maximum_intensity, breaks = 30, xlab = "Traffic jam maximum intensity", ylab = "Frequency")
hist.data = hist(maximum_intensity, plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "Traffic jam maximum intensity", ylab = "log10(frequency)")
# Slope to global maximum
hist(log10(slope_to_max), breaks = 80, xlab = "Traffic jam slope to max", ylab = "Frequency")
# Size
hist(jam_size, breaks = 20, xlab = "Traffic jam size", ylab = "Frequency")
hist.data = hist(log10(jam_size), plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "log10(traffic jam size)", ylab = "log10(frequency)")
# Auto-correlation function
time_auto_corr <- seq(1,60,1)
plot(time_auto_corr,auto_corr,  xlab = "Time delay (minutes)", ylab = "Auto correlation")

# ----Normalized statistics
# Location of the max
hist(location_of_max, xlab = "Location of the maximum", ylab = "Frequency")
boxplot(location_of_max, horizontal = TRUE)
title("Boxplot of location of the maximum")
mtext(side = 1, "Location of the maximum", line = 3)
# Symmetry factor
hist(log10(symmetry_factor), breaks=80, xlab = "Symmetry factor", ylab = "Frequency")
boxplot(symmetry_factor, horizontal = TRUE)
title("Boxplot of symmetry factor")
mtext(side = 1, "Symmetry factor", line = 3)
# Skewness
hist(skewness, breaks=80, xlim = range(-2,2), xlab = "Skewness", ylab = "Frequency")
boxplot(skewness, horizontal = TRUE, outline=FALSE)
title("Boxplot of skewness")
mtext(side = 1, "Skewness", line = 3)
# Trapezoid "a" parameter
hist(trapezoid_a, xlab = "Parameter a", ylab = "Frequency")
hist.data = hist(trapezoid_a, plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "Parameter a", ylab = "log10(frequency)")
# Trapezoid "b" parameter
hist(trapezoid_b, xlab = "Parameter b", ylab = "Frequency")
hist.data = hist(trapezoid_b, plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "Parameter b", ylab = "log10(frequency)")
# Trapezoid "c" parameter
hist(trapezoid_c, xlab = "Parameter c", ylab = "Frequency")
hist.data = hist(trapezoid_c, plot=F)
hist.data$counts = log10(hist.data$counts)
plot(hist.data$mids,hist.data$counts, xlab = "Parameter c", ylab = "log10(frequency)")

# ----Mixed statistics
# Ration of symmetry factor to slope of max
plot(symmetry_factor,slope_to_max)