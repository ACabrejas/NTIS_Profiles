
## Clear-all
rm(list = ls()) # Clear variables
graphics.off()  # Clear plots
cat("\014")     # Clear console

library(reshape2)
library(ggplot2)
## Choose motorway
mX <- "m11"

link_number = 1
week_start = 1
length_weeks = 3

source("./profiles_functions/get_link_data.R")

method = "threshold"
spike_parameter = 1.1

## Load motorway data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#file_name <- paste(mX,'_data_selected_and_links_list_AdvBackgroundSpikes_th_0.8.RData',sep="")
file_name = paste(mX,"_data_spikes_background_threshold_only.rda", sep = "")
load(paste('../00_Data/04_Processed_data/',file_name, sep = ''))
links_list <- links_list_df$link_id
labels_mx = c("M6", "M11", "M25")
if(mX=="m6"){label_mx = labels_mx[1]}else if(mX=="m11"){label_mx = labels_mx[2]}else if(mX=="m25"){label_mx=labels_mx[3]}

link = links_list[link_number]
link_data = get_link_data(link = link, method = method, spike_parameter = spike_parameter)

background = link_data$background[(1+10080*week_start):(10080*week_start+10080*length_weeks)]
spikes = link_data$spikes[(1+10080*week_start):(10080*week_start+10080*length_weeks)]
spikes_plot = rep(0, length(spikes))
spikes_plot[spikes!=0] = spikes[spikes!=0] + background
spikes_plot[spikes==0] = NA

time = seq(1:length(spikes_plot))

plot_data = data.frame(Spikes = spikes_plot, Background = background, Time=time)

plot_data_long = melt(plot_data, id="Time")

dates = c("07/03/2016","08/03/2016","09/03/2016","10/03/2016","11/03/2016",
          "12/03/2016","13/03/2016","14/03/2016","15/03/2016","16/03/2016",
          "17/03/2016","18/03/2016","19/03/2016","20/03/2016","21/03/2016",
          "22/03/2016","23/03/2016","24/03/2016","25/03/2016","26/03/2016",
          "27/03/2016","28/03/2016")

axis_breaks = seq(1,22)


ggplot(data = plot_data_long, aes(x=Time, y=value, colour=variable, group = variable)) + 
  ylab("Travel time [seconds]") + 
  xlab("Date") +
  geom_line(size=1.5) + theme(text = element_text(size=28),axis.text.x = element_text(angle = 0)) +
  scale_x_discrete(breaks=axis_breaks, labels=dates) +
  scale_y_continuous(breaks=seq(0.025,0.15,by=0.025)) + 
  theme(legend.position = c(0.14, 0.9)) + theme(legend.text=element_text(size=24)) 
