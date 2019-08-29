# NTIS_Profiles
Travel Time Profile Estimations using seasonality extraction via non-parametric regression models applied to UK NTIS Motorway data

This is the code that generated the data for the paper indicated below.

We present a new method for long-term estimation of the expected travel time for links on highways and their variation with time.  
The approach is based on a time series analysis of travel time data from the UK's National Traffic Information Service (NTIS).  
Time series of travel times are characterised by a noisy background variation exhibiting the expected daily and weekly patterns punctuated by large spikes associated with congestion events. 

Some spikes are caused by peak hour congestion and some are caused by unforeseen events like accidents. 
Our algorithm uses thresholding to split the data into background and spike signals, each of which is analysed separately. 
The the background signal is extracted using spectral filtering. 
The periodic part of the spike signal is extracted using locally weighted regression (LWR). 
The final estimated travel time is obtained by recombining these two. 
We assess our method by cross-validating in several UK motorways. 

We use 8 weeks of training data and calculate the error of the resulting travel time estimates for a week of test data, repeating this process 4 times. 
We find that the error is significantly reduced compared to estimates obtained by simple segmentation of the data and compared to the estimates published by the NTIS system.

The published paper [can be found here](https://ieeexplore.ieee.org/abstract/document/8569924).
