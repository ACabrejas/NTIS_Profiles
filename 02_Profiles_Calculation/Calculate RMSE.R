profilematrix = hybrid_results$threshold$`1.2`$profile

values = matrix(0L, nrow = dim(profilematrix)[1], ncol = dim(profilematrix)[2])
differ = matrix(0L, nrow = dim(profilematrix)[1], ncol = dim(profilematrix)[2])
squared = matrix(0L, nrow = dim(profilematrix)[1], ncol = dim(profilematrix)[2])
rms =  matrix(0L, nrow = dim(profilematrix)[1], ncol = dim(profilematrix)[2])

for (link in links_list) {
  i= which(link==links_list)
  print(i)
  link_data = get_link_data(link = link, method = method, spike_parameter = spike_parameter)
  values[i,] = link_data$travel_time[(length(link_data$travel_time)-(4*10080)+1):length((link_data$travel_time))]
  
  differ[i,] = values[i,] - profilematrix[i,]
  squared[i,] = differ[i,]^2
  rms[i,] = sqrt(squared[i,])
}

mean(rms)


i = 1

plot(values[i,], type = 'l')
lines(profilematrix[i,], type = 'l', col="red")
i=i+1
