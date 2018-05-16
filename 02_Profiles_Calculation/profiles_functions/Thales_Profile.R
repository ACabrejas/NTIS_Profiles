thales_profile = function(){
  if (mX == "m25") {
    profile = data$m_data_selected$profile_time[data$m_data_selected$link_id == link]
  } else {
    profile = data$m_data_selected$thales_profile[data$m_data_selected$link_id == link]
  }
  profile = profile[(1+(1440*7*(length_weeks$train_weeks))):(1440*7*(length_weeks$total_weeks))]
  return(profile)
}