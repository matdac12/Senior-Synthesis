shot_distance_function <- function(shot_distance) {
  
  b0 <- 0.4158958  # intercept
  X1 <- -0.0432320 
  values <- b0 + X1*shot_distance
  prob <- exp(values)/(1 + exp(values))
  return(prob)
  
}


expected_points_function <- function(shot_distance) {
  
  b0 <- 0.4158958  # intercept
  X1 <- -0.0432320 
  values <- b0 + X1*shot_distance
  prob <- exp(values)/(1 + exp(values))
  if (shot_distance<=23) {
    expected <- 2*prob
  } else {
    expected <- 3*prob
  }
  return(expected)
}


time_function <- function(remaining) {
  late_makes <- subset(season2018_2019_poisson_late, result == "made" & remaining_time <60*remaining )
  
  late_01 <- data.frame(diff(as.matrix(late_makes$elapsed))) 
  late_01$time_between_late <- diff(as.matrix(late_makes$elapsed))
  late_02 <- subset(late_01, time_between_late > 0 )
  final_between_late <- late_02$time_between_late
  delta <- mean(final_between_late)
  return(delta)
}


time_function_4th <- function(remaining,gap) {
  late_makes <- subset(season2018_2019_poisson_late, period ==4 & result == "made" & remaining_time <remaining &
                         abs(season2018_2019_poisson_late$home_score - season2018_2019_poisson_late$away_score)<gap)
  
  late_01 <- data.frame(diff(as.matrix(late_makes$elapsed))) 
  late_01$time_between_late <- diff(as.matrix(late_makes$elapsed))
  late_02 <- subset(late_01, time_between_late > 0 )
  final_between_late <- late_02$time_between_late
  delta <- mean(final_between_late)
  return(delta)
}




