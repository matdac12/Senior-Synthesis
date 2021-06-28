library(ggplot2)
library(tidyverse)

X2018_2019_combined_stats$assisted <- ifelse(is.na(X2018_2019_combined_stats$assist), 0, 1)

logit_data <- na.omit(X2018_2019_combined_stats[,c("points","result","shot_distance","period","remaining_time", "elapsed",  "home_score","away_score", "type", "assisted")])

#Create dummy variables

logit_data$made <- ifelse(logit_data$result == 'made',1,0)
logit_data$close <- ifelse(abs(logit_data$home_score - logit_data$away_score) <4 , 1,0)
logit_data$late <- ifelse( logit_data$period == 4 & logit_data$remaining_time < 120,1,0)

#Find time between shots 

logit_data$time <- logit_data$elapsed
logit_data$time[1] <- 20

for (i in 2:233733) {
  
  logit_data$time[i] <- logit_data$elapsed[i]- logit_data$elapsed[i-1]
}

logit_data$time<- ifelse( logit_data$time < 0, logit_data$elapsed , logit_data$time)

#Histograms
mean(logit_data$time)

ggplot(data= logit_data, aes( x = time)) + stat_bin( binwidth =  1) + geom_vline( xintercept = mean(logit_data$time))  + xlim(0,100)

#This one just finds makes histogram one more time to be sure
makes <- logit_data %>% filter( made == 1)

makes$time[1] <- 65

for (i in 2:107374) {
  
  makes$time[i] <- makes$elapsed[i]- makes$elapsed[i-1]
}

makes$time<- ifelse( makes$time < 0, makes$elapsed , makes$time)


ggplot(data= makes, aes( x = time)) + stat_bin( binwidth =  1) + geom_vline( xintercept = mean(makes$time)) + xlim(0,150)

###

model1 = glm( made ~ shot_distance + close + late + as.factor(type), data = logit_data , family = 'binomial')
summary(model1)


lastmaketime = 0
logit_data$time_since[1] <- 20

for ( i  in 2:233733) {
  
  if (logit_data$made[i] == 1) {
    
    logit_data$time_since[i] <- logit_data$elapsed[i] - lastmaketime
    lastmaketime = logit_data$elapsed[i]
    
  }
  
  else {
    
    logit_data$time_since[i] <- logit_data$elapsed[i] - lastmaketime
  }
  
}

logit_data$time_since <- ifelse(logit_data$time_since < 0 , logit_data$elapsed, logit_data$time_since)

mean(logit_data$time_since)
ggplot(data = logit_data, aes( x = time_since)) + geom_histogram(binwidth = 1, color = 'white') + xlim(0,150)

model2 = glm( made ~ shot_distance + close + late + time_since, data = logit_data , family = 'binomial')
summary(model2)

model_tot = glm( made ~ shot_distance + close + late + time_since + as.factor(type), data = logit_data , family = 'binomial')


#Estimate probabilities
logit_data$prob_exp <- ifelse(logit_data$time_since <= 34, pexp(logit_data$time_since, 0.05033352), pexp(logit_data$time_since, 0.01706836))

logit_data$prob_poiss <- ppois(logit_data$time_since,mean(logit_data$time_since))

logit_data$prob_logit <- predict(model1, logit_data,type = "response" )

logit_data$prob_norm <- pnorm(logit_data$time_since, mean=mean(logit_data$time_since), sd=sd(logit_data$time_since), lower.tail=TRUE) 


#Estimate exponential coefficient
exponential <- logit_data %>% filter(time_since >34)
eexp(exponential$time_since, method = "mle/mme", ci = FALSE, ci.type = "two-sided", 
     ci.method = "exact", conf.level = 0.95)
##############################

grid.arrange( ggplot( data = logit_data , aes (prob_logit, prob_poiss)) + geom_smooth() + xlim(0,1) + ylim(0,1) + ggtitle('Poiss'),
ggplot( data = logit_data , aes (prob_logit, prob_norm)) + geom_smooth() +  xlim(0,1) + ylim(0,1) + ggtitle('norm'),
ggplot( data = logit_data , aes (prob_logit , prob_exp)) + geom_smooth() + xlim(0,1) + ylim(0,1) + ggtitle('exp'), nrow = 2)
library(gridExtra)
library(ggplot2)
