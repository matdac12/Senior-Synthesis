#Note, this is the initial logistic model made, the final will be from other project


season2018_2019$MADE<-ifelse(season2018_2019$result=="made",1,0)


#Estimate Logit Model with shot distance (only)
LogitModel = glm(MADE ~ shot_distance , data = season2018_2019, 
                 family = "binomial")
summary(LogitModel)
plot(allEffects(LogitModel))

#Calculate probability from shot distance
model_range <- seq(from=min(season2018_2019$shot_distance), to=max(season2018_2019$shot_distance), by=.1)


b0 <- 0.4158958  # intercept
X1 <- -0.0432320  # coefficient

logitvalues <- b0 + X1*model_range 

logitvaluesreals <- b0 + X1*season2018_2019$shot_distance

logitprobs <- exp(logitvalues)/(1 + exp(logitvalues))

logitprobsreal <-exp(logitvaluesreals)/(1 + exp(logitvaluesreals))


#Probability of making the shot given shot distance

random <- data.frame(model_range,logitprobs)
random$pps <- ifelse(random$model_range<=23, 2*random$logitprobs, 3*random$logitprobs)

distance_prob <- ggplot() + 
  geom_line(data = random, aes(x = model_range, y = logitprobs),color = 'purple',  size = 1) +
  scale_x_continuous(name="Shot Distance", limits=c(0,80)) +
  scale_y_continuous(name="Probability", limits=c(0, 0.75)) + ggtitle("Predicted Probability of making the shot")
distance_prob

ggplot() + 
  geom_line(data = random, aes(x = model_range, y = (logitprobs/(1-logitprobs))),color = 'purple',  size = 1) +
  scale_x_continuous(name="Shot Distance", limits=c(0,80)) +
  ggtitle("Odds of making the shot") + labs(y = 'ODDS')

ggplot() + 
  geom_line(data = random, aes(x = model_range, y = log(logitprobs/(1-logitprobs))),color = 'purple',  size = 1) +
  scale_x_continuous(name="Shot Distance", limits=c(0,80)) +
  ggtitle("Log Odds of making the shot") + labs(y = 'Log Odds')

ggplot() + 
  geom_line(data = random, aes(x = model_range, y = pps),color = 'purple',  size = 1) +
  scale_x_continuous(name="Shot Distance", limits=c(0,45)) +
  scale_y_continuous(name="Expected Points", limits=c(0, 1.5)) + ggtitle("Predicted Expected points") 


#Working code for expected points variation with distance
# I had to separate the twos and threes and then combine them

season2018_2019$THREE<-ifelse(season2018_2019$shot_distance>23,1,0)

season2018_2019$VALUE<-ifelse(season2018_2019$THREE==1,3,2)

my_range <- seq(from=0, to=46, by=.5)
my_range2 <- seq(from=0, to=23, by=.5)
my_range3 <- seq(from=23.5, to=46, by=.5)


my_values2 <- b0 + X1*my_range2
my_values3 <- b0 + X1*my_range3

my_probabilities2 <- exp(my_values2)/(1 + exp(my_values2))
my_probabilities3 <- exp(my_values3)/(1 + exp(my_values3))

my_expecedpoints2 <- 2*my_probabilities2
my_expectedpoints3 <- 3*my_probabilities3

my_expectedpoints <- c(my_expecedpoints2,my_expectedpoints3)

expected_DATA <- data.frame(my_range,my_expectedpoints)

#Another way to make the plot needed, use this one (!)
expected_value <- ggplot() + 
  geom_line(data = expected_DATA, aes(x = my_range, y = my_expectedpoints), size = 1) +
  scale_x_continuous(name="Shot Distance", limits=c(0,47)) +
  scale_y_continuous(name="Expected Value", limits=c(0.4, 1.3)) + ggtitle("Expected Value")
expected_value

#Find pps by zone

zone1 <- expected_DATA %>% 
  filter(my_range > 24 & my_range <=25) 
mean(zone1$my_expectedpoints)

#Make function for shot made and expected points

shot_distance_function <- function(shot_distance) {
        
        b0 <- 0.4158958  # intercept
        X1 <- -0.0432320 
        values <- b0 + X1*shot_distance
        prob <- exp(values)/(1 + exp(values))
        return(prob)
        
}

shot_distance_function(23)


#Make function for expected point

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



shot_distance_function(25)
expected_points_function(23.0001)
# End working code 


#Add Late and Close dummies

#Try to add a dummy variable for late in the game

season2018_2019_wtime <- na.omit(X2018_2019_combined_stats[,c("points","result","shot_distance","converted_x","converted_y","period","remaining_time")])

period4th <- subset(season2018_2019_wtime, period ==4)

season2018_2019_wtime$LATE <- ifelse(season2018_2019_wtime$period ==4 & season2018_2019_wtime$remaining_time<120,1,0)

season2018_2019_wtime$MADE<-ifelse(season2018_2019_wtime$result=="made",1,0)

#Estimate logit model adding late game situations

Logit_wtime = glm(MADE ~ shot_distance + LATE, data = season2018_2019_wtime, 
                 family = "binomial")
summary(Logit_wtime)

#Plot new model 

install.packages("effects") # only need to do once. 
library(effects)

plot(allEffects(Logit_wtime)) #It takes a while 


#Try to add close game at logit model

season2018_2019_wtime <- na.omit(X2018_2019_combined_stats[,c("points","result","shot_distance","converted_x","converted_y","period","remaining_time", "home_score","away_score")])

season2018_2019_wtime$CLOSE <- ifelse(abs(season2018_2019_wtime$home_score - season2018_2019_wtime$away_score)<4,1,0)
season2018_2019_wtime$FORTH <- ifelse(season2018_2019_wtime$period ==4 & season2018_2019_wtime$remaining_time<60 & abs(season2018_2019_wtime$home_score - season2018_2019_wtime$away_score)<4,1,0)



#This is the one 

Logit_time_close = glm(MADE ~ shot_distance + LATE + CLOSE , data = season2018_2019_wtime, 
                  family = "binomial")

summary(Logit_time_close) #Use this

plot(allEffects(Logit_time_close)) #It takes a while

#Save Table in Word
library(stargazer)
stargazer(Logit_time_close,
          type="html",
          out="Logit_late_close.doc")




#Compare Players

X2018_2019_combined_stats$CLOSE <- ifelse(X2018_2019_combined_stats$period == 4 & abs(X2018_2019_combined_stats$home_score - X2018_2019_combined_stats$away_score)<5,1,0)
X2018_2019_combined_stats$LATE <- ifelse(X2018_2019_combined_stats$period ==4 & X2018_2019_combined_stats$remaining_time < 120 
                                         & X2018_2019_combined_stats$CLOSE ==1, 1,0)
X2018_2019_combined_stats$MADE <- ifelse(X2018_2019_combined_stats$result == "made",1,0)

players <- subset(X2018_2019_combined_stats, player == "LeBron James" | player == "Stephen Curry" )


gg1<- ggplot(players, aes(shot_distance, as.numeric(MADE), color=player)) +
  stat_smooth(method="glm", family= binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=player))  +
  xlab("Shot Distance") + ylab("Pr (makes)")  +
  xlim(-2,40)



gg1 + facet_grid(. ~ CLOSE) + ggtitle("Lebron James vs Steph Curry vs Close game")


gg1 + facet_grid(. ~ LATE) + ggtitle("Lebron James vs Steph Curry vs LATE CLOSE GAME")


#Compare Players (USING ALL 10 SEASONS)
players1 <- subset(Xtotal, player == "LeBron James" | player == "Stephen Curry" )

Xtotal$CLOSE <- ifelse(Xtotal$period == 4 & abs(Xtotal$home_score - Xtotal$away_score)<5,1,0)
Xtotal$LATE <- ifelse(Xtotal$period ==4 & Xtotal$remaining_time < 120 
                                         & Xtotal$CLOSE ==1, 1,0)
Xtotal$MADE <- ifelse(Xtotal$result == "made",1,0)
gg2<- ggplot(players1, aes(shot_distance, as.numeric(MADE), color=player)) +
  stat_smooth(method="glm", family= binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=player))  +
  xlab("Shot Distance") + ylab("Pr (makes)")  +
  xlim(-2,40)



gg2 + facet_grid(. ~ CLOSE) + ggtitle("Lebron James vs Steph Curry vs Close game")


gg2 + facet_grid(. ~ LATE) + ggtitle("Lebron James vs Steph Curry vs LATE CLOSE GAME")





#Try different combinations
#Logit_try1 = glm(MADE ~ LATE + CLOSE , data = season2018_2019_wtime, 
                       #family = "binomial")
#summary(Logit_try1)

#plot(allEffects(Logit_try1))


#Logit_try2 = glm(MADE ~ LATE * CLOSE , data = season2018_2019_wtime, 
  #               family = "binomial")
#summary(Logit_try2)

#plot(allEffects(Logit_try2))

#Logit_try3 = glm(MADE ~ shot_distance * LATE , data = season2018_2019_wtime, 
 #                family = "binomial")
#summary(Logit_try3)

#plot(allEffects(Logit_try3))

#The effects of close are not enough

# try to use time remaining in 4th period as contnuous variable

#Logit_period = glm(MADE ~ shot_distance + remaining_time, data = period4th, 
                  #family = "binomial")
#summary(Logit_period)
#convert time remaining 
#period4th$REMAINING <- 720 - period4th$remaining_time
#Logit_period_02 = glm(MADE ~ shot_distance + REMAINING, data = period4th, 
                   #family = "binomial")
#summary(Logit_period_02)
#plot(allEffects(Logit_period_02))

#did not work