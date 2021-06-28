library(dplyr)

season2018_2019_poisson <- na.omit(X2018_2019_combined_stats[,c("result","elapsed","remaining_time","points")])

season2018_2019_poisson$SCORED<-ifelse(season2018_2019_poisson$points>0,1,0)

SCORING <- subset(season2018_2019_poisson, SCORED == 1)

SCORING$time_passed <- mutate(SCORING, time_passed = c(0,diff(SCORING$elapsed)))


SCORING$SCORED <- NULL

time_elapsed <- na.omit(SCORING[,c("points","time_passed.time_passed")])

#Good Code to do stuff
step1 <- subset(X2017_2018_combined_stats, points > 0 )
step2 <- data.frame(diff(as.matrix(step1$elapsed)))
step2$elapsed <- step2$diff.as.matrix.step1.elapsed..
step2$diff.as.matrix.step1.elapsed.. <- NULL
step25<- step2
step25$elapsed <- ifelse(step25$elapsed <0, 720+step25$elapsed,step25$elapsed)
step3 <- subset(step25, elapsed >0)
step4 <- step3$elapsed

mean(step3$elapsed)
mean(step4)
hist(step4, breaks = 100)

rm(step1,step2,step3,step4,step25)

#provando cose 
#data set with makes and associated time 
SCORING <- subset(season2018_2019_poisson, SCORED == 1)


#call the column 
ELAPSED$time_between <- diff(as.matrix(SCORING$elapsed))

#create 2nd data set with only positive differences 
ELAPSED_2 <- subset(ELAPSED, time_between > 0 )

#create final vector with time elappsed between scores
final_between <- ELAPSED_2$time_between

mean(final_between)
hist(final_between, prob=TRUE)
lines(density(final_between))             # add a density estimate with defaults
curve(dnorm(x, mean=mean(final_between), sd=sd(final_between)), add=TRUE)


#Use ggplot to create histogram
time_frame <- data.frame(final_between)
time_hist2 <- ggplot(time_frame, aes(final_between)) +
  geom_histogram(binwidth=0.5, colour="black", fill = "grey" ,
                 aes(y=..density.., fill=..count..)) + ggtitle("Time Distribution") +
  xlab("Time Between Baskets") + ylab("Density")+ geom_vline(aes(xintercept = mean(time_frame$final_between, na.rm = T)),
                                                            colour = "red", linetype ="longdash", size = .5)

#Now final_between is a vector with all the times between baskets 

#next goal separate first 46 minute of the game and last 2
#let's do it 

#need time scores of final 2 minutes of every close game

season2018_2019_poisson_late <- na.omit(X2018_2019_combined_stats[,c("result","period","elapsed","remaining_time","home_score", "away_score")])

late_makes <- subset(season2018_2019_poisson_late, period ==4 & result == "made" & remaining_time <5 &
                       abs(season2018_2019_poisson_late$home_score - season2018_2019_poisson_late$away_score)<4)

# now need data set with only time ellapsed in final 2 minutes 

#create column with time differences between baskets 
late_01 <- data.frame(diff(as.matrix(late_makes$elapsed))) 

#call the column 
late_01$time_between_late <- diff(as.matrix(late_makes$elapsed))

#create 2nd data set with only positive differences 
late_02 <- subset(late_01, time_between_late > 0 )

#create final vector with time elappsed between scores
final_between_late <- late_02$time_between_late

mean(final_between_late)
mean(final_between)









#Start here
#Create function that tells you the average time between points given parameters remaining time and gap between scores
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

casa <- data.frame(seq(from=720, to=1, by = -1))
casa_01 <- seq(from=720, to=1, by = -1)
 for (i in 720:0) {casa$casetta[i]=time_function_4th(i, 5)}

casa_DATA <- data.frame(casa_01,casa$casetta)


#Show this
rate_4th <- ggplot(casa_DATA, aes(casa_01,casa.casetta))  + geom_smooth() + ggtitle("Time Between Baskets, 4th quarter of close games (<5 pt margin)") +
  xlab("4th Quarter(Seconds)") + ylab("Seconds between Baskets")


time_function_4th(45,4)

time_function_4th(15,25)


time_function <- function(whatperiod, remaining) {
  late_makes <- subset(season2018_2019_poisson_late, period ==whatperiod & result == "made" & remaining_time <60*remaining )
  
  late_01 <- data.frame(diff(as.matrix(late_makes$elapsed))) 
  late_01$time_between_late <- diff(as.matrix(late_makes$elapsed))
  late_02 <- subset(late_01, time_between_late > 0 )
  final_between_late <- late_02$time_between_late
  delta <- mean(final_between_late)
  return(delta)
}

#Poiss in minutes, don't use this but the below in seconds

rm(parco, parco_01,parco_02,parco_03,parco_04,parchi,parco_DATA)

parco <- data.frame(seq(from=48, to=1, by = -1))
parco_01 <- seq(from=12, to=1, by = -1)
parco_02 <- seq(from=12, to=1, by = -1)
parco_03 <- seq(from=12, to=1, by = -1)
parco_04 <- seq(from=12, to=1, by = -1)

for (i in 12:1) {
  
  parco_01[13-i] <- time_function(1,i)}
for (i in 12:1) {
  
  parco_02[13-i] <- time_function(2,i)}
for (i in 12:1) {
  
  parco_03[13-i] <- time_function(3,i)}
for (i in 12:1) {
  
  parco_04[13-i] <- time_function(4,i)}



parchi<- c(parco_01,parco_02,parco_03,parco_04)

parco <- data.frame(seq(from=48, to=1, by = -1))

parco_DATA <- data.frame(parco,parchi)
parco_DATA$parco <- seq(from=1, to=48, by = 1)
time_function(1,11)

#Show this
rate_total <- ggplot() + 
  geom_line(data =parco_DATA, aes(x = parco, y = parchi, color = "Time Between Baskets"), size = 1) +
  scale_color_manual(name = " ", 
                     values = c("Time Between Baskets" = "blue")) +
  ggtitle("Time Between Baskets (48 min game)")+
   scale_x_continuous(name="Minutes", limits=c(0, 48)) +
  scale_y_continuous(name="Rate", limits=c(10, 30))

  
  ggplot(parco_DATA, aes(parco,parchi)) + geom_point() + geom_smooth() + ggtitle("Time Between Baskets, 48 min game") +
  xlab("Game (Minutes)") + ylab("Seconds between Baskets")




######

time_function2 <- function(whatperiod, remaining) {
  late_makes <- subset(season2018_2019_poisson_late, period ==whatperiod & result == "made" & remaining_time <remaining )
  
  late_01 <- data.frame(diff(as.matrix(late_makes$elapsed))) 
  late_01$time_between_late <- diff(as.matrix(late_makes$elapsed))
  late_02 <- subset(late_01, time_between_late > 0 )
  final_between_late <- late_02$time_between_late
  delta <- mean(final_between_late)
  return(delta)
}


parco_05 <- seq(from=720, to=1, by = -1)
parco_06 <- seq(from=720, to=1, by = -1)
parco_07 <- seq(from=720, to=1, by = -1)
parco_08 <- seq(from=720, to=1, by = -1)

for (i in 720:1) {
  
  parco_05[721-i] <- time_function(1,i)}
for (i in 720:1) {
  
  parco_06[721-i] <- time_function(2,i)}
for (i in 720:1) {
  
  parco_07[721-i] <- time_function(3,i)}
for (i in 720:1) {
  
  parco_08[721-i] <- time_function(4,i)}



parchi1<- c(parco_05,parco_06,parco_07,parco_08)



parco_DATA1 <- data.frame(parchi1)
parco_DATA1$parco1 <- seq(from=1, to=2880, by = 1)

library(ggplot2)
#Show this
rate_total1 <- ggplot() + 
  geom_line(data =parco_DATA1, aes(x = parco1, y = parchi1, color = "Time Between Baskets"), size = 1) +
  scale_color_manual(name = " ", 
                     values = c("Time Between Baskets" = "blue")) +
  ggtitle("Time Between Baskets (48 min game)")+
  scale_x_continuous(name="Seconds", limits=c(0, 2880)) +
  scale_y_continuous(name="Rate", limits=c(10, 30))

rm(season2018_2019_poisson,season2018_2019_poisson_late)
rm(XT, XT_made)



rate_4th
rate_total1 +
  geom_vline(aes(xintercept = 700),  linetype = "dashed",size = 0.5, colour = "grey")

ggplot(data=ggdata, aes(x = time_frame.final_between, y= timeprobs)) + geom_point() #This is poiss probs 

library(grid)
library(gridExtra)

grid.arrange(time_hist2,
             rate_4th,
             rate_total1, nrow=3, ncol=1)



#The histogram and the poisson ditribution of times between scoring posessions almost matches

timeprobs <- dpois(x=time_frame$final_between, lambda = mean(time_frame$final_between))
timeseq <- seq(from=0, to=50, by=1)
plot(timeseq,timeprobs)
comp = 1-sum(timeprobs)


chisq.test(x=c(time_frame$final_between,0), p=timeprobs)

chisq.test(x=c(20, 10, 5, 3, 2, 1,0), p=c(timeprobs,comp))


ppois( q=15, lambda = 15, lower.tail = TRUE)


rpois(51,27)
ggdata <- data.frame(time_frame$final_between,timeprobs)
distr2 <- ggplot() + geom_line(data=ggdata, aes(x = time_frame.final_between, y= timeprobs)) + geom_point()  +
  geom_vline(aes(xintercept = mean(time_frame$final_between)),
             colour = "red", linetype ="longdash", size = .5)


shot_rate <- ggplot() + 
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_combined, color = "3pt"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) + 
  scale_color_manual(name = " ", 
                     values = c("3pt" = "blue", "Midrange" = "red")) +
  ggtitle("3pt Shot vs Midrange")+
  xlab('Seasons') +
  ylab('Rate')



plot(time_frame$final_between,timeprobs)

ggplot()+ stat_function(fun = dpois, color = "red", size = 1,  
              args = list(x=time_frame$final_between, lambda = mean(time_frame$final_between))) +
  geom_vline(aes(xintercept = mean(time_frame$final_between)),
             colour = "red", linetype ="longdash", size = .5)



function1 <- function(whatperiod, whatminute) {
  step1 <- subset(X2017_2018_combined_stats, points > 0 & period ==whatperiod & result == "made" & remaining_time <whatminute*60
                  & remaining_time >whatminute*60 - 60)
  step2 <- data.frame(diff(as.matrix(step1$elapsed)))
  step2$elapsed <- step2$diff.as.matrix.step1.elapsed..
  step2$diff.as.matrix.step1.elapsed.. <- NULL
 
  step3 <- subset(step2, elapsed >0)
  step4 <- step3$elapsed
  delta <- mean(step4)
  return(delta)
}


seq1 <- seq(from=12, to=1, by = -1)
seq2 <- seq(from=12, to=1, by = -1)

for (i in 12:1) {
  
  seq1[13-i] <- function1(1,i)}

plot(seq2,seq1)

function1(1,11)


function2 <- function(fromquarter,frominute, toquarter, tominute) {
  step1 <- subset(X2017_2018_combined_stats, points > 0 & ((period >=fromquarter & remaining_time <frominute*60) | 
                    (period <=toquarter & remaining_time >tominute*60)) & result == "made")
  step2 <- data.frame(diff(as.matrix(step1$elapsed)))
  step2$elapsed <- step2$diff.as.matrix.step1.elapsed..
  step2$diff.as.matrix.step1.elapsed.. <- NULL
  
  step3 <- subset(step2, elapsed >0)
  step4 <- step3$elapsed
  delta <- mean(step4)
  return(delta)
}

function2(4,1,4,0)


function3 <- function(whatperiod) {
  step1 <- subset(X2017_2018_combined_stats, points > 0 & period ==whatperiod & remaining_time < 15)
  step2 <- data.frame(diff(as.matrix(step1$elapsed)))
  step2$elapsed <- step2$diff.as.matrix.step1.elapsed..
  step2$diff.as.matrix.step1.elapsed.. <- NULL
  'step25<- step2
  step25$elapsed <- ifelse(step25$elapsed <0, 720+step25$elapsed,step25$elapsed)'
  step3 <- subset(step2, elapsed >0)
  step4 <- step3$elapsed
  
  delta <- mean(step4)
  return(delta)
}

function3(1)
function3(2)
function3(3)
function3(4)



