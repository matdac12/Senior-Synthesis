
library(Stack)
library(tidyverse)
library(ggplot2)

bind_rows(one, two)
Xtotal <- bind_rows(X2009_2010_combined_stats,X2010_2011_combined_stats,X2011_2012_combined_stats,X2012_2013_combined_stats,
                X2013_2014_combined_stats,X2014_2015_combined_stats,X2015_2016_combined_stats,X2016_2017_combined_stats, 
                X2017_2018_combined_stats,X2018_2019_combined_stats )

Xtotal <- bind_rows(season2009_2010,
                    season2010_2011,
                    season2011_2012,
                    season2012_2013,
                    season2013_2014,
                    season2014_2015,
                    season2015_2016,
                    season2016_2017,
                    season2017_2018,
                    season2018_2019)

season2009_2010$data_set <- "2009-2010"
season2010_2011$data_set <- "2010-2011"
season2011_2012$data_set <- "2011-2012"
season2012_2013$data_set <- "2012-2013"
season2013_2014$data_set <- "2013-2014"
season2014_2015$data_set <- "2014-2015"
season2015_2016$data_set <- "2015-2016"
season2016_2017$data_set <- "2016-2017"
season2017_2018$data_set <- "2017-2018"
season2018_2019$data_set <- "2018-2019"

#Histograms
endgames <- na.omit(Xtotal[,c("period", "remaining_time","home_score","away_score")])


onlyfinals <- subset(endgames, period == 4 & remaining_time == 0 )
  

totalscores <- onlyfinals$home_score + onlyfinals$away_score

unite(onlyfinals, SCORED,home_score,away_score, sep = "_", remove = FALSE, na.rm = FALSE)


normdistribution <- dnorm(totalscores)

plot(onlyfinals, normdistribution)

mean(totalscores)


#This is what I need for scores
SCORES<- c(onlyfinals$away_score, onlyfinals$home_score)
mean(SCORES)

scores_frame <- data.frame(SCORES)
#This is the final histogram + normal curve for total points 

total_scores_histogram <- ggplot(scores_frame, aes(SCORES)) +
   geom_histogram(binwidth=1, colour="black", fill = "grey" ,
                   aes(y=..density.., fill=..count..)) + ggtitle("Points Scored Distribution") +
  xlab("Points Scored") + ylab("Density") + labs( subtitle = '1996 - 2019')

total_scores_histogram <- total_scores_histogram + stat_function(fun = dnorm, color = "red", size = 1,  
                                                                 args = list(mean = mean(scores_frame$SCORES), sd = sd(scores_frame$SCORES))) +
                                        geom_vline(aes(xintercept = mean(scores_frame$SCORES, na.rm = T)),
                                           colour = "red", linetype ="longdash", size = .5) +
  annotate( geom = 'text' , x = 125, y = 0.034, label = 'mean = 101', hjust = -0.05)+
  annotate( geom = 'text' , x = 125, y = 0.031, label = 'sd = 10', hjust = -0.06)
total_scores_histogram

ggplot(scores_frame, aes(SCORES)) + geom_density()

#Trnsparant density curve
ggplot(scores_frame, aes(SCORES))  +
  geom_histogram(binwidth=1, colour="black", fill = "white" ,
                 aes(y=..density.., fill=..count..))+
  geom_density(alpha = .2, fill="#FF6655") #overlay with a transparent (alpha value) density plot 
+ geom_vline(aes(xintercept = mean(scores_frame$SCORES, na.rm = T)),
             colour = "red", linetype ="longdash", size = .8)


difference <- onlyfinals$home_score - onlyfinals$away_score 
difference_frame <- data.frame(difference)

#Final margin histogram + normal curve (notice overtime aberration)
difference_histogram <- ggplot(difference_frame, aes(difference)) +
  geom_histogram(binwidth=1, colour="black", fill = "grey" ,
                 aes(y=..density.., fill=..count..)) + ggtitle("Margin Distribution") +
  xlab("Margin (Home Score - Away Score)") + ylab("Density") + labs( subtitle = '1996 - 2019')+ annotate(geom = 'text', x = -2, y = 0.10, 
                                                                               label = '11% of Games end in Overtime',
                                                                               hjust = 1)



  stat_function(fun = dnorm, color = "red", size = 1.2,   args = list(mean = mean(difference_frame$difference), sd = sd(difference_frame$difference)))
  
difference_histogram <- difference_histogram +  stat_function(fun = dnorm, color = "red", size = 1.2, 
                                                              args = list(mean = mean(difference_frame$difference),
                                                                          sd = sd(difference_frame$difference))) +
  geom_vline(aes(xintercept = mean(difference_frame$difference, na.rm = T)),
             colour = "red", linetype ="longdash", size = .5) +
  annotate( geom = 'text' , x = 30, y = 0.10, label = 'mean = 2.1', hjust = -0.05)+
  annotate( geom = 'text' , x = 30, y = 0.0925, label = 'sd = 10', hjust = -0.06)

difference_histogram

#So, final plots to export are 
total_scores_histogram
difference_histogram


#Time between
# Getting distribution of time lags between baskets 
rm(mm,ss)
mm = mean(final_between)
ss = sd(final_between)

poiss <- dnorm(final_between, mean =mean(final_between), sd=sd(final_between))

plot(final_between,poiss)

curve(dnorm(final_between, mean =mean(final_between), sd=sd(final_between)),add=TRUE)


#Getting distribution of time lags between baskets in last 120 seconds of close games

poiss_late <- dnorm(final_between_late, mean =mean(final_between_late), sd=sd(final_between_late))
poiss_late1 <- dpois(final_between_late, lambda  =mean(final_between_late), log = FALSE)

plot(final_between_late,poiss_late)
par(new=TRUE, xaxs = "i")
plot(final_between,poiss,xlab="", ylab="")


par(op)

poisson_DATA <- data.frame(final_between_late,poiss_late)


library(ggplot2)
library(ggeffects)
#Show this
#Average time between baskets

time_histogram <- ggplot(poisson_DATA, aes(final_between_late,poiss_late)) + geom_point() + geom_smooth() + ggtitle("Distribution of time elapsed between baskets") +
  xlab("Seconds") + ylab("Probs")


#So, final plots to export are 
total_scores_histogram
difference_histogram
time_histogram

total_histogram <- grid.arrange(total_scores_histogram,difference_histogram,time_histogram, nrow=3, ncol=1)

library(ggplot2)


#Histogram of shot distance

ggplot(season2009_2010, aes(shot_distance)) +
  geom_histogram(binwidth=1, colour="black", fill = "grey" ,
                 aes(y=..density.., fill=..count..)) + ggtitle("Points Scored Distribution") +
  xlab("Points Scored") + ylab("Density")

ggplot(season2018_2019, aes(shot_distance)) +
  geom_histogram(binwidth=1, colour="black", fill = "grey" ,
                 aes(y=..density.., fill=..count..)) + ggtitle("Points Scored Distribution") +
  xlab("Points Scored") + ylab("Density")


try1 <- na.omit(X2018_2019_combined_stats[,c("shot_distance","player")])

try2 <- subset(try1, player == "James Harden")

ggplot(try2, aes(shot_distance)) +
  geom_histogram(binwidth=1, colour="black", fill = "grey" ,
                 aes(y=..density.., fill=..count..)) + ggtitle("Points Scored Distribution") +
  xlab("Points Scored") + ylab("Density")



player_histogram <- function(name, whatseason) {
  
  
  zz <- subset(whatseason, player == name)
  plot<-  ggplot(zz, aes(shot_distance)) +
    geom_freq(binwidth=1, colour="black", fill = "grey" ,
                   aes(y=..density.., fill=..count..)) + ggtitle("Points Scored Distribution") +
    xlab("Points Scored") + ylab("Density")
  
  return(plot)
}

player_histogram("James Harden", z18_19)

team_player_histogram <- function(whatteam, name, whatseason) {
  
  
  ss <- subset(whatseason, team == whatteam)
  zz <- subset(ss, player == name)
  plot<-  ggplot()+ 
    geom_freqpoly(data = whatseason, binwidth=1,mapping = aes(x = shot_distance, y = 100*..count../sum(..count..),
                                                              color = "Legue Average"), alpha = 0.4, size = 0.8) +
    geom_freqpoly(data = zz ,
                  aes(x = shot_distance, y =100*..count../sum(..count..),
                      color = "Player"), alpha = 1, size = 1.2,) +theme_bw() +xlim(-1,30) + ggtitle("Shot Slection")+
    scale_color_manual("", values = c("steelblue2",  "red4")) + xlab("Shot Distance") +ylab("Frequency %")+
    geom_vline(aes(xintercept = 23.9),  linetype = "dashed",size = 0.9, colour = "gray69")+
    geom_text(mapping = aes(x = 23.5, y = 16, label = "Arch 3pt", hjust = 0.5, vjust = 0))+
    geom_vline(aes(xintercept = 22),  linetype = "dashed",size = 0.9, colour = "gray69")+
    geom_text(mapping = aes(x = 21.8, y =14, label = "Corner 3pt", hjust = 0.5, vjust = 0))
  return(plot)
}

team_player_histogram("HOU", "James Harden", z18_19)

library(ggplot2)

library(grid)

ggplot(season2018_2019, aes(shot_distance)) +
  geom_freqpoly(binwidth=1, colour="red4", 
                 aes(y=..density..))+
   ggtitle("Points Scored Distribution") +
  xlab("Points Scored") + ylab("Density")


ggplot()+ 
  geom_freqpoly(data = season2018_2019, mapping = aes(x = shot_distance, y = 100* ..count../sum(..count..)), size = 1.2) +
     xlab("Shot Distance") +ylab("Frequency %") +xlim(0,30)+
  geom_vline(aes(xintercept = 23),  linetype = "dashed",size = 0.5, colour = "grey")+
  geom_text(mapping = aes(x = 23,
                          y = 12,
                          label = "try",
                          hjust = -0.5,
                          vjust = -0.5))

