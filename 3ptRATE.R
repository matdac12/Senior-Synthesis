#Create functions for rate and lenght to use later

length(which(X2009_2010_combined_stats$shot_distance > 22))

length_function <- function(data) {
  howmany <- length(which(data$shot_distance > 23))
  return(howmany)
}

#Create function for how many threes shot in a season
length_function_01 <- function(data) {
  howmany <- length(which(data$THREE ==1))
  return(howmany)
}

#Function for finding the rate of something
rate_find <-  function(threes, total) { 
  rate <- threes/total
  return(rate)
  }

#Function for finding the rate of threes taken in a season
rate_function <- function(data) {
  rate <- rate_find(length_function_01(data), length(data$shot_distance))
  return(rate)
}

#Function for how many midrange shots in a season
length_function_02 <- function(data) {
  howmany <- length(which(data$MID ==1))
  return(howmany)
}

#Function for finding the rate of midrange taken in a season
rate_function_mid <- function(data) {
  rate <- rate_find(length_function_02(data), length(data$shot_distance))
  return(rate)
}


#Function for finding how many paint shots in a season
length_function_03 <- function(data) {
  howmany <- length(which(data$PAINT ==1))
  return(howmany)
}
#Function for finding the rate of paint shots in a season
rate_function_paint <- function(data) {
  rate <- rate_find(length_function_03(data), length(data$shot_distance))
  return(rate)}

#Convert coordinates from full court to half court, then take in account that corner threes are closer to the basket 
#Also add dummy variables for midrange and paint

season2018_2019$shot_x<-ifelse(season2018_2019$converted_y > 47,50 - season2018_2019$converted_x,season2018_2019$converted_x)
season2018_2019$shot_y<-ifelse(season2018_2019$converted_y > 47,94 - season2018_2019$converted_y,season2018_2019$converted_y)
season2018_2019$THREE <- ifelse(season2018_2019$shot_y < 14.5, ifelse(season2018_2019$shot_distance > 21.5,1,0), ifelse(season2018_2019$shot_distance > 23,1,0))
rate_function(season2018_2019)


season2009_2010$shot_x<-ifelse(season2009_2010$converted_y > 47,50 - season2009_2010$converted_x,season2009_2010$converted_x)
season2009_2010$shot_y<-ifelse(season2009_2010$converted_y > 47,94 - season2009_2010$converted_y,season2009_2010$converted_y)
season2009_2010$THREE <- ifelse(season2009_2010$shot_y < 14.5, ifelse(season2009_2010$shot_distance > 21.5,1,0), ifelse(season2009_2010$shot_distance > 23,1,0))
rate_function(season2009_2010)

season2010_2011$shot_x<-ifelse(season2010_2011$converted_y > 47,50 - season2010_2011$converted_x,season2010_2011$converted_x)
season2010_2011$shot_y<-ifelse(season2010_2011$converted_y > 47,94 - season2010_2011$converted_y,season2010_2011$converted_y)
season2010_2011$THREE <- ifelse(season2010_2011$shot_y < 14.5, ifelse(season2010_2011$shot_distance > 21.5,1,0), ifelse(season2010_2011$shot_distance > 23,1,0))

season2011_2012$shot_x<-ifelse(season2011_2012$converted_y > 47,50 - season2011_2012$converted_x,season2011_2012$converted_x)
season2011_2012$shot_y<-ifelse(season2011_2012$converted_y > 47,94 - season2011_2012$converted_y,season2011_2012$converted_y)
season2011_2012$THREE <- ifelse(season2011_2012$shot_y < 14.5, ifelse(season2011_2012$shot_distance > 21.5,1,0), ifelse(season2011_2012$shot_distance > 23,1,0))

season2012_2013$shot_x<-ifelse(season2012_2013$converted_y > 47,50 - season2012_2013$converted_x,season2012_2013$converted_x)
season2012_2013$shot_y<-ifelse(season2012_2013$converted_y > 47,94 - season2012_2013$converted_y,season2012_2013$converted_y)
season2012_2013$THREE <- ifelse(season2012_2013$shot_y < 14.5, ifelse(season2012_2013$shot_distance > 21.5,1,0), ifelse(season2012_2013$shot_distance > 23,1,0))

season2013_2014$shot_x<-ifelse(season2013_2014$converted_y > 47,50 - season2013_2014$converted_x,season2013_2014$converted_x)
season2013_2014$shot_y<-ifelse(season2013_2014$converted_y > 47,94 - season2013_2014$converted_y,season2013_2014$converted_y)
season2013_2014$THREE <- ifelse(season2013_2014$shot_y < 14.5, ifelse(season2013_2014$shot_distance > 21.5,1,0), ifelse(season2013_2014$shot_distance > 23,1,0))

season2014_2015$shot_x<-ifelse(season2014_2015$converted_y > 47,50 - season2014_2015$converted_x,season2014_2015$converted_x)
season2014_2015$shot_y<-ifelse(season2014_2015$converted_y > 47,94 - season2014_2015$converted_y,season2014_2015$converted_y)
season2014_2015$THREE <- ifelse(season2014_2015$shot_y < 14.5, ifelse(season2014_2015$shot_distance > 21.5,1,0), ifelse(season2014_2015$shot_distance > 23,1,0))

season2015_2016$shot_x<-ifelse(season2015_2016$converted_y > 47,50 - season2015_2016$converted_x,season2015_2016$converted_x)
season2015_2016$shot_y<-ifelse(season2015_2016$converted_y > 47,94 - season2015_2016$converted_y,season2015_2016$converted_y)
season2015_2016$THREE <- ifelse(season2015_2016$shot_y < 14.5, ifelse(season2015_2016$shot_distance > 21.5,1,0), ifelse(season2015_2016$shot_distance > 23,1,0))

season2016_2017$shot_x<-ifelse(season2016_2017$converted_y > 47,50 - season2016_2017$converted_x,season2016_2017$converted_x)
season2016_2017$shot_y<-ifelse(season2016_2017$converted_y > 47,94 - season2016_2017$converted_y,season2016_2017$converted_y)
season2016_2017$THREE <- ifelse(season2016_2017$shot_y < 14.5, ifelse(season2016_2017$shot_distance > 21.5,1,0), ifelse(season2016_2017$shot_distance > 23,1,0))

season2017_2018$shot_x<-ifelse(season2017_2018$converted_y > 47,50 - season2017_2018$converted_x,season2017_2018$converted_x)
season2017_2018$shot_y<-ifelse(season2017_2018$converted_y > 47,94 - season2017_2018$converted_y,season2017_2018$converted_y)
season2017_2018$THREE <- ifelse(season2017_2018$shot_y < 14.5, ifelse(season2017_2018$shot_distance > 21.5,1,0), ifelse(season2017_2018$shot_distance > 23,1,0))


#Create dummy variables for midrange 
season2009_2010$MID <- ifelse(season2009_2010$shot_y < 14.5, ifelse(season2009_2010$shot_distance<22 & season2009_2010$shot_distance >9,1,0),
                              ifelse(season2009_2010$shot_distance<23 & season2009_2010$shot_distance >9,1,0))
season2010_2011$MID <- ifelse(season2010_2011$shot_y < 14.5, ifelse(season2010_2011$shot_distance<22 & season2010_2011$shot_distance >9,1,0),
                              ifelse(season2010_2011$shot_distance<23 & season2010_2011$shot_distance >9,1,0))
season2011_2012$MID <- ifelse(season2011_2012$shot_y < 14.5, ifelse(season2011_2012$shot_distance<22 & season2011_2012$shot_distance >9,1,0),
                              ifelse(season2011_2012$shot_distance<23 & season2011_2012$shot_distance >9,1,0))
season2012_2013$MID <- ifelse(season2012_2013$shot_y < 14.5, ifelse(season2012_2013$shot_distance<22 & season2012_2013$shot_distance >9,1,0),
                              ifelse(season2012_2013$shot_distance<23 & season2012_2013$shot_distance >9,1,0))
season2013_2014$MID <- ifelse(season2013_2014$shot_y < 14.5, ifelse(season2013_2014$shot_distance<22 & season2013_2014$shot_distance >9,1,0),
                              ifelse(season2013_2014$shot_distance<23 & season2013_2014$shot_distance >9,1,0))
season2014_2015$MID <- ifelse(season2014_2015$shot_y < 14.5, ifelse(season2014_2015$shot_distance<22 & season2014_2015$shot_distance >9,1,0),
                              ifelse(season2014_2015$shot_distance<23 & season2014_2015$shot_distance >9,1,0))
season2015_2016$MID <- ifelse(season2015_2016$shot_y < 14.5, ifelse(season2015_2016$shot_distance<22 & season2015_2016$shot_distance >9,1,0),
                              ifelse(season2015_2016$shot_distance<23 & season2015_2016$shot_distance >9,1,0))
season2016_2017$MID <- ifelse(season2016_2017$shot_y < 14.5, ifelse(season2016_2017$shot_distance<22 & season2016_2017$shot_distance >9,1,0),
                              ifelse(season2016_2017$shot_distance<23 & season2016_2017$shot_distance >9,1,0))
season2017_2018$MID <- ifelse(season2017_2018$shot_y < 14.5, ifelse(season2017_2018$shot_distance<22 & season2017_2018$shot_distance >9,1,0),
                              ifelse(season2017_2018$shot_distance<23 & season2017_2018$shot_distance >9,1,0))
season2018_2019$MID <- ifelse(season2018_2019$shot_y < 14.5, ifelse(season2018_2019$shot_distance<22 & season2018_2019$shot_distance >9,1,0),
                              ifelse(season2018_2019$shot_distance<23 & season2018_2019$shot_distance >9,1,0))

#Create dummy variable for paint shot 
season2009_2010$PAINT <- ifelse(season2009_2010$shot_distance<10,1,0)
season2010_2011$PAINT <- ifelse(season2010_2011$shot_distance<10,1,0)
season2011_2012$PAINT <- ifelse(season2011_2012$shot_distance<10,1,0)
season2012_2013$PAINT <- ifelse(season2012_2013$shot_distance<10,1,0)
season2013_2014$PAINT <- ifelse(season2013_2014$shot_distance<10,1,0)
season2014_2015$PAINT <- ifelse(season2014_2015$shot_distance<10,1,0)
season2015_2016$PAINT <- ifelse(season2015_2016$shot_distance<10,1,0)
season2016_2017$PAINT <- ifelse(season2016_2017$shot_distance<10,1,0)
season2017_2018$PAINT <- ifelse(season2017_2018$shot_distance<10,1,0)
season2018_2019$PAINT <- ifelse(season2018_2019$shot_distance<10,1,0)
                              





#Now find rate of every season and plot it.
#Calculate 3 pt rate from 2009-2010 to 2018-2019 + add current rate for 2019-2020 season
rate_combined <- c(rate_function(season2009_2010),rate_function(season2010_2011),rate_function(season2011_2012),rate_function(season2012_2013),
                   rate_function(season2013_2014),rate_function(season2014_2015),rate_function(season2015_2016),rate_function(season2016_2017),
                   rate_function(season2017_2018),rate_function(season2018_2019),0.38)
rate_sequence <- seq(from=2010, to=2020, by = 1)
rate_DATA <- data.frame(rate_sequence,rate_combined)
library(ggplot2)


#Plots

#The following is the ggplot graph with extrapolated curve
ggplot(rate_DATA, aes(rate_sequence,rate_combined)) + geom_point() + geom_smooth() + ggtitle("3pt shot rate") + stat_smooth(method = "gam", fullrange = TRUE) +
  xlab("Years (2009-2018)") + ylab("3pt rate") + expand_limits(x = c(2009,2030), y = c(0,0.60))

#The following is regular ggplot
ggplot(rate_DATA, aes(rate_sequence,rate_combined)) + geom_point()+ geom_smooth() + ggtitle("3pt shot rate") + 
  xlab("Years (2009-2018)") + ylab("3pt rate") + expand_limits(x = c(2009,2020), y = c(0,0.50))

#Using BasketballReference stats plot 3pt rate from 1979 to 2020
bball_reference$rate3pt <- bball_reference$'3PA'/bball_reference$FGA
bball_reference$rate2pt <- (bball_reference$FGA - bball_reference$`3PA`) / bball_reference$FGA
bball_sequence <- seq(from=2020, to=1980, by= -1)
#Plot, regular
ggplot(bball_reference, aes(bball_sequence,rate3pt)) + geom_point() + geom_smooth() + ggtitle("3pt shot rate (1970-2019)") + 
  xlab("Years (2009-2018)") + ylab("3pt rate") + expand_limits(x = c(1975,2020), y = c(0,0.50))


#Calculate midrange rate for the last 10 seasons
rate_mid_combined <- c(rate_function_mid(season2009_2010),rate_function_mid(season2010_2011),rate_function_mid(season2011_2012),rate_function_mid(season2012_2013)
                       ,rate_function_mid(season2013_2014),rate_function_mid(season2014_2015),rate_function_mid(season2015_2016),rate_function_mid(season2016_2017)
                       ,rate_function_mid(season2017_2018),rate_function_mid(season2018_2019),0.173273)

rate_mid_DATA <- data.frame(rate_sequence,rate_mid_combined)
data_rate <- data.frame(rate_sequence,rate_combined,rate_mid_combined,rate_paint_combined)

#Calculate paint shot rate last 10 seasons
rate_paint_combined <- c(rate_function_paint(season2009_2010), rate_function_paint(season2010_2011), rate_function_paint(season2011_2012)
                         , rate_function_paint(season2012_2013), rate_function_paint(season2013_2014), rate_function_paint(season2014_2015)
                         , rate_function_paint(season2015_2016), rate_function_paint(season2016_2017), rate_function_paint(season2017_2018)
                         , rate_function_paint(season2018_2019),0.4543644)


#This graph shows the two lines and the legend
shot_rate <- ggplot() + 
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_combined, color = "3pt"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) + 
  scale_color_manual(name = " ", 
                     values = c("3pt" = "blue", "Midrange" = "red")) +
  ggtitle("3pt Shot vs Midrange")+
  xlab('Seasons') +
  ylab('Rate')
#This one gives better axis
rate_final <- shot_rate + scale_x_continuous(name="NBA Seasons", limits=c(2009, 2020)) +
  scale_y_continuous(name="Shooting Rate", limits=c(0.10, 0.45))



#This one gives 3ptrate from 79 to 2020 but midrange only from 2009 to 2020
ggplot() + 
  geom_line(data = bball_reference, aes(x = bball_sequence, y = rate3pt, color = "3pt"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) + 
  scale_color_manual(name = " ", 
                     values = c("3pt" = "blue", "Midrange" = "red")) +
  ggtitle("3pt Shot vs Midrange")+
  xlab('Seasons') +
  ylab('Rate')+ scale_x_continuous(name="NBA Seasons", limits=c(1998, 2021)) +
  scale_y_continuous(name="Shooting Rate", limits=c(0.10, 0.52))


#This one is the same as above but adds 2 pt shooting rate 
plot2 <- ggplot() + 
  geom_line(data = bball_reference, aes(x = bball_sequence, y = rate3pt, color = "3pt"), size = 1) +
  geom_line(data = bball_reference, aes(x = bball_sequence, y = rate2pt, color = "2pt"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) + 
  scale_color_manual(name = " ", 
                     values = c("3pt" = "blue", "2pt" = "green", "Midrange" = "red")) +
  ggtitle("3pt vs 2pt Shooting rate")+
  xlab('Seasons') +
  ylab('Shooting Rate') + labs(subtitle = '1979 - 2019') + xlim(1979, 2025)

plot2

#This one has 3pt, midrange, paint from 2009 to 2020
paint_mid_threes <- ggplot() + 
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_combined, color = "3pt"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_paint_combined, color = "Paint"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) + 
  scale_color_manual(name = " ", 
                     values = c("3pt" = "blue", "Paint" = "green", "Midrange" = "red")) +
  ggtitle("RIP Midrange ")+
  xlab('Seasons') +
  ylab('Rate') + scale_x_continuous(name="NBA Seasons", limits=c(2009, 2021)) +
  scale_y_continuous(name="Shooting Rate", limits=c(0.10, 0.52))





#Use basketball reference stats
bball_reference$perc3 <- bball_reference$'3P%'


#This one adds 3 pt percetntage which doesn't vary
final_rate <- ggplot() + 
  geom_line(data = bball_reference, aes(x = bball_sequence, y = rate3pt, color = "3pt Rate"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_paint_combined, color = "Paint"), size = 1) +
  geom_line(data = data_rate, aes(x = rate_sequence, y = rate_mid_combined, color = "Midrange"), size = 1) +
  geom_line(data = bball_reference, aes(x = bball_sequence, y = perc3, color = "3pt %"), size = 1) +
  scale_color_manual(name = " ", 
                     values = c("3pt Rate" = "blue", "3pt %" = "grey", "Paint" = "green", "Midrange"= "red")) +
  labs( x  = 'NBA Seasons',
        y = 'Shooting Rate',
        title = 'Shot Selection Rate',
        subtitle = '2009 - 2019') + 
  scale_x_continuous(name="NBA Seasons", limits=c(2009, 2021)) +
  scale_y_continuous(name="Shooting Rate", limits=c(0.10, 0.52))


final_rate 


ggplot(data = bball_reference, aes(x = bball_sequence, y = rate3pt, color = "3pt Rate")) + 
   geom_smooth() + xlim(2011,2025) + ylim(0.2,0.5)












