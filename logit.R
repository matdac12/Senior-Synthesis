library(lubridate)
period_to_seconds(hms("12:12:54"))
shot_data$GAME_CLOCK<- period_to_seconds(shot_data$GAME_CLOCK)

#Some of the shot clock data was negative or greater than 24 
shot_data <- subset(shot_data, TOUCH_TIME > 0 & TOUCH_TIME < 24 & CLOSE_DEF_DIST < 25)
summary(shot_data)

shot_data$CLOSE <- ifelse(abs(shot_data$FINAL_MARGIN) < 6,1,0)
shot_data$LATE <- ifelse(shot_data$PERIOD > 3 & shot_data$GAME_CLOCK < 3600 , 1,0)
shot_data$LATE_SHOT <- ifelse(shot_data$SHOT_CLOCK <7, 1,0)
shot_data$wide_open <- ifelse(shot_data$CLOSE_DEF_DIST > 10, 1,0)
shot_data$open <- ifelse(shot_data$CLOSE_DEF_DIST<11 & shot_data$CLOSE_DEF_DIST>6,1,0)
shot_data$contested <- ifelse(shot_data$CLOSE_DEF_DIST<7 & shot_data$CLOSE_DEF_DIST>2,1,0)
shot_data$forced <- ifelse(shot_data$CLOSE_DEF_DIST<3 & shot_data$CLOSE_DEF_DIST>0,1,0)
shot_data$dribble_0 <- ifelse(shot_data$DRIBBLES == 0, 1,0)
shot_data$dribble_12 <- ifelse(shot_data$DRIBBLES > 0 & shot_data$DRIBBLES < 3 , 1,0)
shot_data$dribble_3more <-  ifelse(shot_data$DRIBBLES > 2 , 1,0)
shot_data$touch_0 <- ifelse(shot_data$TOUCH_TIME <1.1 ,1,0)
shot_data$touch_13 <- ifelse(shot_data$TOUCH_TIME >1 & shot_data$TOUCH_TIME <3.1  ,1,0)
shot_data$touch_3more <- ifelse(shot_data$TOUCH_TIME >3  ,1,0)


#Factorize
shot_data$dribbe <- ifelse(shot_data$DRIBBLES == 0, 1, ifelse(shot_data$DRIBBLES > 0 & shot_data$DRIBBLES < 3, 2,ifelse(shot_data$DRIBBLES > 2 , 3,0)))

shot_data$touchtime <- ifelse(shot_data$TOUCH_TIME <1.1, 1, ifelse(shot_data$TOUCH_TIME > 1 & shot_data$TOUCH_TIME < 3.1, 2,ifelse(shot_data$TOUCH_TIME > 3 , 3,0)))

shot_data$touchtime1 <- ifelse(shot_data$TOUCH_TIME <1.1, 1,
                               ifelse(shot_data$TOUCH_TIME > 1 & shot_data$TOUCH_TIME < 3.1, 2,
                                      ifelse(shot_data$TOUCH_TIME > 3 & shot_data$TOUCH_TIME < 10.1 , 3,
                                            ifelse(shot_data$TOUCH_TIME > 10,4,0))))
#Consider adding more intervals 
shot_data$defender <- ifelse(shot_data$CLOSE_DEF_DIST >10, 1,
                             ifelse(shot_data$CLOSE_DEF_DIST<11 & shot_data$CLOSE_DEF_DIST>6, 2,
                                    ifelse(shot_data$CLOSE_DEF_DIST < 7 & shot_data$CLOSE_DEF_DIST>2, 3,
                                           ifelse(shot_data$CLOSE_DEF_DIST<3 & shot_data$CLOSE_DEF_DIST>-1,4,0))))
LATES <- factor(shot_data$LATE)

install.packages("effects") # only need to do once. 
library(effects)
summary(shot_data$CLOSE_DEF_DIST)

#Estimate all variables ( no factors, just raw variables) reference for closest defender -> forced shot; reference for dribbles -> 0 DRIBBLES
#reference for touch time -> less than 1 second

LogitModel = glm(FGM ~ SHOT_DIST + CLOSE  + LATE + LATE_SHOT + wide_open + open + contested 
                   + dribble_12 + dribble_3more + 
                   touch_13 + touch_3more, data = shot_data, 
                 family = "binomial")
summary(LogitModel)
plot(allEffects(LogitModel))

#This has same variables I had on other file. Also, similar results 
LogitModel_1 = glm(FGM ~ SHOT_DIST + CLOSE + LATE , data = shot_data, 
                 family = "binomial")
summary(LogitModel_1)

levels(dribbles)

#Factor Stuff
dribbles <-factor(shot_data$dribbe)
touchtimes <- factor(shot_data$touchtime)
touchtimes1 <- factor(shot_data$touchtime1)
defenders <- factor(shot_data$defender)

summary(dribbles)

LogitModel_5 = glm(FGM ~ SHOT_DIST  + dribbles + touchtimes , data = shot_data, 
                   family = "binomial")
summary(LogitModel_5)
plot(allEffects(LogitModel_5))


#This is the final model with all the variabes factorized 
LogitModel_final = glm(FGM ~ SHOT_DIST + CLOSE + LATE + LATE_SHOT + defenders + dribbles + touchtimes1 , data = shot_data, 
                   family = "binomial")
summary(LogitModel_final)
plot(allEffects(LogitModel_final))

library(stargazer)
stargazer(LogitModel_final,
          type="html",
          out="LogitModelFinal.doc")

#this is the second option for dribble, if you want the first one run the code on top
shot_data$dribbe <- ifelse(shot_data$DRIBBLES ==0, 1,
                           ifelse(shot_data$DRIBBLES > 0 & shot_data$DRIBBLES <3, 2,
                                  ifelse(shot_data$DRIBBLES > 2 & shot_data$DRIBBLES <6, 3,
                                         ifelse(shot_data$DRIBBLES > 5,4,0))))

summary(dribbles)

#Models with single parameters to see effects 
LogitModel_dribbles = glm(FGM ~ SHOT_DIST  + dribbles  , data = shot_data, 
                   family = "binomial")
summary(LogitModel_dribbles)
plot(allEffects(LogitModel_dribbles))

#Different intervals for dribble
LogitModel_dribbles1 = glm(FGM ~ SHOT_DIST  + dribbles  , data = shot_data, 
                          family = "binomial")
summary(LogitModel_dribbles1)
plot(allEffects(LogitModel_dribbles1))

LogitModel_touchtime = glm(FGM ~ SHOT_DIST + touchtimes , data = shot_data, 
                   family = "binomial")
summary(LogitModel_touchtime)
plot(allEffects(LogitModel_touchtime))

#This one has more intervals for touchtime
LogitModel_touchtime1 = glm(FGM ~ SHOT_DIST + touchtimes1 , data = shot_data, 
                           family = "binomial")
summary(LogitModel_touchtime1)
plot(allEffects(LogitModel_touchtime1))


LogitModel_defenders = glm(FGM ~ SHOT_DIST + defenders , data = shot_data, 
                          family = "binomial")
summary(LogitModel_defenders)
plot(allEffects(LogitModel_defenders))
#End of single parameters
anova(LogitModel_final)

install.packages("ROCR") # only need to do once. 
library(pscl)
pR2(LogitModel_final)





#For meeting
#Show final model 

summary(LogitModel_final)


#Show individual effects 

install.packages("effects") # only need to do once. 
library(effects)

#Defenders 

plot(allEffects(LogitModel_defenders))


#TouchTime

plot(allEffects(LogitModel_touchtime1))


#Dribbles 


plot(allEffects(LogitModel_dribbles1))





