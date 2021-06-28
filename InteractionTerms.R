summary(glm(FGM ~ SHOT_DIST + factor(dribble)*factor(defender), data = training,  family=binomial))


mod_fit_players <- glm(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + as.factor(defender) +
                         as.factor(dribble) + as.factor(touchtime) + as.factor(player_name), 
                       data=training,  family="binomial")

summary(glm(FGM ~ SHOT_DIST + I(DRIBBLES == 0) + I(DRIBBLES == 1)+ I(DRIBBLES == 2) , data = training,  family=binomial))
summary(glm(FGM ~ SHOT_DIST + I(CLOSE_DEF_DIST == 1) + I(CLOSE_DEF_DIST == 3)+ I(CLOSE_DEF_DIST >5) , data = training,  family=binomial))
summary(glm(FGM ~ SHOT_DIST + I(CLOSE_DEF_DIST >5 & DRIBBLES == 0)+ I(CLOSE_DEF_DIST >5 & DRIBBLES == 2) + I(CLOSE_DEF_DIST >5 & DRIBBLES > 2), data = training,  family=binomial))


hist(xg_player$percentage)
library(ggplot2)

ggplot(data = xg_player, aes (x= predicted, color = 'white')) + geom_histogram(binwidth = 0.01) + xlim(0.30,0.70)
ggplot(data = xg_player, aes (x= percentage, color = 'white')) + geom_histogram(binwidth = 0.01) + xlim(0.30, 0.70)


#ADD early shot clock and made last?
shot_logs$EARLY_SHOT <- ifelse(shot_logs$SHOT_CLOCK > 17.9 & shot_logs$SHOT_CLOCK < 22 & shot_logs$GAME_CLOCK >=1,1,0)

shot_logs$shotclock <- ifelse(shot_logs$SHOT_CLOCK > 17.9,2, 
                              ifelse(shot_logs$SHOT_CLOCK > 6.1 & shot_logs$SHOT_CLOCK < 18,1,3))
                              
shot_logs %>% filter(SHOT_CLOCK >23.9 & GAME_CLOCK >1) %>% summary()
shot_logs$madelast <- shot_logs$SHOT_RESULT

shot_logs$madelast[1] = 0
for ( i in 2:119320) {
  
  shot_logs$madelast[i]<-  shot_logs$FGM[i-1]
}

for ( i in 2:119320) {
  
  shot_logs$madelast[i] <- ifelse(shot_logs$player_name[i] != shot_logs$player_name[i-1],0,shot_logs$madelast[i])
  
}
  
model2 = glm(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + EARLY_SHOT + madelast + factor(defender) +
               factor(dribble) + factor(touchtime),  
             data=shot_logs, family="binomial")
summary(model2)
exp(0.2038915)
exp(-0.00640472)
exp(0.1864991)

model3 = glm(FGM ~  madelast, data=shot_logs, family="binomial")
summary(model3)
exp(-0.025270)

corrset <- na.omit(shot_logs[,c("SHOT_DIST", "SHOT_CLOCK", "DRIBBLES", "TOUCH_TIME", "CLOSE_DEF_DIST")])
corrset <- corrset %>% 
  rename(
    'Shot Distance' = SHOT_DIST,
    'Shot Clock' = SHOT_CLOCK,
    'Dribbles' = DRIBBLES, 
    'Touch Time' = TOUCH_TIME,
    'Defender Distance' = CLOSE_DEF_DIST
  )
  
  
corrset <- corrset %>% filter(SHOT_DIST <40)
M <-cor(corrset)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), bg = 'black', tl.cex = 1, tl.col = 'gray15',
         )

model4 =  glm(FGM ~ SHOT_DIST + CRUNCH + SHOT_CLOCK + DRIBBLES  + CLOSE_DEF_DIST,  
              data=shot_logs, family="binomial")
model5 =  glm(FGM ~ SHOT_DIST + CRUNCH + SHOT_CLOCK + DRIBBLES + TOUCH_TIME + CLOSE_DEF_DIST,  
              data=shot_logs, family="binomial")

supreme = glm(FGM ~ SHOT_DIST + factor(dribble)*factor(defender), data = training,  family=binomial)

car::vif(model5)
car::vif(model4)

pR2(model5)
pR2(model4)


dribbles_touchtime_plot <- ggplot( data= shot_logs, aes(DRIBBLES, TOUCH_TIME)) + 
                           geom_point() + geom_smooth()
plot(allEffects(model4))


 ggplot(shot_logs, aes(SHOT_DIST, as.numeric(FGM))) +
  stat_smooth(method="glm", family= binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=LATE_SHOT))  +
  xlab("Shot Distance") + ylab("Pr (makes)") + facet_grid(. ~ dribble)

 logodds <- predict(model4, testing, se.fit = TRUE)$fit
linear <- data.frame(testing$SHOT_DIST,testing$DRIBBLES, logodds)
plot(testing$SHOT_DIST,predict(model4, testing, type="link"))
ggplot(data = linear, aes(testing.SHOT_DIST, logodds)) + geom_smooth(method = 'lm') + geom_point(alpha = 0.1)



model6 =  glm(FGM ~  CRUNCH + dribble, data=shot_logs, family="binomial")

ggPredict(model6,se=TRUE,interactive=TRUE,digits=3)

shotf <- factor(shot_logs$shotclock)

model7 =  glm(FGM ~  SHOT_DIST*defender, data=shot_logs, family="binomial")
model8 =  glm(FGM ~  SHOT_DIST*shotf, data=shot_logs, family="binomial")
model9 =  glm(FGM ~  SHOT_DIST*defender*dribble, data=shot_logs, family="binomial")
model10 =  glm(FGM ~  SHOT_DIST*dribble, data=shot_logs, family="binomial")
model111 =  glm(FGM ~  SHOT_DIST + CRUNCH, data=shot_logs, family="binomial")


defender_effect <- ggPredict(model7,se=TRUE,interactive=FALSE,digits=3) +
  labs( x = 'Shot Distance',
        y = 'Probability',
        title = 'Defensive Coverage Effect',
        color = 'Defender Distance') 


shotclock_effect <- ggPredict(model8,se=TRUE,interactive=FALSE,digits=3,colorAsFactor = FALSE) +
  labs( x = 'Shot Distance',
        y = 'Probability',
        title = 'Shot Clock Effect',
        color = 'Shot Clock') 

defender_dribbles_effect <- ggPredict(model9,se=TRUE,interactive=FALSE,digits=3) +
  labs( x = 'Shot Distance',
        y = 'Probability',
        title = 'Defender and Dribbles',
        color = 'Defender') 

dribble_effect <- ggPredict(model10,se=TRUE,interactive=FALSE,digits=3) +
  labs( x = 'Shot Distance',
        y = 'Probability',
        title = 'Dribles Effects',
        color = 'Dribbles') 

ggPredict(model111,se=F,interactive=FALSE,digits=3, size = 1) +
  labs( x = 'Shot Distance',
        y = 'Probability',
        title = 'CRUNCH time Effect') 

summary(model111)

model11 =  glm(FGM ~ SHOT_DIST + DRIBBLES ,  
              data=shot_logs, family="binomial")

model12 =  glm(FGM ~ SHOT_DIST + TOUCH_TIME ,  
               data=shot_logs, family="binomial")
model13 =  glm(FGM ~ SHOT_DIST + CLOSE_DEF_DIST ,  
               data=shot_logs, family="binomial")
model14 =  glm(FGM ~ SHOT_DIST + SHOT_CLOCK ,  
               data=subset(shot_logs, SHOT_CLOCK <23.8), family="binomial")
model15 =  glm(FGM ~ SHOT_DIST + dribble,  
               data=subset(shot_logs, SHOT_CLOCK <23.8), family="binomial")

image(model13, col = hcl.colors(12, "YlOrRd", alpha = 0.5, rev = FALSE), 
      xlab = 'Shot Distance', ylab = 'Closest Defender Distance', main = 'Defensive Coverage Effect')

image(model12, col = hcl.colors(12, "YlOrRd", alpha = 0.5, rev = FALSE), 
      xlab = 'Shot Distance', ylab = 'Closest Defender Distance', main = 'Defensive Coverage Effect', ylim = c(0,20))


plot(model2)
residualPlots(model2)


outlierTest(final)
influenceIndexPlot(final,id.n=3)
influencePlot(final, col="red", id.n=3)

shot_logs_sm <- shot_logs %>% select(SHOT_DIST,DRIBBLES,CLOSE_DEF_DIST,TOUCH_TIME,SHOT_CLOCK, FGM)

ggpairs(data = shot_logs_sm, aes(color = FGM),
        upper = list(continuous = wrap("density"), combo = "box_no_facet"),
        lower = list(continuous = wrap("points"), combo = wrap("dot_no_facet")))
