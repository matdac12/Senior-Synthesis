

final_reduced <- glm(FGM ~ SHOT_DIST + CRUNCH , 
                     data= shot_logs,  family="binomial")

final <- glm(FGM ~ SHOT_DIST + CRUNCH + factor(shotclock) + as.factor(defender) +
               as.factor(dribble)  , 
             data= shot_logs,  family="binomial")

summary(final_reduced)
summary(final)
##OCT 7th
tree <- rpart(FGM ~ SHOT_DIST + CRUNCH + factor(shotclock) + as.factor(defender) +
                as.factor(dribble)  , 
              data= shot_logs,  method = 'class', minbucket = 1000)
prp(tree)
kobe <- shot_logs %>% filter( player_name == 'kobe bryant' & probs < 0.20 & FGM ==1)



stargazer(final_reduced,final_reduced, final, final,
          type="latex",
          out="finalmodel.tex")

exp(coef(final_reduced))

pR2(final_reduced)
pR2(final)


shot_logs$probs <- predict(final, shot_logs, type = 'response')

distance <- shot_logs %>% 
  group_by(SHOT_DIST, probs) %>% summarize(N = n())


ggplot(data = distance, aes(SHOT_DIST, probs)) + geom_smooth()


#Plot Log Function
ggplot(data = data.frame( seq(0,50,0.1), log(seq(0,50,0.1))), aes(seq(0,50,0.1), log(seq(0,50,0.1)))) + geom_line(color = 'royalblue2', size = 1)+
  geom_hline(yintercept = 0, color = 'black') + 
  labs( x = 'likelihood',
        y = 'log(likelihood)', 
        title = 'Log function') + xlim(0,20)

table(shot_logs$FGM, shot_logs$probs >get_max_precision(shot_logs$SHOT_DIST))
table(qualityTrain$FGM, predictTrain >0.45)







shot_logs[sample(nrow(shot_logs),3),]


Distance <- c(10,23,4,25)
Crunch<- c(0,1,0,1)
Dribbles <- c(0,5,4,0)
Defender_Dis <- c(8,2,1,5)
Shot_Clock <- c(16,3,21,12)
Probability <- c(0.60, 0.16, 0.49, 0.37 )
Result<- c('made', 'made','missed','made')
Prediction <- c('','','','')


sample <- data.frame(Distance,Crunch,Dribbles,Defender_Dis, Shot_Clock, Result, Probability, Prediction)
sample






odribble <- shot_logs %>% 
  group_by(DRIBBLES,FGM) %>% 
  summarize(N = n()) %>% 
  mutate(perc = N/sum(N)) %>% 
  filter( FGM == 1) %>% 
  mutate( odds = perc/(1-perc),
          logodds = log(odds))
ggplot(data = odribble, aes(DRIBBLES, logodds)) + geom_smooth() +
  geom_smooth( data = shot_logs, aes(DRIBBLES, log((probs/(1-probs)))))






















