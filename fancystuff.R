library(ggplot2)

violin_nba <- ggplot(Xtotal, aes(x=data_set, y=shot_distance)) +  geom_violin(fill='gray83', color="darkred") +
  ylim(0,30) + 
  scale_color_brewer(palette="Dark2") + theme_classic()+
  labs( x = 'NBA Season', y = 'Shot Distance (ft)', title = 'Shot Distribution', subtitle = '2009 - 2019' ) +
  geom_hline(yintercept = 22, alpha = 0.7 , linetype = 'dotted' )
violin_nba 

 pps <- function(howfar) {
  sub <- subset(Xtotal, shot_distance ==howfar)

  percentage <- nrow(subset(sub,result == 'made'))/nrow(sub)
  
  pps <- mean(ifelse(sub$shot_distance>23, 3*percentage, 2*percentage))
  
  return(pps)
} 
pps(24)

pointps <- seq(0,35, by = 1)
distance1 <- seq(0,35, by = 1)
for ( i in 0:36) { pointps[i] <- pps(i)}

pps_frame <- data.frame(distance1, pointps)


ggplot(data= pps_frame, aes(x = distance1, y = pointps)) + 
  geom_col(color = 'white', fill = 'lightblue') + 
  labs( x = 'Shot Distance (ft)', y = 'Points Per Shot', title = 'Points Per Shot', subtitle = 'From 2009 to 2019', caption = 'NBA Play By Play' )

expected_value # compare


ggplot(data= pps_frame, aes(x = distance1, y = pointps)) + 
  geom_line(color = 'blue') + 
  geom_line(data = expected_DATA, aes(x = my_range, y = my_expectedpoints),color = 'red') +
  labs( x = 'Shot Distance (ft)', y = 'Points Per Shot', title = 'Points Per Shot', subtitle = 'From 2009 to 2019', caption = 'NBA Play By Play' )

Xtotal$attempt <-1

odds <- Xtotal %>% 
  group_by(shot_distance,result) %>% 
  summarize(N = n()) %>% 
  mutate(perc = N/sum(N)) %>% 
  filter( result == 'made') %>% 
  mutate( odds = perc/(1-perc),
          logodds = log(odds))
ggplot(data = odds, aes(shot_distance, odds))  + xlim(0,45) + geom_smooth()

ggplot() + 
  geom_line(data = random, aes(x = model_range, y = log(logitprobs/(1-logitprobs)),color = 'Expected'),  size = 1, alpha = 0.6) +
  geom_smooth(data = odds, aes(shot_distance, logodds, color = 'Real'), se=F, alpha = 0.5, method = 'lm')+
  scale_x_continuous(name="Shot Distance", limits=c(0,35)) + ylim(-2,2)+
  scale_color_manual(name = " ", 
                     values = c("Expected" = "purple", "Real" = "blue"))+ 
  labs(y = 'Log Odds',
       title = 'Log Odds of making the shot',
       subtitle = 'Expected vs Real Log Odds')



ggplot() + 
  geom_line(data = random, aes(x = model_range, y = logitprobs/(1-logitprobs),color = 'Expected'),  size = 1) +
  geom_smooth(data = odds, aes(shot_distance, odds, color = 'Real'), se=F, alpha = 0.5)+
  scale_x_continuous(name="Shot Distance", limits=c(0,80)) +
  scale_color_manual(name = " ", 
                     values = c("Expected" = "purple", "Real" = "blue"))+ 
  labs(y = 'Odds',
       title = 'Odds of making the shot',
       subtitle = 'Expected vs Real Odds') + 
  geom_vline( xintercept = 23, size = 0.5, linetype = 'dotted')+ 
  annotate( geom = 'text', x = 23.2, y = 1.25, label = '3pt Line', hjust = -0.15)




ggplot() + 
  geom_line(data = random, aes(x = model_range, y = logitprobs,color = 'Expected'),  size = 1) +
  geom_smooth(data = odds, aes(shot_distance, perc, color = 'Real'), se=F, alpha = 0.5, method = 'gam')+
  scale_x_continuous(name="Shot Distance", limits=c(0,80)) +
  scale_color_manual(name = " ", 
                     values = c("Expected" = "purple", "Real" = "blue"))+ 
  labs(y = 'Probability',
       title = 'Probility of making the shot',
       subtitle = 'Expected Probability vs Actual Percentage')


