#Using the webpage on model evaluation (https://www.r-bloggers.com/evaluating-logistic-regression-models/)

library(caret)
library(tidyverse)

shot_logs <- shot_logs %>% rename(
  "touchtime" = touchtime1, 
  "dribble" = dribbe
)

#Add late dummy variables cuz I forgot
shot_logs$CRUNCH <- ifelse( shot_logs$PERIOD >=4 & shot_logs$GAME_CLOCK <=1 & shot_logs$FINAL_MARGIN <=5,1,0)

#Divide into training and testing
set.seed(88)
shot_data$split <- sample.split(shot_logs$FGM, SplitRatio = 0.70)
training <- subset(shot_logs, split == TRUE)
testing <- subset(shot_logs, split == FALSE)

mod_fit <- train(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + as.factor(defender) +
                   as.factor(dribble) + as.factor(touchtime),  
                 data=training, method="glm", family="binomial")

#You could add odds ratio at the table 
exp(coef(mod_fit$finalModel))
#Ex. For every one unit increase in shotdist the odds for making the shot decrease 
#By a factor of 6.1

#Try to add players as factor
mod_fit_players <- glm(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + as.factor(defender) +
                   as.factor(dribble) + as.factor(touchtime) + as.factor(player_name), 
                 data=training,  family="binomial")



shot_logs$probplayer <- predict(mod_fit_players, shot_logs, type = "response")

summary(mod_fit_players)
testing$probplayer <- predict(mod_fit_players,testing, type = "response")

#Model Evaluation and Diagnostic
#Two separate models, one with more variables than the others

mod_fit_one <- glm(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + as.factor(defender) +
                     as.factor(dribble) + as.factor(touchtime),  
                   data=training, family="binomial")

summary(mod_fit_one)

mod_fit_two <- glm(FGM ~ SHOT_DIST +CRUNCH,
                   data=training, family="binomial")
summary(mod_fit_two)

#Goodness of Fit
'A logistic regression is said to provide a better fit to the data
if it demonstrates an improvement over a model with fewer predictors.'

library(lmtest)
lrtest(mod_fit_one, mod_fit_two)
lrtest(final, final_reduced)

'Model two has a lower log likelihood and the difference is statistically significant'


'Pseudo R squared'

library(pscl)
pR2(mod_fit_one)  # look for 'McFadden' = 0.5452689147
pR2(mod_fit_two)  ' = 0.3332987281'
'Look at pdf for explanation, values of .2 to .4 represent an excellent fit'
'the gold standard value of 1 corresponds to a situation where we can predict
whether a given subject will have Y=0 or Y=1 with almost 100% certainty.'
'the proportion of the total variability of the outcome that is accounted for by the model'
'see p. 34-35-36 of pdf'
pR2(mod_fit_players) # 0.6136119336

library(rcompanion)

nagelkerke(mod_fit_one)
nagelkerke(mod_fit_players)

anova(mod_fit_one)

'Homer-Lemeshow Test'
library(MKmisc)
HLgof.test(fit = fitted(mod_fit_one), obs = training$FGM)
library(ResourceSelection)
hoslem.test(training$FGM, fitted(mod_fit_one), g=3)

'Using Homer idea, take all probabilities over chosen, and see how many makes out of total, it should match'
homer <- function(threshold) {
  subset10 <- subset(shot_logs, probs > threshold)
  rate <- sum(subset10$FGM)/sum(subset10$attempts)
  return(rate)}
homer(0.40)
hom <- seq(0,100, by = 1)

i <- 0
j <- 1
while (i <1.01) { 
  hom[j] <- homer(i)
  j <- j+1
  i <- i + 0.01
}

homer.set <- data.frame(prec,hom)

homer_plot <- ggplot(homer.set, aes(prec, hom)) + geom_smooth() + xlab('Probability Threshold')+
  ylab('Makes percentage')

'This test doesnt work because peobabilities range from 0.08 to 0.80'
'the inverse may work tho, ask'

summary(shot_logs$probs)

#Wald Test
'A wald test is used to evaluate the statistical significance of each coefficient'
library(survey)

regTermTest(mod_fit_one, "SHOT_DIST") #hull hyp that removing it does't chenge model rejected
regTermTest(mod_fit_one, "CRUNCH")


#Variable Importance
varImp(mod_fit_one)

#Validation of Predicted Values
testing$probs <- predict(mod_fit, testing)
'see other document'

#ROC curve

library(pROC)
f1 = roc(FGM ~ SHOT_DIST, data=training) 
plot(f1, col="red")
auc(f1)
library(ROCR)
prob <- testing$probplayer
pred <- prediction(prob, testing$FGM)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc



#K cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(FGM ~ SHOT_DIST + CRUNCH + LATE_SHOT + as.factor(defender) +
                   as.factor(dribble) + as.factor(touchtime),  
                 data=testing, method="glm", family="binomial", trControl = ctrl,
                 tuneLength = 5)

pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred,testing$FGM)
table(testing$FGM, testing$probs > 0.70)

roc.test(testing$FGM, testing$probs,testing$probplayer)

ggplot(data = shot_logs, aes(probs,probplayer)) + geom_smooth()
mean(shot_logs$probs-shot_logs$probplayer)
mean(shot_logs$probs)
mean(shot_logs$probplayer)
