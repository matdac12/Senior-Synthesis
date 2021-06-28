library(ModelMetrics)
preds <- predict(Logit_time_close, type = 'response')
logLoss(Logit_time_close, season2018_2019$MADE, preds, distribution = 'binomial')

regrmodel = glm (MADE ~ shot_distance, data = season2018_2019)
summary(regrmodel)

season2018_2019$prob <- predict(regrmodel, season2018_2019, type = "response")

ggplot(season2018_2019, aes(shot_distance, prob)) + geom_smooth() + xlim(0,40)
