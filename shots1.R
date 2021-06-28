#Install psych package
library(psych)

#SSSSS <-subset(subsample1, FEMALE==0 & HISPANIC ==1)

#descstat <- na.omit(subsample1[,c("AGEP","PERNP")])

shotsdata$MADE<-ifelse(shotsdata$result=="made",1,0)

onlymade <- subset(shotsdata, MADE == 1)

coordinates <- na.omit(onlymade[,c("converted_x","converted_y")])

fromwhere <- onlymade(x= onlymade$converted_x, y= onlymade$converted_y)

#Season 2018-2019 code 
season2018_2019 <- na.omit(X2018_2019_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])

season2018_2019$MADEE<-ifelse(shotsdata$result=="made",1,0)

onlymade2018_2019 <- subset(season2018_2019, result == "made")

coordinates2018_2019 <- na.omit(onlymade2018_2019[,c("converted_x","converted_y")])

fromwhere2018_2019 <- onlymade2018_2019(x= onlymade2018_2019$converted_x, y= onlymade2018_2019$converted_y)

#Season 2009-2010 code 
season2009_2010 <- na.omit(X2009_2010_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])

onlymade2009_2010 <- subset(season2009_2010, result == "made")

coordinates2009_2010 <- na.omit(onlymade2009_2010[,c("converted_x","converted_y")])



#Season 2010-2011 through 2017-2018 

season2010_2011 <- na.omit(X2010_2011_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2011_2012 <- na.omit(X2011_2012_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2012_2013 <- na.omit(X2012_2013_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2013_2014 <- na.omit(X2013_2014_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2014_2015 <- na.omit(X2014_2015_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2015_2016 <- na.omit(X2015_2016_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2016_2017 <- na.omit(X2016_2017_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])
season2017_2018 <- na.omit(X2017_2018_combined_stats[,c("points","result","shot_distance","converted_x","converted_y")])


#Select only makes for everyseason

onlymade2009_2010 <- subset(season2009_2010, result == "made")
onlymade2010_2011 <- subset(season2010_2011, result == "made")
onlymade2011_2012 <- subset(season2011_2012, result == "made")
onlymade2012_2013 <- subset(season2012_2013, result == "made")
onlymade2013_2014 <- subset(season2013_2014, result == "made")
onlymade2014_2015 <- subset(season2014_2015, result == "made")
onlymade2015_2016 <- subset(season2015_2016, result == "made")
onlymade2016_2017 <- subset(season2016_2017, result == "made")
onlymade2017_2018 <- subset(season2017_2018, result == "made")
onlymade2018_2019 <- subset(season2018_2019, result == "made")


#Create datasets with all coordinates of last 10 seasons

