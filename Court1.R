library(ggplot2)

#For final results, see Final_Stuff file

#Code for plotting a basketball court. Call final result with halfP    (long - do not open)

# Need a function to draw circle
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}

# Need symmetry
rev_y <- function(y) 94-y

# Create data frame containing coordinates of polygons
new_coords <- function(x, y, group, descri){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group <- group
  new_coords_df$side <- 1
  group <- group + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = x, y = rev_y(y))
  new_coords_df2$group <- group
  new_coords_df2$side <- 2
  group <<- group + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  
  return(new_coords_df)
}

#Circles we need 
# Restricted area
cercle_np_out <- circle_fun(center = c(25,5+3/12), diameter = (4+1/6)*2)
cercle_np_in <- circle_fun(center = c(25,5+3/12), diameter = 4*2)
# Three point
cercle_3pts_out <- circle_fun(center = c(25,5+3/12), diameter = (23+9/12)*2)
cercle_3pts_in <- circle_fun(center = c(25,5+3/12), diameter = (23+7/12)*2)
# Hoop
cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# Free Throws
cercle_lf_out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf_in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
# Center Circle
cercle_mil_out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mil_in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)
# Small Center Circle
cercle_mil_petit_out <- circle_fun(center = c(25,47), diameter = 2*2)
cercle_mil_petit_in <- circle_fun(center = c(25,47), diameter = (2-1/6)*2)


#We need to assign the first value of the variable group. Then, each use of new_coords increments group value by one.

group <- 1
court <- new_coords(c(0-1/6,0-1/6,50 + 1/6,50 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
court <- rbind(court, new_coords(x = c(0,0,3,3), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur gauche"))
court <- rbind(court, new_coords(x = c(47,47,50,50), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
court <- rbind(court, new_coords(x = c(3,3,3+1/6,3+1/6), y = c(0,14,14,0), group = group, descri = "3pts bas gauche"))
court <- rbind(court, new_coords(x = c(47-1/6,47-1/6,47,47), y = c(0,14,14,0), group = group, descri = "3pts bas droit"))
court <- rbind(court, new_coords(x = c(17,17,17+1/6,17+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
court <- rbind(court, new_coords(x = c(33-1/6,33-1/6,33,33), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
court <- rbind(court, new_coords(x = c(17,17,33,33), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
court <- rbind(court, new_coords(x = c(14-1/6,14-1/6,14,14), y = c(0,1/2,1/2,0), group = group, descri = "marque fond gauche"))
court <- rbind(court, new_coords(x = c(36,36,36+1/6,36+1/6), y = c(0,1/2,1/2,0), group = group, descri = "marque fond droit"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF gauche interieur"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF droite interieur"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
court <- rbind(court, new_coords(x = c(cercle_3pts_out[31:220,"x"], rev(cercle_3pts_in[31:220,"x"])),
                                 y = c(cercle_3pts_out[31:220,"y"], rev(cercle_3pts_in[31:220,"y"])), group = group, descri = "cercle 3pts"))
court <- rbind(court, new_coords(x = c(cercle_np_out[1:250,"x"], rev(cercle_np_in[1:250,"x"])),
                                 y = c(cercle_np_out[1:250,"y"], rev(cercle_np_in[1:250,"y"])), group = group, descri = "cercle non passage en force"))
court <- rbind(court, new_coords(x = c(20+1/6,20+1/6,20+8/12,20+8/12), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas gauche cercle LF"))
court <- rbind(court, new_coords(x = c(30-8/12,30-8/12,30-1/6,30-1/6), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas droite cercle LF"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[1:250,"x"], rev(cercle_lf_in[1:250,"x"])),
                                 y = c(cercle_lf_out[1:250,"y"], rev(cercle_lf_in[1:250,"y"])), group = group, descri = "cercle LF haut"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[250:269,"x"], rev(cercle_lf_in[250:269,"x"])),
                                 y = c(cercle_lf_out[250:269,"y"], rev(cercle_lf_in[250:269,"y"])), group = group, descri = "cercle LF partie 1"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[288:308,"x"], rev(cercle_lf_in[288:308,"x"])),
                                 y = c(cercle_lf_out[288:308,"y"], rev(cercle_lf_in[288:308,"y"])), group = group, descri = "cercle LF partie 2"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[327:346,"x"], rev(cercle_lf_in[327:346,"x"])),
                                 y = c(cercle_lf_out[327:346,"y"], rev(cercle_lf_in[327:346,"y"])), group = group, descri = "cercle LF partie 3"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[365:385,"x"], rev(cercle_lf_in[365:385,"x"])),
                                 y = c(cercle_lf_out[365:385,"y"], rev(cercle_lf_in[365:385,"y"])), group = group, descri = "cercle LF partie 4"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[404:423,"x"], rev(cercle_lf_in[404:423,"x"])),
                                 y = c(cercle_lf_out[404:423,"y"], rev(cercle_lf_in[404:423,"y"])), group = group, descri = "cercle LF partie 5"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[442:462,"x"], rev(cercle_lf_in[442:462,"x"])),
                                 y = c(cercle_lf_out[442:462,"y"], rev(cercle_lf_in[442:462,"y"])), group = group, descri = "cercle LF partie 6"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[481:500,"x"], rev(cercle_lf_in[481:500,"x"])),
                                 y = c(cercle_lf_out[481:500,"y"], rev(cercle_lf_in[481:500,"y"])), group = group, descri = "cercle LF partie 7"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF gauche"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF droite"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
court <- rbind(court, new_coords(x = c(cercle_mil_out[250:500,"x"], rev(cercle_mil_in[250:500,"x"])),
                                 y = c(cercle_mil_out[250:500,"y"], rev(cercle_mil_in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
court <- rbind(court, new_coords(x = c(cercle_mil_petit_out[250:500,"x"], rev(cercle_mil_petit_in[250:500,"x"])),
                                 y = c(cercle_mil_petit_out[250:500,"y"], rev(cercle_mil_petit_in[250:500,"y"])), group = group, descri = "cercle milieu petit"))
court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))



#Create the graph 

library(ggplot2)
P <- ggplot() + geom_polygon(data = court, aes(x = x, y = y, group = group), col = "black") +
  coord_equal() +
  ylim(-2,96) +
  xlim(-5,55) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  scale_y_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank()
  )

P


halfP <- P + coord_cartesian(ylim = c(-2, 44.7))

halfP 


#Initial tries for season plotting. if needed use function generate_chart. DONT OPEN

plot2009_2010 <- halfP + stat_bin_hex(data = onlymade2009_2010, aes(x =onlymade2009_2010$shot_x ,
                                                    y =onlymade2009_2010$shot_y, ),
                   binwidth = 0.1, alpha = .8) + ggtitle("Season 09/10 Make Shots ")

plot2010_2011 <- halfP + stat_bin_hex(data = onlymade2010_2011, aes(x =onlymade2010_2011$shot_x ,
                                                                  y =onlymade2010_2011$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 10/11 Make Shots ")
plot2011_2012 <- halfP + geom_bin2d(data = onlymade2011_2012, aes(x =onlymade2011_2012$shot_x ,
                                                                  y =onlymade2011_2012$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 11/12 Make Shots ")
plot2012_2013 <- halfP + geom_bin2d(data = onlymade2012_2013, aes(x =onlymade2012_2013$shot_x ,
                                                                  y =onlymade2012_2013$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 12/13 Make Shots ")
plot2013_2014 <- halfP + geom_bin2d(data = onlymade2013_2014, aes(x =onlymade2013_2014$shot_x ,
                                                                  y =onlymade2013_2014$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 13/14 Make Shots ")
plot2014_2015 <- halfP + geom_bin2d(data = onlymade2014_2015, aes(x =onlymade2014_2015$shot_x ,
                                                                  y =onlymade2014_2015$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 14/15 Make Shots ")
plot2015_2016 <- halfP + geom_bin2d(data = onlymade2015_2016, aes(x =onlymade2015_2016$shot_x ,
                                                                  y =onlymade2015_2016$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 15/16 Make Shots ")
plot2016_2017 <- halfP + geom_bin2d(data = onlymade2016_2017, aes(x =onlymade2016_2017$shot_x ,
                                                                  y =onlymade2016_2017$shot_y),
                                    binwidth = 0.1, alpha = .8) +  ggtitle("Season 16/17 Make Shots ")
plot2017_2018 <- halfP + geom_bin2d(data =season2017_2018, aes(x =season2017_2018$shot_x ,
                                                                  y =season2017_2018$shot_y, color = season2017_2018$result),
                                    binwidth = 0.1, alpha = .3) +  ggtitle("Season 17/18 Make Shots ")
plot2018_2019 <- halfP + stat_bin_hex(data = onlymade2018_2019, aes(x =onlymade2018_2019$shot_x ,
                                                                  y =onlymade2018_2019$shot_y,
                                                                  fill = stat(cut(log(count),
                                                                                  breaks = log(c(10,50, 250, 500, Inf)),))),
                                    binwidth = 1, alpha = 0.4, show.legend = NA,  colour = "lightblue") +
  scale_fill_manual(values = c("blue", "yellow", "red" , "black"), #labels = c("a","s","w","q"), name = "Count")+
  )+ggtitle("Season 18/19 Make Shots ") 
 

#Initial code for plotting made field goals. Don't need
halfP + geom_hex(data = onlymade2018_2019, aes(x =onlymade2018_2019$shot_x ,
                                                        y =onlymade2018_2019$shot_y,
                                                        fill = cut(..count.., c(
                                                          0,5,25, 50, 100, Inf))),
                          colour = "lightblue",  binwidth = 1, alpha = 0.75) +
  scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), labels = c("0-5","5-25","25-50","50-100","100+"), name = "Count")+
  ggtitle("Season 18/19 Make Shots ") 



#Show this - difference in 10 seasons

grid.arrange(generate_chart(z18_19),generate_chart(z09_10), nrow=2, ncol=1)


#Create function that returns plot of selected season. Example: generate_chart(z18_19)
generate_chart = function(season) {
  halfP + geom_hex(data = season, aes(x =season$shot_x ,
                                                 y =season$shot_y,
                                                 fill = cut(..count.., c(
                                                   0,5,25, 50, 100, Inf))),
                   colour = "lightblue",  binwidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                      labels = c("0-5","5-25","25-50","50-100","100+"), name = "Count")+
    ggtitle(" Made Shots Season ") 
}


generate_chart(z18_19)
#End

library(tidyverse)

#Initial function made for just season 18-19
generate_player_chart <- function(name) {
  
  
  zz <- subset(z18_19, player == name)
 plot<-  halfP + geom_hex(data = zz, aes(x =zz$shot_x ,
                                                y =zz$shot_y,
                                                fill = cut(..count.., c(
                                                  0,1,2, 5, 10, Inf))),
                            colour = "lightblue",  binwidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                      labels = c("1","2","2-5","5-10","11+"), name = "Count")+
    ggtitle(paste(name,"Total Shots"))
  return(plot)
}

#End

#Player Funtion, with parameters name and season. example : player_chart("Kevin Durant", z18_19)
player_chart <- function(name, whatseason) {
  
  
  zz <- subset(whatseason, player == name)
  plot<-  halfP + geom_hex(data = zz, aes(x =shot_x ,
                                          y =shot_y,
                                          fill = cut(..count.., c(
                                            0,1,2, 5, 10, Inf))),
                           colour = "lightblue",  binwidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                      labels = c("1","2","2-5","5-10","11+"), name = "Count")+
    ggtitle(paste(name,"Total Shots"))
  return(plot)
}


player_chart("Kevin Durant", z18_19_made)

grid.arrange(player_chart("James Harden", z18_19_made), player_chart("James Harden", z16_17_made), player_chart("James Harden", z14_15_made)
             , player_chart("James Harden", z12_13_made),
             nrow=2, ncol=2)


#End


#Team function, with parameters team and season. 

team_chart <- function(name, whatseason) {
  
  
  zz <- subset(whatseason, team == name)
  plot<-  halfP + geom_hex(data = zz, aes(x =shot_x ,
                                          y =shot_y,
                                          fill = cut(..count.., c(
                                            0,1,2, 5, 10, Inf))),
                           colour = "lightblue",  binwidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                      labels = c("1","2","2-5","5-10","11+"), name = "Count")+
    ggtitle(paste(name,"Total Shots"))
  return(plot)
}

grid.arrange(team_chart("TOR", z18_19), team_chart("BOS",z18_19),  team_chart("HOU",z18_19),  team_chart("PHI",z18_19), nrow = 2, ncol = 2)

#Function for team and player

team__player_chart <- function(whatteam, name, whatseason) {
  
  
  ss <- subset(whatseason, team == whatteam)
  zz <- subset(ss, player == name)
  plot<-  halfP + geom_hex(data = zz, aes(x =shot_x ,
                                          y =shot_y,
                                          fill = cut(..count.., c(
                                            0,1,2, 5, 10, Inf))),
                           colour = "lightblue",  binwidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                      labels = c("1","2","2-5","5-10","11+"), name = "Count")+
    ggtitle(paste(name,"Total Shots"))
  return(plot)
} 

team__player_chart("HOU", "James Harden", z18_19)


#Code for writing data set in folder for later upload

write.csv(ZZ,"ZZ.csv", row.names = FALSE)



library(dplyr)
#Get player name list
ZZ1819 <- distinct(X2018_2019_combined_stats,player, .keep_all = TRUE)
ZZ1718 <- distinct(X2017_2018_combined_stats,player, .keep_all = TRUE)
ZZ1617 <- distinct(X2016_2017_combined_stats,player, .keep_all = TRUE)
ZZ1516 <- distinct(X2015_2016_combined_stats,player, .keep_all = TRUE)
ZZ1415 <- distinct(X2014_2015_combined_stats,player, .keep_all = TRUE)
ZZ1314 <- distinct(X2013_2014_combined_stats,player, .keep_all = TRUE)
ZZ1213 <- distinct(X2012_2013_combined_stats,player, .keep_all = TRUE)
ZZ1112 <- distinct(X2011_2012_combined_stats,player, .keep_all = TRUE)
ZZ1011 <- distinct(X2010_2011_combined_stats,player, .keep_all = TRUE)
ZZ0910 <- distinct(X2009_2010_combined_stats,player, .keep_all = TRUE)

ZZ <- rbind(ZZ1819,ZZ1718,ZZ1617,ZZ1516,ZZ1415,ZZ1314,ZZ1213,ZZ1112,ZZ1011,ZZ0910 )

ZZ <- na.omit(ZZ[,c("data_set", "team", "player")])
