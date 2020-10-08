setwd("C:\\Users\\seong\\Desktop\\Dacon")

fifa <- read.csv("FIFA_train.csv")
head(fifa)

fifa_data <- data.frame(age = fifa$age, 
                        reputation = fifa$reputation, 
                        stat_overall = fifa$stat_overall, 
                        potential = fifa$stat_potential, 
                        skill_moves = fifa$stat_skill_moves, 
                        value=fifa$value)

head(fifa_data)                      
str(fifa_data)                        

r<- lm(value~age+reputation+stat_overall+potential+skill_moves, data=fifa_data)
summary(r)


vartest <- c('value', 'reputation', 'stat_overall', 'potential', 'skill_moves')
pairs(fifa_data[vartest], cex=1, col=as.integer(fifa_data$value))

step(r, direction="both")
pairs(fifa_data[vartest], cex=1, col=as.integer(fifa_data$value))
summary(r)

cor(fifa_data$skill_moves, fifa_data$value)
cor(fifa_data$stat_overall, fifa_data$value)
cor(fifa_data$potential, fifa_data$value)
cor(fifa_data$age, fifa_data$value)
cor(fifa_data$reputation, fifa_data$value)


t<- lm(value~stat_overall+potential+reputation, data=fifa_data)
summary(t)
step(t, direction="both")

