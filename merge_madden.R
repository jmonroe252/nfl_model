##Merge Madden Files

m13 <- read.csv('C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_nfl_13.csv')
m14 <- read.csv('C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_nfl_14.csv')
m15 <- read.csv('C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_nfl_15.csv')
m16 <- read.csv('C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_nfl_16.csv')
m17 <- read.csv('C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_nfl_17.csv')

m13$Season <- 2013
m14$Season <- 2014
m15$Season <- 2015
m16$Season <- 2016
m17$Season <- 2017

t1 <- merge(m13, m14, all = TRUE)
t2 <- merge(t1, m15, all = TRUE)
t3 <- merge(t2, m16, all = TRUE)
t4 <- merge(t3, m17, all = TRUE)
madden <- t4[order(t4$Season, t4$Team, t4$Last.Name),] 

write.csv(madden, 'C:/Users/jmonroe/Desktop/NFL/nfl/nfl/madden_full.csv')

