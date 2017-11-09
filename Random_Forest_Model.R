##NFL_Logistic_Regression_Model
library('RSQLite')
library('dplyr')

filename <- "C:/Users/jmonroe/Desktop/NFL/1_Data/NFL_DB.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
games = dbGetQuery(db,'select * from games' )
elo = dbGetQuery(db,'select * from elo' )
dist = dbGetQuery(db,'select * from stadium_dist' )

elo <- reshape::melt(elo, id=c("Team","Season"))
elo$variable <- as.character(elo$variable)
elo$Week <- unlist(strsplit(elo$variable, "[.]"))


##Recode variables
games$Home <- ifelse(games$Home == "@", 1, 0)
games$YdsW <- as.numeric(games$YdsW)
games$YdsL <- as.numeric(games$YdsL)
games$TOW <- as.numeric(games$TOW)
games$TOL <- as.numeric(games$TOL)

#Check for Missing Values
library(Amelia)
missmap(games, main = "Missing values vs observed")

games$Winner.tie <- as.factor(games$Winner.tie)
contrasts(games$Winner.tie)
games$Loser.tie <- as.factor(games$Loser.tie)
contrasts(games$Loser.tie)

##Subset variables
games <- subset(games, select=Winner.tie:TOL)

train <- games[1:800,]
test <- games[801:2213,]

model <- glm(Winner.tie ~., family=binomial(link='logit'), data=train)

summary(model)

#Only TOW (Turnovers by winning team appeared significant, 
##negatively correlated, meaning the fewer the turnovers, the higher the probability of winning)


library(pscl)
pR2(model)

##McFadden @ .2297,  == not very good model








