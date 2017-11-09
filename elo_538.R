##Connect to SQL

library("RSQLite")

filename <- "C:/Users/jmonroe/Desktop/NFL/1_Data/NFL_DB.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## Some operations
dbListTables(db)
games <- dbReadTable(db,"games")





# NFL Ranking using 538's ELO Algorithm 
#
# 9/2/17
#For scraping injury data
#http://www.quantifan.com/post/105605618658/scraping-player-injury-data-with-rvest

##NEXT STEPS
#Rank players using nflscrapR(season_player_game) or by retrieving from:  http://bleacherreport.com/articles/2685170-nfl1000-final-regular-season-breakdown-of-the-leagues-top-players
#Identify importance of each position and weight accordingly.
#Sum Team's rank of players * position importance
#Develop Adder to elo based on sum teams rank
#Build Shiny App to review history and forecast


print("Clearing Workspace ...")
rm(list = ls(all = TRUE))
gc(TRUE)
gcinfo(FALSE)
#install.packages("dplyr")
require(dplyr)
sessionInfo()





#
library(openxlsx)
wb <- loadWorkbook('C:/Users/jmonroe/Desktop/NFL/games_data.xlsx')


###Season 2009
data <- readWorkbook(wb, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")
rankings$Week.0 <- 1500

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]

  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}


rankings %>% 
  print(n = 10, width = Inf)

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/09_out.csv" )


###Season 2010
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)

  
data2 <- readWorkbook(wb, sheet = 2, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/10_out.csv" )

###Season 2011
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- readWorkbook(wb, sheet = 3, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/11_out.csv" )


###Season 2012
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- readWorkbook(wb, sheet = 4, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/12_out.csv" )

###Season 2013
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- readWorkbook(wb, sheet = 5, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/13_out.csv" )

###Season 2014
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- readWorkbook(wb, sheet = 6, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/14_out.csv" )

###Season 2015
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- readWorkbook(wb, sheet = 7, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")


rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/15_out.csv" )


###Season 2016
rank1 <- rankings %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)




data2 <- readWorkbook(wb, sheet = 8, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)


#
rankings <- data_frame()
teams <- data2 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)

#print(rankings)
rankings <- rankings[-33,]

for (i in 2:25) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22","Week.23")




rankings <- merge(x = rank1, y = rankings, by = "Team", all = TRUE)

rankings <- rankings[c(-3)]

rankings <- plyr::rename(rankings, c("Week.0.x"="Week.0"))

# Iterate for each week of play
week.no <- 1
k_factor <- 20.0
week.data <- data [data$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data [data$Week == week.no,]
  
  for (i in 1:nrow(week.data)) {
    #
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    #
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    #
    # Calculate Margin of Victory Multiplier 
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- week.data$PtsW[i] - week.data$PtsL[i]
    mv_mult <- 1 #Margin For Victory Multiplier
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    #
    # Use old ELO Algorithm
    #
    w_w <- 1.0
    w_l <- 0.0
    if (pd == 0) {
      w_w <- 0.5
      w_l <- 0.5
    }
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round( old.rank.w + (k_factor * mv_mult * (w_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round( old.rank.l + (k_factor * mv_mult * (w_l - mu_ij_l)))
    #
    print (sprintf("Rank : W = %d L = %d",new.rank.w,new.rank.l))
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}

rankings <- rankings[ which(rankings$Team != 'Los Angeles Rams'), ]


rankings$Team <- ifelse(rankings$Team=='St. Louis Rams', 'Los Angeles Rams', rankings$Team)

write.csv <- write.csv(rankings, "C:/Users/jmonroe/Desktop/NFL/16_out.csv" )











# Pr(A) = 1 / (10^(-ELODIFF/400) + 1) http://fivethirtyeight.com/datalab/introducing-nfl-elo-ratings/

# Superbowl XLIX Week 20
# Packers vs Seahawks
# Colts vs Patriots
# ELO Algorithm #1
Pr_49 <- 1 / (10 ^(-(1740-1661)/400)+1) # 0.61 Seattle
Pr_Packers <- 1 / (10 ^(-(1661-1740)/400)+1) # 0.39 Green Bay
#
Pr_Colts <- 1 / (10 ^(-(1629-1463)/400)+1) # 0.72 Indianapolis
Pr_Patriots <- 1 / (10 ^(-(1463-1629)/400)+1) # 0.28 New England

# ELO Algorithm 538
Pr_Seahawks <- 1 / (10 ^(-(1695-1661)/400)+1) # 0.55
Pr_Packers <- 1 / (10 ^(-(1661-1695)/400)+1) # 0.45
#
Pr_Colts <- 1 / (10 ^(-(1602-1667)/400)+1) # 0.41
Pr_Patriots <- 1 / (10 ^(-(1667-1602)/400)+1) # 0.59