##NFL Model


library(RSQLite)
library(dplyr)
library(rvest)
library(httr)
library(data.table)

url <- "http://www.pro-football-reference.com/years/2017/games.htm"
set_config( config( ssl_verifypeer = 0L ))
games <- GET(url) %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="games"]') %>%
  html_table()
games <- games[[1]]

names(games)[5] <- "Winner.tie"
names(games)[6] <- "Home"
names(games)[7] <- "Loser.tie"
games <- games[c(-8)]
games$Season <- '2017'
games <- games[ which(games$Week != 'Week'), ]

games$Winner.tie <- ifelse(games$Winner.tie =="Los Angeles Chargers", "San Diego Chargers",games$Winner.tie)
games$Loser.tie <- ifelse(games$Loser.tie =="Los Angeles Chargers", "San Diego Chargers",games$Loser.tie)

schedule <- games[ which(games$YdsW == ''), ]
vars <- c('Season', 'Week', 'Date', 'Time', 'Winner.tie', 'Loser.tie')
schedule <- schedule[vars]
schedule <- plyr::rename(schedule, c("Winner.tie"="Away", "Loser.tie"="Home"))
schedule$id <- seq_len(nrow(schedule))

games_new <- games[ which(games$YdsW!= ''), ]
week_select <- max(as.numeric(games_new$Week))
week_select <- paste0("Week.", week_select)


filename <- "C:/Users/jmonroe/Desktop/NFL/1_Data/NFL_DB.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
games = dbGetQuery(db,'select * from games' )
games <- rbind(games, games_new)
games <- unique(games)
games$PtsW <- as.numeric(games$PtsW)
games$PtsL <- as.numeric(games$PtsL)

dbRemoveTable(db, "games")
dbWriteTable(db, "games", games)

###Season 2009####################################################################################################
data <- games[ which(games$Season=='2009'), ]

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


rank09 <- rankings
rank09$Season <- '2009'

###Season 2010####################################################################################################
rank1 <- rank09 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2010'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank10 <- rankings
rank10$Season <- '2010'


###Season 2011####################################################################################################
rank1 <- rank10 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2011'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank11 <- rankings
rank11$Season <- '2011'

###Season 2012####################################################################################################
rank1 <- rank11 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2012'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank12 <- rankings
rank12$Season <- '2012'

###Season 2013####################################################################################################
rank1 <- rank12 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2013'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank13 <- rankings
rank13$Season <- '2013'

###Season 2014####################################################################################################
rank1 <- rank13 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2014'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank14 <- rankings
rank14$Season <- '2014'

###Season 2015####################################################################################################
rank1 <- rank14 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2015'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank15 <- rankings
rank15$Season <- '2015'


###Season 2016####################################################################################################
rank1 <- rank15 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Team <- ifelse(rank1$Team=="St. Louis Rams", "Los Angeles Rams",rank1$Team)

rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2016'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank16 <- rankings
rank16$Season <- '2016'

###Season 2017##################################################################################
rank1 <- rank16 %>% select(Team,Week.23) %>% filter (Week.23 > 0) %>% arrange(-Week.23)
rank1$Week.0 <- rank1$Week.23 - (rank1$Week.23 - 1500) / 3
rank1 <- rank1 %>% select(Team,Week.0)


data2 <- games[ which(games$Season=='2017'), ]

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
week.data <- data2[data2$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:24) {
  week.no <- j
  k_factor <- 20
  week.data <- data2[data2$Week == week.no,]
  
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

rank17 <- rankings
rank17$Season <- '2017'

rank17[is.na(rank17)] <- 0

rank_all <- merge(rank09, rank10, all = TRUE)
rank_all <- merge(rank_all, rank11, all = TRUE)
rank_all <- merge(rank_all, rank12, all = TRUE)
rank_all <- merge(rank_all, rank13, all = TRUE)
rank_all <- merge(rank_all, rank14, all = TRUE)
rank_all <- merge(rank_all, rank15, all = TRUE)
rank_all <- merge(rank_all, rank16, all = TRUE)
rank_all <- merge(rank_all, rank17, all = TRUE)
elo <- rank_all[order(rank_all$Season, rank_all$Team),]

dbWriteTable(db, "elo", elo, overwrite = TRUE)



#Select the latest week's data
max_week <- max(elo$Season)
elo_base  <- elo %>% subset(Season==max_week)
vars <- c("Team", week_select)
elo_base <- elo_base[vars]


##NFL_Injuries
page <- read_html("http://espn.go.com/nfl/injuries")

injured <- page %>%
  html_nodes(".tablehead a") %>%
  html_text() 

filename <- "C:/Users/jmonroe/Desktop/NFL/1_Data/NFL_DB.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
teams = dbGetQuery(db,'select * from teams' )
madden = dbGetQuery(db,'select * from madden' )
hta = dbGetQuery(db,'select * from stadium_dist')
positions = dbGetQuery(db,'select * from positions' )
madden$Overall <- as.numeric(madden$Overall)
madden <- madden %>% subset(Season=="2017") %>%
  select(Team, Last.Name, First.Name, Position, Overall)
madden$Name <- paste(madden$First.Name, madden$Last.Name, sep = " ")
madden <- madden[!(madden$Name %in% injured), ]

positions$Rank <- as.numeric(positions$Rank)

#positions$Weight <- 1/positions$Rank / sum(1/positions$Rank)
positions$Weight <- positions$Rank

madden <- merge(x = madden, y = positions, by = "Position", all = TRUE)
madden$Player_Weight <- madden$Overall * madden$Weight
madden <- data.table(madden)

p_rank <- madden

madden <- madden[ , max(Player_Weight), by = list(Position, Team)]
madden <- madden[ , sum(V1), by = list(Team)]
madden <- plyr::rename(madden, c("V1"="Weight"))

madden_mean <- mean(madden$Weight)
madden$Weight <- madden$Weight / madden_mean * 1500

madden <- merge(x = madden, y = teams, by.x = c("Team"), by.y = c("Madden"), all = TRUE)

madden <- madden %>% select(ELO, Weight) %>% arrange(ELO)
elo_final <- merge(x = elo_base, y = madden, by.x = c("Team"), by.y = c("ELO"), all = TRUE)
names(elo_final)[2] <- "Weight1"
names(elo_final)[3] <- "Weight2"

elo_final$ELO <- (elo_final$Weight1 + elo_final$Weight2) / 2

elo_final <- elo_final %>% select(Team, ELO) %>% arrange(Team)

##Read Next Week's Schedule
sched <- merge(schedule, elo_final, by.x = c("Home"), by.y = c("Team"))
sched <- plyr::rename(sched, c("ELO" = "Home_ELO"))
sched <- merge(sched, elo_final, by.x = c("Away"), by.y = c("Team"))
sched <- plyr::rename(sched, c("ELO" = "Away_ELO"))

##Home Team Advantage, based on Distance from script stadium_distance.R
sched <- merge(sched,hta,by=c("Home","Away"))
sched$Home_ELO <- sched$Home_ELO + sched$HTA

# Pr(A) = 1 / (10^(-ELODIFF/400) + 1) http://fivethirtyeight.com/datalab/introducing-nfl-elo-ratings/
sched$Home_Pr <- (1 / (10^(-(sched$Home_ELO - sched$Away_ELO)/400) + 1))
sched$id <- as.numeric(sched$id)
sched <- sched[order(sched$id),]
sched$Spread <- round(abs((sched$Home_ELO - sched$Away_ELO) / 25),0)
model <- sched %>% select(Season, Week, Date, Away, Home, Away_ELO, Home_ELO, Home_Pr, Spread)

dbWriteTable(db, "model", model, overwrite = TRUE)













