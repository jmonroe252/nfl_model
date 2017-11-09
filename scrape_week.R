##NFL New Week Data

library(rvest)
library(httr)

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
games <- games[ which(games$YdsW!= ''), ]

