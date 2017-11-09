library(rvest)
library(tidyr)
library(ggmap)

url_stad <- 'https://en.wikipedia.org/wiki/List_of_current_National_Football_League_stadiums'
webpage <- read_html(url_stad)
stadium <- html_nodes(webpage, 'table')
stadium <- html_table(stadium, fill = TRUE)[[2]]
stadium <- plyr::rename(stadium, c("Team(s)"="Teams"))
stadium <- stadium[,c("Location", "Teams")]
stadium$Team <- ifelse(stadium$Teams=='New York GiantsNew York Jets','New York Giants',stadium$Teams)

Location <- "East Rutherford, New Jersey"
Teams <- "New York GiantsNew York Jets"
Team <- "New York Jets"
jets <- cbind(Location, Teams, Team)
stadium <- rbind(stadium, jets)
stadium <- stadium[,c("Location", "Team")]


stadium <- separate(stadium, Location, into = c("City", "State"), sep = ",")

##Split to keep within query limits
stad1 <- head(stadium, 4)
stad2 <- tail(head(stadium, 8),4)
stad3 <- tail(head(stadium, 12),4)
stad4 <- tail(head(stadium, 16),4)
stad5 <- tail(head(stadium, 20),4)
stad6 <- tail(head(stadium, 24),4)
stad7 <- tail(head(stadium, 28),4)
stad8 <- tail(head(stadium, 32),4)


geo1 <- geocode(as.character(stad1$City))
Sys.sleep(300)
geo2 <- geocode(as.character(stad2$City))
Sys.sleep(300)
geo3 <- geocode(as.character(stad3$City))
Sys.sleep(300)
geo4 <- geocode(as.character(stad4$City))
Sys.sleep(300)
geo5 <- geocode(as.character(stad5$City))
Sys.sleep(300)
geo6 <- geocode(as.character(stad6$City))
Sys.sleep(300)
geo7 <- geocode(as.character(stad7$City))
Sys.sleep(300)
geo8 <- geocode(as.character(stad8$City))

stadium_locations <- rbind(geo1, geo2, geo3, geo4, geo5, geo6, geo7, geo8)

stadium <- cbind(stadium, stadium_locations)

stadium$from_team <- stadium$Team
stadium$from_lon <- stadium$lon
stadium$from_lat <- stadium$lat

stadium <- stadium[,c("Team", "lon", "lat", "from_team", "from_lon", "from_lat")]
colnames(stadium)<-c("to_team","to_lon", "to_lat", "from_team", "from_lon", "from_lat")

from_stad <- stadium[,c("from_team", "from_lon", "from_lat")]
to_stad <- stadium[,c("to_team", "to_lon", "to_lat")]

stad_dist <- merge(x = from_stad, y = to_stad, all = TRUE)

dist <- stad_dist[,c("from_lon", "from_lat", "to_lon", "to_lat")]

library(geosphere)

dist$dist.km <- sapply(1:nrow(dist),function(i)
  spDistsN1(as.matrix(dist[i,1:2]),as.matrix(dist[i,3:4]),longlat=T))


stad_dist <- cbind(stad_dist, dist$dist.km)

stadium_dist <- stad_dist[,c("from_team", "to_team", "dist$dist.km")]
colnames(stadium_dist)<-c("Away", "Home", "Distance")

stadium_dist <- stadium_dist[ which(stadium_dist$Distance != 0), ]

dist_mean <- aggregate(stadium_dist$Distance, by=list(stadium_dist$Home), 
                    FUN=mean, na.rm=TRUE)
colnames(dist_mean)<-c("Home", "Mean")

stadium_dist <- merge(x = stadium_dist, y = dist_mean, by = 'Home', all = TRUE)
stadium_dist$HTA <- (stadium_dist$Distance/stadium_dist$Mean)*60

stadium_dist <- stadium_dist[,c("Home", "Away", "Distance", "HTA")]

stadium_dist$Home <- ifelse(stadium_dist$Home == "Los Angeles Chargers", "San Diego Chargers", stadium_dist$Home)
stadium_dist$Away <- ifelse(stadium_dist$Away == "Los Angeles Chargers", "San Diego Chargers", stadium_dist$Away)

filename <- "C:/Users/jmonroe/Desktop/NFL/1_Data/NFL_DB.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

dbWriteTable(db, "stadium_dist", stadium_dist, overwrite = TRUE)




