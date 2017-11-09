#library(devtools)

#devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)


playerstats16 <- season_player_game(2016)







data(nflteams)
ten_abbr <- filter(nflteams, TeamName == "Tennessee Titans")
                           
                           # Load the Titans Roster from 2013
                           tenroster2013 <- season_rosters(Season = 2013, TeamInt = ten_abbr)
                           
                           head(tenroster2013)



##Explore
# Downlaod the game data
superbowl47 <- game_play_by_play(GameID = 2013020300)

# Explore dataframe dimensions
dim(superbowl47)  

#We see that Superbowl XLVII had r nrow(superbowl47) plays. Now we will explore whether one team dominated offensive possession over another:
  
# Counting Offensive Plays using dplyr
  suppressMessages(library(dplyr))
superbowl47 %>% group_by(posteam) %>% summarise(offensiveplays = n()) %>%
  filter(., posteam != "")

# Loading the ggplot2 library
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))

# Using dplyr and knitr to find statistics 
sb_team_summary_stats <- superbowl47 %>% group_by(posteam) %>% 
  summarise(offensiveplays = n(), 
            avg.yards.gained = mean(Yards.Gained, 
                                    na.rm = TRUE),
            pointsperplay = max(PosTeamScore) / n(),
            playduration = mean(PlayTimeDiff)) %>%
  filter(., posteam != "") %>% 
  as.data.frame() 

# Yards per play plot
plot_yards <- ggplot(sb_team_summary_stats, aes(x = posteam, y = avg.yards.gained)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = avg.yards.gained + .3, 
                 label = round(avg.yards.gained,2)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Yards per Play by Team",
       x = "Teams", y = "Average Yards per Play") +
  scale_fill_manual(values = c("#241773", "#B3995D"))

# Points per play plot
plot_points <- ggplot(sb_team_summary_stats, aes(x = posteam, y = pointsperplay)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = pointsperplay + .05, 
                 label = round(pointsperplay,5)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Points per Play by Team",
       x = "Teams", y = "Points per Play") +
  scale_fill_manual(values = c("#241773", "#B3995D"))

# Play duration plot
plot_time <- ggplot(sb_team_summary_stats, aes(x = posteam, y = playduration)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = playduration + .05, 
                 label = round(playduration,2)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Average Play Time Duration \n by Team",
       x = "Teams", y = "Average Play Duration") +
  scale_fill_manual(values = c("#241773", "#B3995D"))


grid.arrange(plot_yards, plot_points, plot_time, ncol =2)


data(playerstats09, playerstats10, playerstats11, playerstats12,
     playerstats13, playerstats14, playerstats15)

allplayerstats <- rbind(playerstats09, playerstats10, playerstats11,
                        playerstats12, playerstats13, playerstats14,
                        playerstats15)


# Loading the data with season_play_by_play function: (Note the
# season_play_by_play function takes a few minutes to run)

pbp_2009 <- season_play_by_play(2009)
pbp_2010 <- season_play_by_play(2010)
pbp_2011 <- season_play_by_play(2011)
pbp_2012 <- season_play_by_play(2012)
pbp_2013 <- season_play_by_play(2013)
pbp_2014 <- season_play_by_play(2014)
pbp_2015 <- season_play_by_play(2015)
pbp_2016 <- season_play_by_play(2016)

# Stack the datasets together: (Load the tidyverse first - as if you didn't
# already...)

library(tidyverse)
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> complete(): tidyr, RCurl
#> filter():   dplyr, stats
#> lag():      dplyr, stats

pbp_data <- bind_rows(pbp_2009, pbp_2010, pbp_2011, pbp_2012, pbp_2013, pbp_2014, 
                      pbp_2015)

write.csv(pbp_data, 'C:/Users/Jeff/Desktop/nfl1.csv')

# Now filter down to only passing attempts, group by the season and passer,
# then calculate the number of passing attempts, total expected points added
# (EPA), EPA per attempt, then finally filter to only those with at least 50
# pass attempts:

passing_stats <- pbp_data %>% filter(PassAttempt == 1 & PlayType != "No Play" & 
                                       !is.na(Passer)) %>% group_by(Season, Passer) %>% summarise(Attempts = n(), 
                                                                                                  Total_EPA = sum(EPA, na.rm = TRUE), EPA_per_Att = Total_EPA/Attempts) %>% 
  filter(Attempts >= 50)

# Using the ggjoy package (install with the commented out code below) can
# compare the EPA per Pass Attempt for each NFL season:
library(ggplot2)
# install.packages('ggjoy')
library(ggjoy)

ggplot(passing_stats, aes(x = EPA_per_Att, y = as.factor(Season))) + geom_joy(scale = 3, 
                                                                              rel_min_height = 0.01) + theme_joy() + ylab("Season") + xlab("EPA per Pass Attempt") + 
  scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(expand = c(0.01, 
                                                                        0)) + ggtitle("The Shifting Distribution of EPA per Pass Attempt") + theme(plot.title = element_text(hjust = 0.5, 
                                                                                                                                                                             size = 16), axis.title = element_text(size = 16), axis.text = element_text(size = 16))
#> Picking joint bandwidth of 0.0603