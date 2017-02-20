######################
## MEDIUM POST
## Data from 19 Feb 2017
## Code for Analysis and Viz
## See scrape and clean.R for data collection
##
## By Alex Freeman
######################

## Data Visualizations

library(tidyverse)
library(data.table)
setwd("~/R/Subterfuge/Medium 19 Feb 17")

## Read in Data

sub_full <- readRDS("~/R/Subterfuge/Medium 19 Feb 17/2017-02-19 sub.rds")

## Who has rating of <600?
sub_full[sub_full$rank[sub_full$rating < 600],]
## Name: Check my ELO
## http://forums.subterfuge-game.com/viewtopic.php?f=5&t=2424

sub <- filter(sub_full, rating > 600) ## REMOVE 1 player under rating of 600
rm(sub_full)

##### SECTION 1: EXPLORATORY DATA ANALYSIS

sub[sub$rank[sub$player == "Baer"],]

## Graph of players when they joined

ggplot(sub, aes(x = joined)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "Dates Players Joined Subterfuge",
       subtitle = "Subterfuge data from 19 Feb 2017",
       x = "Date Joined",
       y = "Number of Players",
       caption = "note: Subterfuge was publicly released on 15 Oct 2015") +
  scale_x_date(date_breaks = "12 weeks",
               limits = c(as.Date("2015-10-01"), Sys.Date())) +
  annotate("segment", x = as.Date("2015-10-15"),
           xend = as.Date("2015-10-15"),
           y = 0, yend = 380, color = "red") +
  theme_light()

ggsave(paste(Sys.Date(),"1A Sub Date Joined.png"))

## Graph of games played per Player

ggplot(sub, aes(x = rated_games)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "Distribution of Rated Games Played per Player",
       subtitle = "Subterfuge data from 19 Feb 2017",
       x = "Total Rated Games Played",
       y = "Number of Players",
       caption = "note: 80 observations greater than 45 removed") +
  scale_x_continuous(limits = c(0,45)) +
  theme_light()

ggsave(paste(Sys.Date(),"1B Sub Games per Player.png"))

## Graph of Games Entered per Week

ggplot(sub, aes(x = game_rate)) + 
  geom_histogram(binwidth = .01) + 
  labs(title = "Distribution of Rated Games Entered per Week",
       subtitle = "Subterfuge data from 19 Feb 2017",
       x = "Number of Rated Games Entered per Week",
       y = "Number of Players",
       caption = "note: 52 observations greater than 1.2 removed") +
  scale_x_continuous(limits = c(0, 1.19)) +
  theme_light()

ggsave(paste(Sys.Date(),"1C Sub Games Weekly Rate per Player.png"))

## Distribution of Ratings

ggplot(sub, aes(x = rating)) + 
  geom_histogram(binwidth = 10) + 
  labs(title = "Distribution of All Player's Ratings",
       subtitle = "Subterfuge data from 19 Feb 2017",
       x = "Rating",
       y = "Number of Players") +
  scale_x_continuous(breaks = seq(800, 2000, 200), limits = c(800, 1800)) +
  annotate("segment", x = 1200, xend = 1200, size = 1,
           y = 0, yend = 475, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"1D Sub Ratings Distribution.png"))

dim(filter(sub, rating < 1200))[1]
## Total of 4405 players below 1200

dim(filter(sub, rating >= 1200))[1]
## Total of 3179 players above or equal to 1200

dim(filter(sub, rating == 1200))[1]
## Total of 12 players equal to 1200

########## SECTION 2:  Rating by Games Entered

ggplot(sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  labs(title = "Individual Rating by Total Rated Games Entered",
       subtitle = "Subterfuge data from: 2017-02-19",
       x = "Total Rated Games Entered", y = "Rating") +
  annotate("segment", x = 0, xend = max(sub$rated_games), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"2A Sub Ratings by Games.png"))

## Baer on Rating by Games entered graph

ggplot(sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(breaks = seq(0, 2000, 100)) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  labs(title = "Baer's Rating by Total Rated Games Entered",
       subtitle = "Subterfuge data from: 2017-02-19",
       x = "Total Rated Games Played", y = "Rating") + 
  annotate("segment", x = sub$rated_games[sub$player == "Baer"], 
           xend = 77.5, 
           y = sub$rating[sub$player == "Baer"], 
           yend = 1700,
           color = "darkblue", size = 1) +
  annotate("text", x = 88, 
           y = 1700, 
           label = "Baer", color = "darkblue", size = 7) +
  annotate("segment", x = 0, xend = max(sub$rated_games), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"2B Sub Baer Rating by Games.png"))


####### SECTION 3: Rating by Weekly Game Rate

## All observations

## Filtered to > 21 days since joining

days <- 21

ggplot(filter(sub, total_days > days), aes(x = game_rate, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  xlim(0,2) +
  labs(title = "Individual Rating by Weekly Games Entered",
       subtitle = "Subterfuge data from: 2017-02-19",
       x = "Rate of Games Entered per Week", y = "Rating",
       caption = paste("note: minimum of", days, 
                       "days since joining.\n3 observations greater than 2.5 games per week removed.")) +
  annotate("segment", x = 0, xend = 2, 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"3A Sub Ratings by Game Rate 21 Days.png"))

## Baer on game rate graph

#### REVIEW THIS:

ggplot(filter(sub, total_days > days), aes(x = game_rate, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  xlim(0,2) +
  labs(title = "Baer's Rating by Total Rated Games Entered",
       subtitle = "Subterfuge data from: 2017-02-19",
       x = "Rate of Games Entered per Week", y = "Rating",
       caption = paste("note: minimum of", days, 
                       "days since joining.\n3 observations greater than 2.5 games per week removed.")) + 
  annotate("segment", x = sub$game_rate[sub$player == "Baer"], 
           xend = 1.6,
           y = sub$rating[sub$player == "Baer"], 
           yend = 1700,
           color = "darkblue", size = 1) +
  annotate("text", x = 1.8, 
           y = 1700, 
           label = "Baer", color = "darkblue", size = 7) +
  annotate("segment", x = 0, xend = 2, 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"3B Sub Baer Rating by Game Rate.png"))


