## Subterfuge
## Wordpress Post

#######################################

## Data Wrangling

## Subterfuge Top Leaders
## Scrub Website to create dataset

## Used this method:
## http://www.r-bloggers.com/rvest-easy-web-scraping-with-r/
## http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html
## https://github.com/hadley/rvest

library(rvest)
library(dplyr)
library(ggplot2)
library(ggthemes)

setwd("~/R/Subterfuge")

url <- read_html('http://subterfuge-game.com/leaderboards.html')

sub <- url %>%
  html_nodes(xpath = '/html/body/table') %>%
  html_table() %>%
  as.data.frame()

rm(url)

## Data Cleaning

### Select and Rename all variables
sub <- rename(sub, rank = Rank,
              player = Player,
              rating = Rating,
              gold = Var.4,
              silver = Var.5,
              bronze = Var.6,
              rated_games = Ratedgames,
              total_games = Totalgames,
              finished = Finished,
              eliminated = Eliminated,
              resigned = Resigned,
              joined = Joined)

### Fix Dates. Create Game Rate
sub$joined <- as.Date(sub$joined, "%d %b %Y")

sub <- mutate(sub,
              total_days = (Sys.Date() - sub$joined))

sub$total_days <- as.integer(sub$total_days)

sub <- mutate(sub,
              game_rate = (rated_games / total_days)*7)

### Fix Medals variables. Create Total and Adjusted
sub$gold <- as.integer(sub$gold)
sub$gold[is.na(sub$gold)] <- 0

sub$silver <- as.integer(sub$silver)
sub$silver[is.na(sub$silver)] <- 0

sub$bronze <- as.integer(sub$bronze)
sub$bronze[is.na(sub$bronze)] <- 0

sub <- mutate(sub, 
              total_medals = gold + silver + bronze,
              adj_medals = 3*gold + 2*silver + bronze)

### Fix Eliminated, Resigned and Finished vars
sub$eliminated <- sub("%", "", sub$eliminated)
sub$eliminated <- as.integer(sub$eliminated)
sub$eliminated[is.na(sub$eliminated)] <- 0

sub$resigned <- sub("%", "", sub$resigned)
sub$resigned <- as.integer(sub$resigned)
sub$resigned[is.na(sub$resigned)] <- 0

sub$finished <- sub("%", "", sub$finished)
sub$finished <- as.integer(sub$finished)
sub$finished[is.na(sub$finished)] <- 0


## Save dataset. Change date in filename when applicable

saveRDS(sub, paste("sub", Sys.Date()))

## Check out Data

glimpse(sub)

summary(sub$total_games)
summary(sub$total_days)
summary(sub$total_games)
summary(sub$rating)
summary(sub$gold)
summary(sub$total_medals)
summary(sub$game_rate)

## Graph of games played per Player

ggplot(sub, aes(x = total_games)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = paste("Subterfuge", Sys.Date(),
                     "\n Distribution of Rated Games Played per Player"),
       x = "Total Rated Games Played",
       y = "Number of Players") +
  scale_x_continuous(breaks = seq(0, 80, 5), limits = c(0,75)) +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Games per Player.png"))

## Graph of Games Entered per Week

#### DO NOT USE
#### JANKY SPIKES IN GRAPH

ggplot(sub, aes(x = game_rate)) + 
  geom_histogram(binwidth = .007) + 
  labs(title = "Subterfuge \n Distribution of Games Entered per Week",
       x = "Number of Rated Games Entered per Week",
       y = "Number of Players") +
  scale_x_continuous(limits = c(0, 1)) +
  theme_light()

## Graph of ratings

ggplot(sub, aes(x = rating)) + 
  geom_histogram(binwidth = 20) + 
  labs(title = paste("Subterfuge", Sys.Date(), 
                     "\n Distribution of Ratings"),
       x = "Rating",
       y = "Number of Players") +
  scale_x_continuous(breaks = seq(800, 2000, 200)) +
  annotate("segment", x = 1200, xend = 1200, size = 1.5,
           y = 0, yend = 800, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Ratings Distribution.png"))

## Total > and < 1200 Rating

dim(filter(sub, rating < 1200))[1]
dim(filter(sub, rating >= 1200))[1]

## Rating by Rated Games graph

ggplot(sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 2000, 100)) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  labs(title = paste("Subterfuge", Sys.Date(), 
                     "\n Individual Rating by Total Rated Games Played"),
       x = "Total Rated Games Played", y = "Rating") +
  annotate("segment", x = 0, xend = max(sub$rated_games), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Rating by Rated Games.png"))

## Rating by Number of Games Entered per Week

ggplot(sub, aes(x = game_rate, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  labs(title = paste("Subterfuge", Sys.Date(), 
                     "\n Individual Rating by Weekly Games Entered"),
       x = "Rate of Games Entered per Week", y = "Rating") +
  annotate("segment", x = 0, xend = max(sub$game_rate), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Rating by Game Rate.png"))

## Baer on Rating by Games Graph

ggplot(sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 2000, 100)) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  labs(title = paste("Subterfuge", Sys.Date(), 
                     "\n Baer's Rating by Total Rated Games Played"),
       x = "Total Rated Games Played", y = "Rating") + 
  annotate("segment", x = sub$rated_games[sub$player == "Baer"], 
           xend = 77.5, 
           y = sub$rating[sub$player == "Baer"], 
           yend = 1700,
           color = "red", size = 1) +
  annotate("text", x = 90, 
           y = 1700, 
           label = "Baer", color = "red", size = 7) +
  annotate("segment", x = 0, xend = max(sub$rated_games), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Baer Rating by Rated Games.png"))

## Baer on Rating by Weekly Games Rate Graph

ggplot(sub, aes(x = game_rate, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  labs(title = paste("Subterfuge", Sys.Date(), 
                     "\n Baer's Rating by Weekly Games Entered"),
       x = "Games Entered per Week", y = "Rating") + 
  annotate("segment", x = sub$game_rate[sub$player == "Baer"], 
           xend = 2,
           y = sub$rating[sub$player == "Baer"], 
           yend = 1700,
           color = "red", size = 1) +
  annotate("text", x = 2.3, 
           y = 1700, 
           label = "Baer", color = "red", size = 7) +
  annotate("segment", x = 0, xend = max(sub$game_rate), 
           y = 1200, yend = 1200, color = "purple") +
  theme_light()

ggsave(paste(Sys.Date(),"Sub Baer Rating by Game Rate.png"))


## Baer stats

sub[sub$rank[sub$player == "Baer"],]
