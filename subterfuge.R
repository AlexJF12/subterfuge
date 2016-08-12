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

url <- read_html('http://subterfuge-game.com/leaderboards.html')

sub <- url %>%
  html_nodes(xpath = '/html/body/table') %>%
  html_table() %>%
  as.data.frame()

sub_date_updated <- url %>%
  html_nodes(xpath = '/html/body/p[2]') %>%
  strptime(sub_date_updated, 
           format = "<p>Leaderboard updated on %a, %d %b %y %H:%M:%S -0400.</p")

## Data Cleaning / Managing

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

sub$joined <- as.Date(sub$joined, "%d %b %Y")

sub$gold <- as.integer(sub$gold)
sub$gold[is.na(sub$gold)] <- 0

sub$silver <- as.integer(sub$silver)
sub$silver[is.na(sub$silver)] <- 0

sub$bronze <- as.integer(sub$bronze)
sub$bronze[is.na(sub$bronze)] <- 0

sub <- mutate(sub, 
              total_medals = gold + silver + bronze,
              adj_medals = 3*gold + 2*silver + bronze)

sub$eliminated <- sub("%", "", sub$eliminated)
sub$eliminated <- as.integer(sub$eliminated)
sub$eliminated[is.na(sub$eliminated)] <- 0

sub$resigned <- sub("%", "", sub$resigned)
sub$resigned <- as.integer(sub$resigned)
sub$resigned[is.na(sub$resigned)] <- 0

sub$finished <- sub("%", "", sub$finished)
sub$finished <- as.integer(sub$finished)
sub$finished[is.na(sub$finished)] <- 0

sub <- mutate(sub,
              total_days = (Sys.Date() - sub$joined))
sub$total_days <- as.integer(sub$total_days)

sub_open <- filter(sub, joined > "2015-10-11")

glimpse(sub)


## Top Ten Table

top10 <- sub %>% 
           head(10) %>%
           select(player, rating, total_medals, total_games, total_days)
top10

sub$player[1]
max(sub$rank)
count(sub)
(max(sub$total_games))

## Analysis

mean(sub$total_games)
median(sub$total_games)
mean(sub$rating)
table(sub$gold)
mean(sub$gold)
mean(sub$total_medals)
mean(sub$adj_medals)
mean(sub$total_days)

median(sub$total_days)
names(sort(-table(sub$total_days)))[1]
names(sort(-table(sub$joined)))[1]

## Data Subsets

ten_percent <- round(max(sub$rank)*.1)

  ## 10th Percentile of players by rating



ten_rating_sub <- subset(sub, sub$rank < round(max(sub$rank)*.1))

mean(ten_rating_sub$rating)
mean(ten_rating_sub$total_games)
mean(ten_rating_sub$rated_games)
mean(ten_rating_sub$total_medals)

min(ten_rating_sub$total_days)
min(ten_rating_sub$total_games)

  ## 10th Percentile by Games Played

ten_games_sub <- subset(sub, 
                        sub$total_games < round(max(sub$total_games) *.1))

round(max(sub$total_games) *.1)


## Data Viz

 ## Date Joined Distribution

ggplot(sub, aes(x = joined)) + geom_histogram()

 ## Histogram of Games played

ggplot(sub, aes(x = total_games)) + geom_histogram(binwidth = 1)

ggplot(sub, aes(x = total_games)) + 
  geom_histogram(binwidth = 1) + 
  xlim(0,75)

 ## Ratings Boxplot by factors

ggplot(sub, aes(x = factor(rated_games), y = rating)) + 
  geom_boxplot() +
  geom_jitter(alpha = .05) +
  labs(title = "Subterfuge \n Rating by Total Rated Games Played",
       x = "Total Rated Games Played", y = "Rating") +
  theme_light()


ggplot(sub, aes(x = factor(total_medals), y = rating)) + 
  geom_boxplot() +
  geom_jitter(alpha = .05) +
  labs(title = "Subterfuge Rating by Total Medals Earned",
       x = "Total Medals Earned (Gold + Silver + Bronze)", 
       y = "Rating") +
  theme_light()

ggplot(sub, aes(x = factor(adj_medals), y = rating)) + geom_boxplot()
ggplot(sub, aes(x = factor(gold), y = rating)) + geom_boxplot()
ggplot(sub, aes(x = factor(silver), y = rating)) + geom_boxplot()
ggplot(sub, aes(x = factor(bronze), y = rating)) + geom_boxplot()

ggplot(sub, aes(x = total_games)) + geom_histogram(binwidth = 1) + 
  labs(title = "Distribution of Games Played per Player",
       x = "Total Games Played",
       y = "Number of Players") +
  xlim(0,75) +
  theme_light()

ggplot(sub, aes(x = rating)) + geom_density()


  ## Total Days
ggplot(sub, aes(x = total_days)) + geom_histogram(binwidth = 20) 

  ## Scatterplot of Rating by Total Games Played

plot1 <- ggplot(sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.15) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 1800, 100)) + 
  labs(title = "Subterfuge \n Individual Rating by Total Rated Games Played",
       x = "Total Rated Games Played", y = "Rating") + 
  theme_light()
plot1

ggplot(ten_rating_sub, aes(x = rated_games, y = rating)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 1800, 100)) + 
  labs(title = "Subterfuge 10th Percentile \n Individual Rating by Total Rated Games Played",
       x = "Total Rated Games Played", y = "Rating") + 
  theme_light()

  ## Rating by Medals earned

ggplot(sub, aes(x = total_medals, y = rating)) + 
  geom_jitter(alpha = .25) +
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 1800, 100)) + 
  labs(title = "Subterfuge \n Individual Rating by Total Medals Earned",
       x = "Total Medals Earned", y = "Rating") + 
  theme_light()



ggplot(sub, aes(x = adj_medals, y = rating)) + geom_point() + geom_jitter()
ggplot(sub, aes(x = gold, y = rating)) + geom_point() + geom_jitter()
ggplot(sub, aes(x = silver, y = rating)) + geom_point() + geom_jitter()
ggplot(sub, aes(x = bronze, y = rating)) + geom_point() + geom_jitter()

  ## Rating and Medals by Total Days Played

ggplot(sub, aes(x = total_days, y = rating)) + geom_point() + geom_jitter()
ggplot(sub, aes(x = total_days, y = total_medals)) + geom_point() + geom_jitter()


## Plotly

install.packages("viridis") # dependency
install.packages("devtools")
devtools::install_github("ropensci/plotly")

library(plotly)

plotly:::verify("AlexJF12")
plotly:::verify("46x27besdd")

(gg <- ggplotly(plot1))
