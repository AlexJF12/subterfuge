##### SUBTERFUGE ######
## Residuals
## 1 April 2017
##
## Inspired by:
## https://drsimonj.svbtle.com/visualising-residuals
#######################

## DATA MUNGE ##
################

library(tidyverse)
library(data.table)
setwd("~/R/Subterfuge/Residuals")

sub_full <- readRDS("~/R/Subterfuge/Residuals/2017-04-01 sub.rds")
sub <- filter(sub_full, rating > 600)
rm(sub_full)
str(sub)

sub <- sub %>%
  select(rank, player, rating, rated_games, total_days)

## linear model fit

fit <- lm(rating ~ rated_games, sub)
summary(fit)

# add predicted and residual values

sub$predicted <- predict(fit)
sub$residuals <- residuals(fit)

# residual is also actual - predicted

sub$test.resid <- sub$rating - sub$predicted
all.equal(sub$test.resid, sub$residuals) # = TRUE
sub <- select(sub, -test.resid)

# get only highest and lowest residuals from each level of rated_games

sub_resid <- sub %>%
  group_by(rated_games) %>%
  filter(residuals == max(residuals) | residuals == min(residuals))

best <- filter(sub, residuals == max(residuals))
worst <- filter(sub, residuals == min(residuals))

## VIZ ##
#########

# base r viz
# not sure what it all means...

# lm plots
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

# ggplot

# linear model

ggplot(sub, aes(rated_games, rating)) +
  geom_point(alpha = .08) +
  geom_point(data = sub_resid, aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_smooth(method = "lm", data = sub, alpha = .75,
              color = "lightblue", se = FALSE, alpha = .75) +
  geom_smooth(method = "lm", data = filter(sub_resid, residuals >= 0),
              se = FALSE, color = "darkred") +
  geom_smooth(method = "lm", data = filter(sub_resid, residuals <= 0),
              se = FALSE, color = "darkblue", alpha = .75) +
  guides(color = FALSE) +
  geom_segment(aes(xend = rated_games, yend = predicted), alpha = .3) +
  labs(title = "Who's the Best?",
       subtitle = "Using Residuals with Subterfuge's Leaderboard",
       x = "Rated Games Played",
       y = "Rating") +
  annotate("text", x = 96, y = 1700, label = best$player[1]) +
  annotate("segment", x = best$rated_games[1], xend = 92,
           y = best$rating[1], yend = 1700) +
  annotate("text", x = 106, y = 1080, label = "Baer") +
  annotate("segment", x = 35, xend = 102,
           y = 1264, yend = 1075) +
  theme_dark()

library(Cairo)
ggsave("Subterfuge Residuals graph.png", height = 5, width = 9,
       type = "cairo-png")

