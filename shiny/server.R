#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

sub <- readRDS('sub 2016-10-10')

# url <- read_html('http://subterfuge-game.com/leaderboards.html')

#sub <- url %>%
#  html_nodes(xpath = '/html/body/table') %>%
#  html_table() %>%
#  as.data.frame()

#sub_date_updated <- url %>%
#  html_nodes(xpath = '/html/body/p[2]') %>%
#  as.character() %>%
#  strptime(format = "<p>Leaderboard updated on %a, %d %b %y %H:%M:%S -0500.</p")

## Data Cleaning / Managing

#sub <- select(sub,
#              player = Player,
#              rating = Rating,
#              rated_games = Ratedgames)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$rating_plot <- renderPlot({
    ggplot(sub, aes(x = rated_games, y = rating)) + 
      geom_jitter(alpha = 0.25) + 
      geom_smooth() +
      scale_y_continuous(breaks = seq(0, 2000, 100)) + 
      scale_x_continuous(breaks = seq(0, 200, 5)) +
      labs(title = "Subterfuge \n Individual Rating by Total Rated Games Played",
           x = "Total Rated Games Played", y = "Rating") + 
      annotate("text", x = 85, y = 1600, 
               label = input$playerInput, color = "red", size = 7) +
      geom_segment(aes(x = 75, y = 1600,
                       xend = sub$rated_games[sub$player == input$playerInput],
                       yend = sub$rating[sub$player == input$playerInput]),
                   color = "red", arrow = arrow()) + 
      theme_light()
      
  })
  
})
