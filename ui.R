
library(shiny)

shinyUI(fluidPage(
  titlePanel("Spotify Music Vibe Analysis"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("genres", "Select Genres:", 
                         choices = c("blues rock", "classic soul", "celtic punk", "pop punk", "atl hip hop", "baroque pop")),
      sliderInput("popularity", "Average Popularity Range:", min = 70, max = 90, value = c(70, 90)),
      sliderInput("decade", "Decade Range:", min = 1950, max = 2020, value = c(1950, 2020))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Genres over decades", plotOutput("plot1")),
        tabPanel("Danceability over years", plotOutput("plot2")),
        tabPanel("Lasso Regression Analysis", plotOutput("lassoPlot"))
      )
    )
  )
))
