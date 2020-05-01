library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library (ggplot2)
load(url("https://github.com/peo042/ShinyApp/blob/master/Data.RData?raw=true"))

ui <- fluidPage(
  titlePanel("Regional Video Game Sales by Console and Genre"),
  
    sidebarPanel(
      hr(),
      checkboxGroupInput("Console", 
                  label = "Console:",
                  choices = c("Playstation 4", "Xbox One"), 
                  selected = NULL),
      hr(),
      selectInput("colorp",
                  label = "Choose color one:",
                  choices = c("Red", "Blue", "Black", "Green"),
                  selected = "Blue"),
      hr(),
      selectInput(inputId = "colorx",
                  label = "Choose color two:",
                  choices = c("Red", "Blue", "Black", "Green"),
                  selected = "Green"),
      hr(),
      selectInput(inputId = "Genre", 
                  label = "Genre:",
                  choices = c("Action", "Action-Adventure", "Adventure", "Fighting", 
                              "Misc", "MMO", "Music", "Party", "Platform", "Puzzle", 
                              "Racing", "Role-Playing", "Simulation", "Shooter",
                              "Sports", "Strategy", "Visual Novel"), 
                  selected = "Action")
    ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("World", plotlyOutput(outputId = "World")),
          tabPanel("North America", plotlyOutput(outputId = "US")),
          tabPanel("Europe", plotlyOutput(outputId = "EU")),
          tabPanel("Japan", plotlyOutput(outputId = "JP"))
    )
  )
)

server <- function(input, output) {
  
  
  dat1 <- reactive({
      ds1 <- all_games_t[(all_games_t$Console %in% input$Console) & (all_games_t$Genre %in% input$Genre), ]
     return(ds1)})
  
  
  output$World <- renderPlotly({
    g2 <- filter(dat1(), World > 0)
    ggplot(g2, aes(x=Game, y=World, fill=Console)) + geom_col() + scale_fill_manual(values=c(input$colorp, input$colorx)) +
         theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank()) + scale_y_continuous(name = "Sales in Millions") +
          scale_x_discrete(name = "Alphabetized Game List") + labs (fill ="")
    })
  
  output$US <- renderPlotly({
    g3 <- filter(dat1(), US > 0)
    ggplot(g3, aes(x=Game, y=US, fill=Console)) + geom_col() + scale_fill_manual(values=c(input$colorp, input$colorx)) +
      theme(axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + scale_y_continuous(name = "Sales in Millions") +
      scale_x_discrete(name = "Alphabetized Game List") + labs (fill ="")
  }) 
  
  output$EU <- renderPlotly({
    g4 <- filter(dat1(), EU > 0)
    ggplot(g4, aes(x=Game, y=EU, fill=Console)) + geom_col() + scale_fill_manual(values=c(input$colorp, input$colorx)) +
      theme(axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + scale_y_continuous(name = "Sales in Millions") +
      scale_x_discrete(name = "Alphabetized Game List") + labs (fill ="")
  })
  
  output$JP<- renderPlotly({
    g5 <- filter(dat1(), JP > 0)
    ggplot(g5, aes(x=Game, y=JP, fill=Console)) + geom_col() + scale_fill_manual(values=c(input$colorp, input$colorx)) +
      theme(axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + scale_y_continuous(name = "Sales in Millions") +
      scale_x_discrete(name = "Alphabetized Game List") + labs (fill ="")
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)