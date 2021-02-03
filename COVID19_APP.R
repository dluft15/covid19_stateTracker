# US Covid 19 % of State Population Positive - R Shiny App
# Created by Daniel Luft, February 2021
# Data from https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv - Nytimes covid data on github
# dluft@ithaca.edu

# load packages
library(shiny)
library(tidyverse)

# load state_names.csv from folder (read.csv may need to be changed to get file from your directory)
allStates <- read.csv("~/state_names.csv", sep="", stringsAsFactors = FALSE)
allStates <- allStates$x # make the states a list

statePop <- read.csv("C:/Users/dluft/Desktop/statePop.csv") # load 2019* state population data
            # state pop data from census, last available year 2019


ui <- fluidPage(
  
  sidebarPanel(
    
    titlePanel('States to Compare:'), # add title panel
    selectInput('st1', 'State 1', allStates), # first state input
    selectInput('st2', 'State 2', allStates), # second state input
    actionButton('gen', 'Generate Plot') # action button to generate the plot
    
  ),
  
  mainPanel(
    
    plotOutput('plot1') # plot output as 'plot1'
    
  )
  
)

server <- function(input, output) {
  
  
d <- eventReactive(input$gen, {
    
      datURL <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv' # link to csv
      dat <- read.csv(url(datURL), stringsAsFactors = FALSE) # load the data
      dat <- dat %>% filter(state == input$st1 | state == input$st2) # filter for selected states
      dat <- dat %>% left_join(statePop, by = 'state') # join population data
      dat <- dat %>% mutate(asP = cases/pop) # create new column for cases as percent of pop
      dat$date<- as.Date(dat$date) # classify date column as date
     
      return(dat)
      
    })
  
  
  output$plot1 <- renderPlot({
    
    # get the reactive data d() and save as plotD
    plotD <- d()  
    
    # plot the time series using ggplot
    ggplot(plotD, aes(date, asP, group = state, color = state)) + 
      geom_line(lwd = 2) + 
      theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12), plot.title = element_text(hjust = 0.5, size = 13, face = 'bold.italic'), 
            axis.title.x = element_text(size = 15, face = 'italic'), axis.title.y = element_text(size = 15, face = 'italic')) + 
      xlab("Month") + ylab("% Positive") + 
      scale_x_date(date_breaks="1 month", date_labels = "%b") + # group x axis tick marks in months for visual effect
      scale_y_continuous(labels = scales::percent) + 
      ggtitle(paste('Comparison of Positive Cases as % of Population for', input$st1, 'and' ,input$st2))
    
  })
  
  
}

shinyApp(ui = ui, server = server)


