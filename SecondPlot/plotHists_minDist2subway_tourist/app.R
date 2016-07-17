#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

vzMinDist <- read_csv("vzMinDist.csv")
vzMinDist2Subway <- read_csv("vzMinDist2subway.csv")
randMinDist <- read_csv("randMinDist.csv")
randMinDist2Subway <- read_csv("randMinDist2subway.csv")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel("Statistical Evidence that Persons are Injured CLose to Tourist Attractions or Subway Stops in Manhattan"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", p("Place of interest"),
                  choices = c("Subway", "Tourist Attraction")),
      "Mean closest distance from a place of interest to injured person in Manhattan in meters:",
      verbatimTextOutput("oid1"),
      br(),
      "Mean closest distance from a place of interest to point from uniform random location in Manhattan in meters:",
      verbatimTextOutput("oid2"),
      br(),
      "P-value between random distribution and distribution of persons injured:",
      verbatimTextOutput("oid3"),
      br(),
      "P-value is less than 0.05 so the difference is statistically significant"
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$oid1<- renderPrint({
    if(input$countryInput == 'Subway'){
      mean(vzMinDist2Subway$dists)
    } else {
      mean(vzMinDist$dists)
    }
    })
  output$oid2<- renderPrint({
    if(input$countryInput == 'Subway'){
      mean(randMinDist2Subway$dists)
    } else {
      mean(randMinDist$dists)
    }
  })
  
  output$oid3<- renderPrint({
    if(input$countryInput == 'Subway'){
      t.test(vzMinDist2Subway$dists, randMinDist2Subway$dists)$p.value
    } else {
      t.test(vzMinDist$dists, randMinDist$dists)$p.value
    }
  })
  output$coolplot <- renderPlot({
    if(input$countryInput == 'Subway'){  
      
      meanVZ <- mean(log(vzMinDist2Subway$dists), na.rm = T)
      meanRand <- mean(log(randMinDist2Subway$dists), na.rm = T)
      ggplot() +
        geom_histogram(data = log(randMinDist2Subway),
                       aes(x = dists, y=..count../sum(..count..), fill= 'green', color = 'green'),
                       fill = "green", alpha = 0.2, bins = 50) +
        geom_histogram(data = log(vzMinDist2Subway),
                       aes(x = dists, y=..count../sum(..count..), fill= 'red', color = 'red'),
                       fill = "red",  alpha = 0.2, bins = 50) +
        geom_vline(xintercept = (meanRand), color = 'green') +
        geom_vline(xintercept = (meanVZ), color = 'red') +
        labs(list(title = 'Histogram Showing Distribution of Log Distance to Closest Subway'
                                    , y = 'Normalized Count', x = 'Log distance(m)')) +
        xlim(c(0, 8)) +
        # ylim(c(0, 0.09)) +
        scale_colour_manual(name="group", values=c("red" = "red", "green"="green"),
                            labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution")) +
        scale_fill_manual(name="group", values=c("red" = "red", "green"="green"),
                          labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution"))
    } else {
      meanVZ <- mean(log(vzMinDist$dists), na.rm = T)
      meanRand <- mean(log(randMinDist$dists), na.rm = T)
      ggplot() +
        geom_histogram(data = log(randMinDist),
                       aes(x = dists, y=..count../sum(..count..), fill= 'green', color = 'green'),
                       fill = "green", alpha = 0.2, bins = 50) +
        geom_histogram(data = log(vzMinDist),
                       aes(x = dists, y=..count../sum(..count..), fill= 'red', color = 'red'),
                       fill = "red",  alpha = 0.2, bins = 50) +
        geom_vline(xintercept = meanRand, color = 'green') +
        geom_vline(xintercept = meanVZ, color = 'red') +
        labs(list(title = 'Histogram Showing Distribution of Log Distance to Closest Tourist Attraction'
                  , y = 'Normalized Count', x = 'Log distance(m)')) +
        xlim(c(2.5, 9.5)) +
        # ylim(c(0, 0.09)) +
        scale_colour_manual(name="group", values=c("red" = "red", "green"="green"),
                            labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution")) +
        scale_fill_manual(name="group", values=c("red" = "red", "green"="green"),
                          labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution"))
    }
  
  })
  output$results <- renderTable({
    
    
  })
  
}

shinyApp(ui = ui, server = server)