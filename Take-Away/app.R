library(shiny)
library(tidyverse)
library(shinyjs)
library(httr)
library(dplyr)
library(jsonlite)

#library(RCurl)
#getURL
#503072804
#http://www.omdbapi.com/?apikey=[yourkey]&

url<- "https://swapi.co/api/people/"
url2<- "https://swapi.co/api/planets/"

planets<- GET(url2)
starwars<- GET(url)

details<- httr::content(starwars,as='parsed')$results
details2<- httr::content(planets,as='parsed')$results


heights <- numeric(length(details))
mass <- numeric(length(details))
name <- numeric(length(details))
gender <- numeric(length(details))
eye <- numeric(length(details))
skin <- numeric(length(details))
hair <- numeric(length(details))

for (i in 1:length(details)) {
    heights[i] <- as.numeric(details[[i]]$height) 
    mass[i] <- as.numeric(details[[i]]$mass)
    name[i] <- details[[i]]$name
    gender[i] <- details[[i]]$gender
    eye[i] <- details[[i]]$eye_color
    skin[i] <- details[[i]]$skin_color
    hair[i] <- details[[i]]$hair_color
}

sw<- as.data.frame(cbind(name,heights,mass, gender, eye, skin, hair))

ui <- fluidPage(
    titlePanel("Old Faithful Geyser Data"),
###
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
